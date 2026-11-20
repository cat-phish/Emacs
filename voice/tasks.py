#!/usr/bin/env python3
"""Voice capture and bill tracking for Tasks.org (Home Assistant shell_command).

Usage:  tasks.py <op> "<text>"

  capture  everything said after "capture"
           "call the landlord due friday"           -> Todos: TODO + DEADLINE
           "inbox call the landlord"                 -> Inbox: TODO (or "inbox chore ...")
           "chore take out recycling"                -> Todos: CHORE
           "assignment for 317 lab five due sunday"  -> School > CPT_S 317: ASSIGNMENT
           "barista assistant backlog dark mode"     -> Projects > Barista-Assistant > Backlog: - [ ]
           "barista assistant todo fix bean dialog"  -> Projects > Barista-Assistant: TODO
           "system home lab set up headscale"        -> System > Home-Lab: - [ ]
  inbox    "add X to my inbox" (same as "capture inbox X")
  answer   a reply to a follow-up question: "due friday", "no deadline", "for 317"
  paid     "I paid the electric bill": BILL -> DONE (repeating bills move to the next date)
  bills    read unpaid bills
  read     read a list: "todo", "inbox", "system general", "barista assistant backlog"
  undo     undo the last change this script made, if the file hasn't changed since
  cancel   "scratch that": drop a waiting question and undo a capture from the last
           few minutes (also triggered by answering a question with "scratch that")

New headings and checkboxes always go at the BOTTOM of their section. A TODO-type
heading captured without a date is written straight away, then the reply asks for
a deadline; "due <date>" or "no deadline" answers it.

Prints JSON {"say": "...", "ask": true|false} and always exits 0. "ask" means the
reply is a question waiting for an answer.
"""
import datetime as dt
import difflib
import hashlib
import json
import os
import re
import sys
import tempfile

TASKS_FILE = os.environ.get("TASKS_FILE", "/drives/local/org/main/Tasks.org")
STATE_DIR = os.environ.get("TASKS_STATE_DIR", os.path.dirname(os.path.abspath(__file__)))
STATE_FILE = os.path.join(STATE_DIR, ".tasks_state.json")
UNDO_FILE = os.path.join(STATE_DIR, ".tasks_undo.org")
FOLLOW_UP_MINUTES = 10  # how long a question waits for "due friday" / "for 317"

ACTIVE = ["TODO", "ASSIGNMENT", "BILL", "CHORE", "MEETING", "NEXT", "PLANNING",
          "REVIEW", "HOLD", "READY", "ACTIVE"]
DONE = ["DONE", "CANCELED"]
EXCLUDE_MARK = "#"   # a heading starting with # (and everything under it) is never read or written
SPOKEN_KEYWORDS = {"todo": "TODO", "to do": "TODO", "chore": "CHORE", "bill": "BILL",
                   "assignment": "ASSIGNMENT", "meeting": "MEETING", "next": "NEXT",
                   "review": "REVIEW"}
WORD_ALIASES = {"neovim": "nvim", "miscellaneous": "misc", "homelab": "home lab",
                "todos": "todo", "tasks": "todo", "task": "todo"}
CANCEL = {"cancel", "cancel that", "scratch", "scratch that", "never mind", "nevermind",
          "forget it", "forget that", "stop"}
NO_DEADLINE = {"no", "none", "nope", "no deadline", "no due date", "no date", "skip",
               "never", "nothing"}

DAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]
MONTH_NUM = {m.lower(): i + 1 for i, m in enumerate(MONTHS)}
MONTH_NUM.update({m[:3].lower(): i + 1 for i, m in enumerate(MONTHS)})
MONTH_NUM["sept"] = 9
NUM_WORDS = {w: i for i, w in enumerate(
    "zero one two three four five six seven eight nine ten eleven twelve".split())}
NUM_WORDS.update({"a": 1, "an": 1})

# spoken numbers -> digits ("three twenty two" -> 322, "twenty second" -> 22th)
ONES = {w: i for i, w in enumerate("zero one two three four five six seven eight nine".split())}
ONES["oh"] = 0
TEENS = {w: i + 10 for i, w in enumerate(
    "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen".split())}
TENS = {w: (i + 2) * 10 for i, w in enumerate(
    "twenty thirty forty fifty sixty seventy eighty ninety".split())}
ORDS = {w: i + 1 for i, w in enumerate(
    "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth "
    "thirteenth fourteenth fifteenth sixteenth seventeenth eighteenth nineteenth".split())}
ORDS.update({"twentieth": 20, "thirtieth": 30})
NUMBER_WORDS = set(ONES) | set(TEENS) | set(TENS) | set(ORDS) | {"hundred"}

KW_ALT = "|".join(ACTIVE + DONE)
HEADING_RE = re.compile(rf"^(\*+)\s+(?:({KW_ALT})(?:\s+|$))?(?:\[#\w\]\s+)?(.*?)\s*$")
COOKIE_RE = re.compile(r"\s*\[\d*(?:/\d*|%)\]")
TAGS_RE = re.compile(r"\s+:[\w@#%:]+:\s*$")
LINK_RE = re.compile(r"\[\[([^\]]*)\](?:\[([^\]]*)\])?\]")
URL_RE = re.compile(r"https?://\S+")
ITEM_RE = re.compile(r"^(\s*)[-+]\s+\[([ xX-])\]\s?(.*?)\s*$")
PLANNING_RE = re.compile(r"^\s*(?:DEADLINE|SCHEDULED|CLOSED):")
DRAWER_RE = re.compile(r"^\s*:(?!END:)[A-Za-z_-]+:\s*$")
END_RE = re.compile(r"^\s*:END:")
STAMP_RE = re.compile(r"<(\d{4})-(\d{2})-(\d{2})(?: [A-Za-z]{2,3})?([^>]*)>")
REPEAT_RE = re.compile(r"(\.\+|\+\+|\+)(\d+)([hdwmy])")


class Reply(Exception):
    def __init__(self, say, ask=False):
        super().__init__(say)
        self.say, self.ask = say, ask


# ---------- text ----------

def norm(s):
    s = LINK_RE.sub(" ", s)
    s = URL_RE.sub(" ", s.lower()).replace("&", " and ")
    s = re.sub(r"[.'’]", "", s)
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return " ".join(WORD_ALIASES.get(t, t) for t in s.split())


def say_text(s):
    """Item/title text for speech: link descriptions kept, bare URLs dropped."""
    s = LINK_RE.sub(lambda m: m.group(2) or "", s)
    s = " ".join(URL_RE.sub("", s).split()).strip(" -")
    return s or "a link"


def clean_title(s):
    return " ".join(s.split()).strip(" .!?,;:")


def join_words(words, conj="and"):
    words = list(words)
    if len(words) <= 2:
        return f" {conj} ".join(words)
    return ", ".join(words[:-1]) + f", {conj} " + words[-1]


def cap(s):
    return s[:1].upper() + s[1:]


# ---------- spoken numbers ----------

def _two_digit(ws):
    if len(ws) == 1:
        for table in (ONES, TEENS, TENS, ORDS):
            if ws[0] in table:
                return table[ws[0]]
    if len(ws) == 2 and ws[0] in TENS and (ws[1] in ONES or ws[1] in ORDS) \
            and (ONES.get(ws[1]) or ORDS.get(ws[1], 99)) < 10:
        return TENS[ws[0]] + (ONES.get(ws[1]) or ORDS[ws[1]])
    return None


def _run_value(ws):
    ws = [w for w in ws if w != "and"]
    suffix = "th" if ws[-1] in ORDS else ""
    if "hundred" in ws:
        k = ws.index("hundred")
        h = _two_digit(ws[:k]) if k else 1
        rest = _two_digit(ws[k + 1:]) if ws[k + 1:] else 0
        return None if h is None or rest is None else f"{h * 100 + rest}{suffix}"
    if len(ws) >= 2 and all(w in ONES for w in ws):        # "three two two" -> 322
        return "".join(str(ONES[w]) for w in ws)
    if len(ws) >= 2 and ws[0] in ONES and ONES[ws[0]] > 0:  # "three twenty two" -> 322
        rest = _two_digit(ws[1:])
        if rest is not None and rest >= 10:
            return f"{ONES[ws[0]] * 100 + rest}{suffix}"
    v = _two_digit(ws)
    return None if v is None else f"{v}{suffix}"


def numbers_to_digits(tokens):
    """Normalized tokens -> (tokens with number-word runs as digits, how many tokens each covers)."""
    out, sizes, i = [], [], 0
    while i < len(tokens):
        j = i
        while j < len(tokens) and (tokens[j] in NUMBER_WORDS or
                                   (tokens[j] == "and" and j > i and tokens[j - 1] == "hundred")):
            j += 1
        val = _run_value(tokens[i:j]) if j > i else None
        if val is not None:
            out.append(val)
            sizes.append(j - i)
            i = j
        else:
            out.append(tokens[i])
            sizes.append(1)
            i += 1
    return out, sizes


# ---------- dates ----------

def ordinal(n):
    return f"{n}{'th' if 10 <= n % 100 <= 20 else {1: 'st', 2: 'nd', 3: 'rd'}.get(n % 10, 'th')}"


def spoken_date(d):
    return f"{DAYS[d.weekday()]}, {MONTHS[d.month - 1]} {ordinal(d.day)}"


def org_date(d):
    return f"{d:%Y-%m-%d} {DAYS[d.weekday()][:3]}"


def add_months(d, n):
    m = d.month - 1 + n
    y, m = d.year + m // 12, m % 12 + 1
    last = (dt.date(y + (m == 12), m % 12 + 1, 1) - dt.timedelta(days=1)).day
    return d.replace(year=y, month=m, day=min(d.day, last))


def add_unit(d, n, unit):
    if unit == "d":
        return d + dt.timedelta(days=n)
    if unit == "w":
        return d + dt.timedelta(weeks=n)
    if unit == "m":
        return add_months(d, n)
    if unit == "y":
        return add_months(d, 12 * n)
    raise ValueError(unit)


def parse_date(text, today):
    """Whole-phrase date parser for spoken dates. Returns a date or None."""
    t = " ".join(numbers_to_digits(norm(text).split())[0])
    t = re.sub(r"^(?:on|by|due|for)\s+", "", t)
    t = re.sub(r"\s+(\d+)\s*(st|nd|rd|th)\b", r" \1\2", t)
    if t in ("today", "tonight", "this evening"):
        return today
    if t == "tomorrow":
        return today + dt.timedelta(days=1)
    if t in ("day after tomorrow", "the day after tomorrow"):
        return today + dt.timedelta(days=2)
    if t in ("next week", "a week from today", "a week from now"):
        return today + dt.timedelta(days=7)
    if t in ("end of the month", "end of month", "the end of the month"):
        return add_months(today.replace(day=1), 1) - dt.timedelta(days=1)
    m = re.fullmatch(r"(?:(this|next|this coming|coming)\s+)?"
                     r"(mon|tues|tue|wednes|wed|thurs|thu|fri|satur|sat|sun)(?:day)?", t)
    if m:
        names = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"]
        wd = names.index(m.group(2)[:3])
        ahead = (wd - today.weekday()) % 7 or 7
        return today + dt.timedelta(days=ahead + (7 if m.group(1) == "next" else 0))
    m = re.fullmatch(r"in\s+(\w+)\s+(day|week|month|year)s?", t)
    if m:
        n = int(m.group(1)) if m.group(1).isdigit() else NUM_WORDS.get(m.group(1))
        if n is not None:
            return add_unit(today, n, m.group(2)[0])
    mon = r"(" + "|".join(sorted(MONTH_NUM, key=len, reverse=True)) + r")"
    day = r"(\d{1,2})(?:st|nd|rd|th)?"
    for pat, order in ((rf"{mon}\s+(?:the\s+)?{day}(?:\s+(\d{{4}}))?", "md"),
                       (rf"(?:the\s+)?{day}\s+(?:of\s+)?{mon}(?:\s+(\d{{4}}))?", "dm"),
                       (r"(\d{1,2})\s+(\d{1,2})(?:\s+(\d{2,4}))?", "nn"),
                       (rf"(?:the\s+)?{day}", "d")):
        m = re.fullmatch(pat, t)
        if not m:
            continue
        g = m.groups()
        try:
            if order == "md":
                mo, d, y = MONTH_NUM[g[0]], int(g[1]), g[2]
            elif order == "dm":
                mo, d, y = MONTH_NUM[g[1]], int(g[0]), g[2]
            elif order == "nn":
                mo, d, y = int(g[0]), int(g[1]), g[2]
            else:
                d = int(g[0])
                cand = today.replace(day=d) if d >= today.day else None
                return cand or add_months(today.replace(day=1), 1).replace(day=d)
            yr = int(y) + (2000 if y and len(y) == 2 else 0) if y else today.year
            res = dt.date(yr, mo, d)
            if not y and res < today:
                res = res.replace(year=yr + 1)
            return res
        except (ValueError, KeyError):
            return None
    return None


def split_trailing_date(title, today):
    """'call mom due friday' -> ('call mom', date). Only 'due/by/on <date>' or a final today/tomorrow."""
    words = title.split()
    for i in range(len(words) - 1, 0, -1):
        if norm(words[i]) in ("due", "by", "on"):
            d = parse_date(" ".join(words[i + 1:]), today)
            if d:
                return " ".join(words[:i]), d
    if len(words) > 1 and norm(words[-1]) in ("today", "tonight", "tomorrow"):
        return " ".join(words[:-1]), parse_date(words[-1], today)
    return title, None


# ---------- org structure ----------

def heading(line):
    m = HEADING_RE.match(line.rstrip("\r\n"))
    if not m:
        return None
    title = COOKIE_RE.sub("", TAGS_RE.sub("", m.group(3))).strip()
    return {"level": len(m.group(1)), "kw": m.group(2), "title": title}


def tree(lines):
    out, stack = [], []
    for i, line in enumerate(lines):
        h = heading(line)
        if not h:
            continue
        while stack and stack[-1]["level"] >= h["level"]:
            stack.pop()
        h["i"], h["parent"] = i, stack[-1] if stack else None
        h["excluded"] = h["title"].startswith(EXCLUDE_MARK) or bool(h["parent"] and h["parent"]["excluded"])
        h["name"] = h["title"].rstrip(":").strip()
        stack.append(h)
        out.append(h)
    return out


def children(secs, sec):
    return [s for s in secs if s["parent"] is sec and not s["excluded"]]


def descendants(secs, sec):
    out, frontier = [], [sec]
    while frontier:
        cur = frontier.pop()
        for k in children(secs, cur):
            out.append(k)
            frontier.append(k)
    return out


def find_top(secs, name):
    for s in secs:
        if s["level"] == 1 and not s["excluded"] and norm(s["name"]) == norm(name):
            return s
    raise Reply(f"I couldn't find a {name} heading in Tasks.org.")


def subtree_end(lines, sec):
    for j in range(sec["i"] + 1, len(lines)):
        h = heading(lines[j])
        if h and h["level"] <= sec["level"]:
            return j
    return len(lines)


def next_heading(lines, i):
    return next((j for j in range(i + 1, len(lines)) if heading(lines[j])), len(lines))


def body_start(lines, i):
    j = i + 1
    while j < len(lines) and PLANNING_RE.match(lines[j]):
        j += 1
    while j < len(lines) and DRAWER_RE.match(lines[j]):
        while j < len(lines) and not END_RE.match(lines[j]):
            j += 1
        j += 1
    return j


def todos_target(secs):
    todos = find_top(secs, "Todos")
    general = [k for k in children(secs, todos) if norm(k["name"]) == "general"]
    return general[0] if general else todos


def class_list(secs):
    school = find_top(secs, "School")
    return [k for k in children(secs, school) if norm(k["name"]) != "repeaters"]


def class_forms(c):
    """Ways a class heading can be said. For "TA CPT_S 122": "ta cpt s 122",
    "ta cpts 122", "122", "ta 122", "cpt 122", ... For "Misc": "misc"."""
    n = norm(c["name"])
    forms = {n, norm(c["name"].replace("_", ""))}  # "CPT_S 317" also as "cpts 317"
    digits = " ".join(re.findall(r"\d+", n))
    if digits:
        forms.add(digits)
        for w in n.split():
            if not w.isdigit():
                forms.add(f"{w} {digits}")
    return forms


def match_class(words, secs, ambiguous=None):
    """Like match_prefix over the classes, but understands spoken course numbers
    ("three twenty two", "TA one twenty-two"). Returns (class, original words used)."""
    toks, origin = [], []
    for oi, w in enumerate(words):
        for t in norm(w).split():
            toks.append(t)
            origin.append(oi)
    conv, sizes = numbers_to_digits(toks)

    def used(n):
        ntok = sum(sizes[:n])
        return origin[ntok - 1] + 1 if ntok else 0

    amb = []
    cls, k = match_prefix(conv, [(class_forms(c), c) for c in class_list(secs)], 3, amb)
    if amb and ambiguous is not None:
        ambiguous[:] = [used(amb[0]), amb[1]]
    return cls, used(k) if cls else 0


def class_spoken(c):
    """Short name for replies: the course number if there is one, else the heading."""
    digits = " ".join(re.findall(r"\d+", c["name"]))
    return digits or c["name"]


def match_prefix(words, cands, max_k=5, ambiguous=None):
    """cands: list of (forms, obj). Shortest leading run of words whose normalized
    text is one of an object's forms. Returns (obj, words_used) or (None, 0).
    If a run matches several objects ("317" for CPT_S 317 and PHIL 317), returns
    (None, 0) and, when given, fills `ambiguous` with (words_used, [objs])."""
    for k in range(1, min(max_k, len(words)) + 1):
        key = norm(" ".join(words[:k]))
        hits = [o for forms, o in cands if key in forms]
        if len(hits) == 1:
            return hits[0], k
        if len(hits) > 1 and not any(len([o for f, o in cands if norm(" ".join(words[:j])) in f]) == 1
                                     for j in range(k + 1, min(max_k, len(words)) + 1)):
            if ambiguous is not None:
                ambiguous[:] = [k, hits]
            return None, 0
    for forms, o in cands:  # small slips: "neo vim", "barrista assistant"
        for f in forms:
            k = len(f.split())
            if 0 < k <= len(words) and \
                    difflib.SequenceMatcher(None, norm(" ".join(words[:k])), f).ratio() >= 0.85:
                return o, k
    return None, 0


def project_cands(secs):
    return [({norm(p["name"])}, p) for p in children(secs, find_top(secs, "Projects"))]


def system_cands(secs):
    return [({norm(s["name"])}, s) for s in children(secs, find_top(secs, "System"))]


# ---------- edits ----------

def insert_heading(lines, eol, parent, kw, title, date):
    end = subtree_end(lines, parent)
    while end - 1 > parent["i"] and not lines[end - 1].strip():
        end -= 1
    new = [f"{'*' * (parent['level'] + 1)} {kw} {title}{eol}"]
    if date:
        new.append(f"DEADLINE: <{org_date(date)}>{eol}")
    lines[end:end] = new
    return new[0].rstrip("\r\n")


def insert_checkbox(lines, eol, sec, text):
    start, stop = body_start(lines, sec["i"]), next_heading(lines, sec["i"])
    last = max((j for j in range(start, stop) if lines[j].strip()), default=None)
    if last is None:
        pos, indent = start, ""
    else:
        m = ITEM_RE.match(lines[last].rstrip("\r\n"))
        pos = last if (m and m.group(3) == "") else last + 1  # stay above a bottom placeholder
        items = [ITEM_RE.match(lines[j].rstrip("\r\n")) for j in range(start, last + 1)]
        indent = next((x.group(1) for x in reversed(items) if x), "")
    lines.insert(pos, f"{indent}- [ ] {text}{eol}")


def add_deadline(lines, eol, heading_text, date):
    idx = next((j for j in range(len(lines) - 1, -1, -1)
                if lines[j].rstrip("\r\n") == heading_text), None)
    if idx is None:
        raise Reply("I couldn't find that entry any more, so I didn't add a deadline.")
    if idx + 1 < len(lines) and PLANNING_RE.match(lines[idx + 1]):
        if "DEADLINE:" in lines[idx + 1]:
            raise Reply("That entry already has a deadline.")
        lines[idx + 1] = f"DEADLINE: <{org_date(date)}> " + lines[idx + 1].lstrip()
    else:
        lines.insert(idx + 1, f"DEADLINE: <{org_date(date)}>{eol}")


def stamp(now):
    return f"[{now:%Y-%m-%d} {DAYS[now.weekday()][:3]} {now:%H:%M}]"


def log_state(lines, eol, i, old_kw, now):
    """Add '- State "DONE" from "<old>" [ts]' to the LOGBOOK, as Emacs does."""
    j = i + 1
    while j < len(lines) and PLANNING_RE.match(lines[j]):
        j += 1
    if j < len(lines) and lines[j].strip().upper() == ":PROPERTIES:":
        while j < len(lines) and not END_RE.match(lines[j]):
            j += 1
        j += 1
    note = f'- State {chr(34) + "DONE" + chr(34):<12} from {chr(34) + old_kw + chr(34):<12} {stamp(now)}{eol}'
    if j < len(lines) and lines[j].strip().upper() == ":LOGBOOK:":
        lines.insert(j + 1, note)
    else:
        lines[j:j] = [f":LOGBOOK:{eol}", note, f":END:{eol}"]


def set_last_repeat(lines, eol, i, now):
    j = i + 1
    while j < len(lines) and PLANNING_RE.match(lines[j]):
        j += 1
    prop = f":LAST_REPEAT: {stamp(now)}{eol}"
    if j < len(lines) and lines[j].strip().upper() == ":PROPERTIES:":
        k = j + 1
        while k < len(lines) and not END_RE.match(lines[k]):
            if lines[k].strip().upper().startswith(":LAST_REPEAT:"):
                lines[k] = prop
                return
            k += 1
        lines.insert(j + 1, prop)
    else:
        lines[j:j] = [f":PROPERTIES:{eol}", prop, f":END:{eol}"]


def mark_paid(lines, eol, bill, now):
    """Returns the next due date for a repeating bill, else None."""
    i, today = bill["i"], now.date()
    plan = i + 1 if i + 1 < len(lines) and PLANNING_RE.match(lines[i + 1]) else None
    repeat = plan is not None and REPEAT_RE.search(lines[plan] or "")
    next_due = None
    if repeat:
        def advance(m):
            nonlocal next_due
            d = dt.date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
            rest = m.group(4)
            r = REPEAT_RE.search(rest)
            if not r or r.group(3) == "h":
                return m.group(0)
            kind, n, unit = r.group(1), int(r.group(2)), r.group(3)
            if kind == ".+":
                d = add_unit(today, n, unit)
            else:
                d = add_unit(d, n, unit)
                while kind == "++" and d <= today:
                    d = add_unit(d, n, unit)
            if "DEADLINE" in lines[plan][:m.start()] or next_due is None:
                next_due = d
            return f"<{org_date(d)}{rest}>"
        lines[plan] = STAMP_RE.sub(advance, lines[plan])
        log_state(lines, eol, i, bill["kw"], now)
        set_last_repeat(lines, eol, i, now)
    else:
        lines[i] = re.sub(rf"^(\*+\s+){bill['kw']}\b", r"\1DONE", lines[i], count=1)
        if plan is not None:
            lines[plan] = f"CLOSED: {stamp(now)} " + lines[plan].lstrip()
        else:
            lines.insert(i + 1, f"CLOSED: {stamp(now)}{eol}")
        log_state(lines, eol, i, bill["kw"], now)
    return next_due


# ---------- bills ----------

def bill_name(title):
    t = LINK_RE.sub(" ", title)
    t = re.sub(r"\([^)]*\)", " ", t)
    return " ".join(URL_RE.sub(" ", t).split()).strip(" -") or title


def deadline_of(lines, sec):
    j = sec["i"] + 1
    if j < len(lines) and PLANNING_RE.match(lines[j]):
        m = re.search(r"DEADLINE:\s*" + STAMP_RE.pattern, lines[j])
        if m:
            return dt.date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    return None


def open_bills(lines, secs):
    bills = find_top(secs, "Bills")
    return [s for s in descendants(secs, bills) if s["kw"] == "BILL"]


def match_bill(lines, secs, spoken):
    sp = norm(spoken)
    sp = re.sub(r"^(?:the|my|our)\s+", "", sp)
    sp = re.sub(r"\s*\b(?:bill|bills|payment)$", "", sp).strip()
    if not sp:
        raise Reply("Which bill did you pay?")
    cands = [(norm(bill_name(b["title"])), b) for b in open_bills(lines, secs)]
    hits = [b for k, b in cands if k == sp]
    if not hits:
        hits = [b for k, b in cands if set(sp.split()) <= set(k.split())]
    if not hits:
        scored = [(difflib.SequenceMatcher(None, sp, k).ratio(), b) for k, b in cands]
        best = max((s for s, _ in scored), default=0)
        hits = [b for s, b in scored if s == best and s >= 0.8]
    if not hits:
        raise Reply(f"I couldn't find an unpaid {sp} bill.")
    return min(hits, key=lambda b: deadline_of(lines, b) or dt.date.max)


# ---------- state ----------

def load_state():
    try:
        with open(STATE_FILE) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def save_state(state):
    try:
        with open(STATE_FILE, "w") as f:
            json.dump(state, f)
    except OSError as e:
        print(f"tasks: could not save state: {e!r}", file=sys.stderr)


def fresh(entry, now):
    if not entry:
        return False
    t = dt.datetime.fromisoformat(entry["time"])
    return (now - t) <= dt.timedelta(minutes=FOLLOW_UP_MINUTES)


# ---------- file I/O ----------

def read_file(path):
    with open(path, "r", encoding="utf-8", newline="") as f:
        text = f.read()
    lines = text.splitlines(keepends=True)
    eol = "\r\n" if lines and lines[0].endswith("\r\n") else "\n"
    if lines and not lines[-1].endswith(("\n", "\r")):
        lines[-1] += eol
    return text, lines, eol


def atomic_write(path, content):
    st = os.stat(path)
    d, base = os.path.split(path)
    fd, tmp = tempfile.mkstemp(dir=d, prefix=f".syncthing.{base}.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="") as f:
            f.write(content)
        os.chmod(tmp, st.st_mode)
        try:
            os.chown(tmp, st.st_uid, st.st_gid)
        except PermissionError:
            pass
        os.replace(tmp, path)
    except BaseException:
        if os.path.exists(tmp):
            os.unlink(tmp)
        raise


def sha(s):
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def commit(path, old_text, lines, state, what):
    new_text = "".join(lines)
    try:
        with open(UNDO_FILE, "w", encoding="utf-8", newline="") as f:
            f.write(old_text)
        state["undo"] = {"hash": sha(new_text), "what": what, "time": dt.datetime.now().isoformat()}
    except OSError as e:
        print(f"tasks: could not save undo copy: {e!r}", file=sys.stderr)
    atomic_write(path, new_text)


# ---------- operations ----------

def capture(words, kw, parent, where, lines, eol, state, now, date=None):
    title = clean_title(" ".join(words))
    if date is None:
        title, date = split_trailing_date(title, now.date())
    title = clean_title(title)
    if not title:
        raise Reply("Sorry, I didn't catch what to capture.")
    text = insert_heading(lines, eol, parent, kw, title, date)
    if date:
        state.pop("last", None)
        return f"Added {title} to {where}, due {spoken_date(date)}.", False, f"adding {title}"
    state["last"] = {"heading": text, "title": title, "time": now.isoformat()}
    return (f"Added {title} to {where}. When's it due?",
            True, f"adding {title}")


def capture_assignment(secs, cls, words, lines, eol, state, now):
    title, date = split_trailing_date(clean_title(" ".join(words)), now.date())
    if not clean_title(title):
        raise Reply("Say the assignment name too, like 'capture assignment for 317 lab five due Friday'.")
    if cls is None:
        state["pending"] = {"op": "assignment", "title": clean_title(title),
                            "date": date.isoformat() if date else None, "time": now.isoformat()}
        return "Which class?", True, None
    state.pop("pending", None)
    return capture([title], "ASSIGNMENT", cls, class_spoken(cls), lines, eol, state, now, date)


def do_capture(text, lines, eol, secs, state, now):
    words = text.split()
    if not words:
        raise Reply("Sorry, I didn't catch what to capture.")
    # "capture take out the trash into my inbox" == "capture inbox take out the trash"
    m = re.search(r"\s+(?:to|into|in|for)\s+(?:my\s+|the\s+)?inbox[\s.!?]*$", text, re.I)
    if m and norm(words[0]) != "inbox":
        words = ["inbox"] + text[:m.start()].split()
    first = norm(words[0])

    if first == "inbox":
        rest = words[1:]
        kw = "TODO"
        for k in (2, 1):
            if len(rest) > k and norm(" ".join(rest[:k])) in SPOKEN_KEYWORDS:
                kw, rest = SPOKEN_KEYWORDS[norm(" ".join(rest[:k]))], rest[k:]
                break
        return capture(rest, kw, find_top(secs, "Inbox"), "your inbox", lines, eol, state, now)

    if first in ("assignment", "assignments"):
        rest, cls = words[1:], None
        if rest and norm(rest[0]) == "for":
            amb = []
            cls, used = match_class(rest[1:], secs, amb)
            if amb:          # "for 317" fits two classes: drop the number, then ask which
                rest = rest[1 + amb[0]:]
            elif cls is None:  # unknown class: everything after "for" is the title, then ask
                rest = rest[1:]
            else:
                rest = rest[1 + used:]
        return capture_assignment(secs, cls, rest, lines, eol, state, now)

    if first == "system":
        sec, used = match_prefix(words[1:], system_cands(secs), 3)
        if sec is None:
            raise Reply("I couldn't find that system list.")
        item = clean_title(" ".join(words[1 + used:]))
        if not item:
            raise Reply("Sorry, I didn't catch what to add.")
        insert_checkbox(lines, eol, sec, item)
        state.pop("last", None)
        return f"Added {item} to System {sec['name']}.", False, f"adding {item}"

    if first == "chore":
        return capture(words[1:], "CHORE", todos_target(secs), "your chores", lines, eol, state, now)

    proj, used = match_prefix(words, project_cands(secs), 5)
    if proj is not None and len(words) > used:
        rest = words[used:]
        nxt = norm(rest[0])
        if nxt == "backlog":
            backlog = [k for k in children(secs, proj) if norm(k["name"]) == "backlog"]
            if not backlog:
                raise Reply(f"{proj['name']} doesn't have a Backlog heading.")
            item = clean_title(" ".join(rest[1:]))
            if not item:
                raise Reply("Sorry, I didn't catch what to add.")
            insert_checkbox(lines, eol, backlog[0], item)
            state.pop("last", None)
            return f"Added {item} to the {proj['name']} backlog.", False, f"adding {item}"
        rest = strip_todo_word(rest)
        return capture(rest, "TODO", proj, proj["name"], lines, eol, state, now)

    return capture(strip_todo_word(words), "TODO", todos_target(secs), "your todos",
                   lines, eol, state, now)


def strip_todo_word(words):
    """Drop a leading 'todo' / 'to do' / 'task' (the target word, not the title)."""
    if len(words) > 2 and norm(" ".join(words[:2])) == "to do":
        return words[2:]
    if len(words) > 1 and norm(words[0]) in ("todo", "to do"):
        return words[1:]
    return words


def find_class_anywhere(tokens, secs):
    """The class may follow junk the mic picked up ("which class three twenty two")."""
    for start in range(len(tokens)):
        cls, used = match_class(tokens[start:], secs)
        if cls is not None:
            return cls, start + used
    return None, 0


def find_date_anywhere(tokens, today):
    """Longest run of words anywhere in the answer that reads as a date."""
    n = len(tokens)
    for size in range(n, 0, -1):
        for start in range(0, n - size + 1):
            d = parse_date(" ".join(tokens[start:start + size]), today)
            if d:
                return d
    return None


def has_phrase(t, phrases):
    """Exact match for one-word phrases; multi-word phrases may appear anywhere."""
    padded = f" {t} "
    return t in phrases or any(" " in p and f" {p} " in padded for p in phrases)


def do_answer(text, lines, eol, secs, state, now):
    print(f"tasks answer heard: {text!r}", file=sys.stderr)  # shows in the HA trace
    t = norm(text)
    t = re.sub(r"^(?:its|it is|it s|that s|thats)\s+", "", t)
    if not t:  # no answer (timed out / silence): leave everything as it is, say nothing
        state.pop("pending", None)
        state.pop("last", None)
        return "", False, None
    pending = state.get("pending")
    if fresh(pending, now):
        words = t.split()
        cls, used = find_class_anywhere(words, secs)
        if cls is None:
            raise Reply("Which class?", ask=True)
        date = dt.date.fromisoformat(pending["date"]) if pending.get("date") else None
        if date is None and words[used:]:  # "322 due friday" in one go
            date = find_date_anywhere(words[used:], now.date())
        state.pop("pending", None)
        return capture([pending["title"]], "ASSIGNMENT", cls, class_spoken(cls),
                       lines, eol, state, now, date)
    last = state.get("last")
    if fresh(last, now):
        if has_phrase(t, NO_DEADLINE):
            state.pop("last", None)
            return "OK, no deadline.", False, None
        d = find_date_anywhere(t.split(), now.date())
        if d is None:
            raise Reply("When's it due?", ask=True)
        add_deadline(lines, eol, last["heading"], d)
        state.pop("last", None)
        return f"Got it. {last['title']} is due {spoken_date(d)}.", False, f"adding a deadline to {last['title']}"
    raise Reply("There's nothing waiting for an answer.")


def do_paid(text, lines, eol, secs, now):
    bill = match_bill(lines, secs, text)
    name = bill_name(bill["title"])
    next_due = mark_paid(lines, eol, bill, now)
    if next_due:
        return f"Marked {name} as paid. The next one is due {spoken_date(next_due)}.", f"paying {name}"
    return f"Marked {name} as paid.", f"paying {name}"


def do_bills(lines, secs, now):
    today = now.date()
    bills = sorted(open_bills(lines, secs), key=lambda b: deadline_of(lines, b) or dt.date.max)
    if not bills:
        return "You have no unpaid bills."
    parts = []
    for b in bills[:10]:
        d, name = deadline_of(lines, b), bill_name(b["title"])
        if d is None:
            parts.append(name)
        elif d < today:
            parts.append(f"{name}, overdue since {MONTHS[d.month - 1]} {ordinal(d.day)}")
        elif d == today:
            parts.append(f"{name}, due today")
        else:
            parts.append(f"{name}, due {spoken_date(d)}")
    more = f", and {len(bills) - 10} more" if len(bills) > 10 else ""
    n = len(bills)
    return f"You have {n} unpaid bill{'s' if n != 1 else ''}: {'; '.join(parts)}{more}."


def open_headings(lines, secs, sec):
    return [say_text(k["title"]) for k in children(secs, sec) if k["kw"] in ACTIVE]


def open_items(lines, sec):
    out = []
    for j in range(body_start(lines, sec["i"]), next_heading(lines, sec["i"])):
        m = ITEM_RE.match(lines[j].rstrip("\r\n"))
        if m and m.group(2) == " " and m.group(3):
            out.append(say_text(m.group(3)))
    return out


def speak_list(name, items, limit=15):
    if not items:
        return f"{cap(name)} is empty."
    more = f", and {len(items) - limit} more" if len(items) > limit else ""
    n = len(items)
    return f"{cap(name)} has {n} item{'s' if n != 1 else ''}: {', '.join(items[:limit])}{more}."


def do_read(text, lines, secs, now):
    t = re.sub(r"^(?:my|the|our)\s+", "", norm(text))
    t = re.sub(r"\s+list$", "", t)
    if t in ("todo", "to do"):
        return speak_list("your todo list", open_headings(lines, secs, todos_target(secs)))
    if t == "inbox":
        return speak_list("your inbox", open_headings(lines, secs, find_top(secs, "Inbox")))
    if t in ("bill", "bills"):
        return do_bills(lines, secs, now)
    words = t.split()
    if words and words[0] == "system":
        words = words[1:]
    sec, used = match_prefix(words, system_cands(secs), 3)
    if sec is not None and used == len(words):
        return speak_list(f"System {sec['name']}", open_items(lines, sec))
    proj, used = match_prefix(t.split(), project_cands(secs), 5)
    if proj is not None:
        rest = " ".join(t.split()[used:])
        if rest == "backlog":
            backlog = [k for k in children(secs, proj) if norm(k["name"]) == "backlog"]
            if backlog:
                return speak_list(f"the {proj['name']} backlog", open_items(lines, backlog[0]))
        if rest in ("", "todo", "to do"):
            return speak_list(proj["name"], open_headings(lines, secs, proj))
    raise Reply(f"I couldn't find a {text} list.")


def do_undo(path, state):
    undo = state.get("undo")
    if not undo or not os.path.exists(UNDO_FILE):
        raise Reply("There's nothing to undo.")
    current, _, _ = read_file(path)
    if sha(current) != undo["hash"]:
        raise Reply("Tasks.org has changed since then, so I won't undo it.")
    with open(UNDO_FILE, encoding="utf-8", newline="") as f:
        atomic_write(path, f.read())
    for k in ("undo", "last", "pending"):
        state.pop(k, None)
    return f"Undone: {undo['what']}."


def do_cancel(path, state, now):
    """'Scratch that': drop a waiting question and undo a capture made in the last few minutes."""
    pending = state.pop("pending", None)
    if pending and fresh(pending, now):  # nothing written yet: just drop the question
        state.pop("last", None)
        return "OK, cancelled."
    state.pop("last", None)
    undo = state.get("undo")
    if undo and fresh(undo, now):
        try:
            do_undo(path, state)
            return f"Scratched that. I undid {undo['what']}."
        except Reply as r:
            return f"I couldn't undo it. {r.say}"
    if pending:
        return "OK, cancelled."
    return "There's nothing to cancel."


def run(op, text, path=TASKS_FILE, now=None):
    now = now or dt.datetime.now()
    state = load_state()
    try:
        if op == "answer" and has_phrase(re.sub(r"^(?:just|please|ok|okay)\s+", "", norm(text)), CANCEL):
            op = "cancel"
        if op == "cancel":
            return {"say": do_cancel(path, state, now), "ask": False}
        if op == "undo":
            return {"say": do_undo(path, state), "ask": False}
        old, lines, eol = read_file(path)
        secs = tree(lines)
        what, ask = None, False
        if op in ("capture", "inbox"):
            say, ask, what = do_capture(("inbox " if op == "inbox" else "") + text,
                                        lines, eol, secs, state, now)
        elif op == "answer":
            say, ask, what = do_answer(text, lines, eol, secs, state, now)
        elif op == "paid":
            say, what = do_paid(text, lines, eol, secs, now)
        elif op == "bills":
            say = do_bills(lines, secs, now)
        elif op == "read":
            say = do_read(text, lines, secs, now)
        else:
            raise Reply("Sorry, I don't know how to do that with your tasks.")
        if what:
            commit(path, old, lines, state, what)
        return {"say": say, "ask": ask}
    except Reply as r:
        return {"say": r.say, "ask": r.ask}
    finally:
        save_state(state)


def main():
    args = sys.argv[1:] + ["", ""]
    try:
        out = run(args[0].strip().lower(), args[1])
    except Exception as e:  # never crash the automation
        print(f"tasks failed: {e!r}", file=sys.stderr)
        out = {"say": "Sorry, something went wrong updating your tasks.", "ask": False}
    print(json.dumps(out))


if __name__ == "__main__":
    main()
