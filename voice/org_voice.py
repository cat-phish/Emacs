#!/usr/bin/env python3
"""Voice engine for Org files (Home Assistant shell_command).

Usage:
  org_voice.py [--dry-run] <verb> "<text>"
  org_voice.py describe [--json]

Verbs (HA needs one trigger per verb; the rest of the sentence is free text):
  capture, add  write an item or heading. The target is named at either end:
                "bagels to the costco list", "lab five for 322 due friday",
                "system home lab set up headscale"; otherwise the VOICE_DEFAULT heading.
  done          complete: a checkbox becomes [X], a heading DONE (repeaters advance).
                "eggs and milk", "take out the trash", "eggs off the costco list".
  paid          like done, but only BILL headings: "the electric bill".
  out           "the bar is out of bourbon", "we're out of milk": un-complete,
                following VOICE_LINK. Never adds anything.
  read          read a list back: "the costco list", "bills", "" (the default target).
  answer        a reply to a follow-up question ("due friday", "322", "no deadline").
  undo, cancel  undo the last change / "scratch that".
  describe      every reachable heading, its spoken names and effective rules.

--dry-run prints the reply and the diff it would write, and saves nothing.

Where things go and how comes from VOICE_* properties on the Org headings
(inherited by the subtree, except VOICE_NAME, VOICE_PICK and VOICE_DEFAULT),
then the file's #+PROPERTY: lines, then the engine defaults. Engine-wide
settings come from .voice.toml (VOICE_CONFIG). See HANDOFF-voice-org-engine.md.

Prints JSON {"say": "...", "ask": true|false} and always exits 0; "ask" means
the reply is a question waiting for an answer. Standard library only.
"""
import copy
import datetime as dt
import difflib
import fcntl
import hashlib
import json
import os
import re
import sys
import tempfile
import tomllib

CONFIG_FILE = os.environ.get("VOICE_CONFIG", "/drives/local/org/main/.voice.toml")
STATE_DIR = os.environ.get("VOICE_STATE_DIR", os.path.dirname(os.path.abspath(__file__)))

DEFAULT_CONFIG = {
    "files": ["Tasks.org", "Shopping.org"],
    "filler": ["the", "my", "our", "some", "please"],
    # spoken word at the start of a capture -> TODO keyword ("capture chore ...")
    "keywords": {"todo": "TODO", "to do": "TODO", "chore": "CHORE"},
    "todo_keywords": ["TODO", "ASSIGNMENT", "BILL", "CHORE", "MEETING", "NEXT", "PLANNING",
                      "REVIEW", "HOLD", "READY", "ACTIVE"],
    "done_keywords": ["DONE", "CANCELED"],
    "read_limit": 15,
    "follow_up_minutes": 10,   # how long a question waits for an answer
    "undo_minutes": 10,        # how far back "scratch that" reaches
    "match": {"item_cutoff": 0.88, "name_cutoff": 0.85},
}

LOCAL_KEYS = {"VOICE_NAME", "VOICE_PICK", "VOICE_DEFAULT"}   # never inherited
PREPOSITIONS = {"to", "into", "in", "on", "onto", "for", "from", "off", "at", "under"}
CANCEL = {"cancel", "cancel that", "scratch", "scratch that", "never mind", "nevermind",
          "forget it", "forget that", "stop"}
NO_DEADLINE = {"no", "none", "nope", "no deadline", "no due date", "no date", "skip",
               "never", "nothing"}
NO_SPLIT = {"mac and cheese", "macaroni and cheese", "half and half"}
MAX_NAME_TOKENS = 8

COOKIE_RE = re.compile(r"\s*\[\d*(?:/\d*|%)\]")
TAGS_RE = re.compile(r"\s+:[\w@#%:]+:\s*$")
LINK_RE = re.compile(r"\[\[([^\]]*)\](?:\[([^\]]*)\])?\]")
URL_RE = re.compile(r"https?://\S+")
ITEM_RE = re.compile(r"^(\s*)[-+]\s+\[([ xX-])\]\s?(.*?)\s*$")
PLANNING_RE = re.compile(r"^\s*(?:DEADLINE|SCHEDULED|CLOSED):")
DRAWER_RE = re.compile(r"^\s*:(?!END:)[A-Za-z_-]+:\s*$")
END_RE = re.compile(r"^\s*:END:")
PROP_RE = re.compile(r"^\s*:([^:\s]+):\s*(.*?)\s*$")
FILE_PROP_RE = re.compile(r"^#\+PROPERTY:\s+(\S+)\s*(.*?)\s*$", re.I)
STAMP_RE = re.compile(r"<(\d{4})-(\d{2})-(\d{2})(?: [A-Za-z]{2,3})?([^>]*)>")
REPEAT_RE = re.compile(r"(\.\+|\+\+|\+)(\d+)([hdwmy])")

DAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August",
          "September", "October", "November", "December"]
MONTH_NUM = {m.lower(): i + 1 for i, m in enumerate(MONTHS)}
MONTH_NUM.update({m[:3].lower(): i + 1 for i, m in enumerate(MONTHS)})
MONTH_NUM["sept"] = 9
NUM_WORDS = {w: i for i, w in enumerate(
    "zero one two three four five six seven eight nine ten eleven twelve".split())}
NUM_WORDS.update({"a": 1, "an": 1})
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


class Reply(Exception):
    """A message to speak instead of carrying on."""
    def __init__(self, say, ask=False):
        super().__init__(say)
        self.say, self.ask = say, ask


class Ambiguous(Exception):
    """A spoken name fits several headings."""
    def __init__(self, heads):
        super().__init__("ambiguous")
        self.heads = heads


# ---------- text ----------

def norm(s):
    s = LINK_RE.sub(" ", s)
    s = URL_RE.sub(" ", s.lower()).replace("&", " and ")
    s = re.sub(r"[.'’]", "", s)
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return " ".join("and" if t == "n" else t for t in s.split())


def singular(t):
    if len(t) <= 3 or t.endswith("ss") or t.isdigit():
        return t
    if t.endswith("ies"):
        return t[:-3] + "y"
    if t.endswith(("oes", "ses", "xes", "ches", "shes")):
        return t[:-2]
    return t[:-1] if t.endswith("s") else t


def say_text(s):
    """Title or item text for speech: link descriptions kept, bare URLs dropped."""
    s = LINK_RE.sub(lambda m: m.group(2) or "", s)
    s = " ".join(URL_RE.sub("", s).split()).strip(" -")
    return s or "a link"


def clean_title(s):
    return " ".join(s.split()).strip(" .!?,;:")


def clean_item(s):
    s = clean_title(s)
    return re.sub(r"^(?:some|more|the|a|an)\s+", "", s, flags=re.I)


def join_words(words, conj="and"):
    words = list(words)
    if len(words) <= 2:
        return f" {conj} ".join(words)
    return ", ".join(words[:-1]) + f", {conj} " + words[-1]


def cap(s):
    return s[:1].upper() + s[1:]


def has_phrase(t, phrases):
    """Exact match for one-word phrases; multi-word phrases may appear anywhere."""
    padded = f" {t} "
    return t in phrases or any(" " in p and f" {p} " in padded for p in phrases)


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


def spoken_tokens(text):
    """-> (words, tokens). Each token is (key, first word index, last word index):
    normalized, number words as digits, singular, "wish list" as "wishlist"."""
    words = text.split()
    toks, origin = [], []
    for oi, w in enumerate(words):
        for t in norm(w).split():
            toks.append(t)
            origin.append(oi)
    conv, sizes = numbers_to_digits(toks)
    out, k = [], 0
    for c, n in zip(conv, sizes):
        out.append([singular(c), origin[k], origin[k + n - 1]])
        k += n
    merged = []
    for t in out:
        if merged and merged[-1][0] == "wish" and t[0] == "list":
            merged[-1] = ["wishlist", merged[-1][1], t[2]]
        else:
            merged.append(t)
    return words, [tuple(t) for t in merged]


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


def find_date_anywhere(tokens, today):
    """Longest run of words anywhere in the answer that reads as a date."""
    n = len(tokens)
    for size in range(n, 0, -1):
        for start in range(0, n - size + 1):
            d = parse_date(" ".join(tokens[start:start + size]), today)
            if d:
                return d
    return None


# ---------- items ----------

def item_key(s):
    n = re.sub(r"^(?:a|an|the|some|more)\s+", "", norm(s))
    return " ".join(singular(t) for t in n.split())


def item_keys(text):
    base = URL_RE.sub("", LINK_RE.sub(lambda m: m.group(2) or "", text)).strip()
    keys = {item_key(base)}
    m = re.match(r"^(.*?)\s*\((.*?)\)\s*$", base)  # "mayo (dukes)" ~ "dukes mayo"
    if m:
        x, y = m.group(1), m.group(2)
        for q in {y, y.split(",")[0]}:  # "(Green, or sub)" also matches as "green"
            keys |= {item_key(f"{q} {x}"), item_key(f"{x} {q}")}
    keys.discard("")
    return keys


def item_parts(line):
    m = ITEM_RE.match(line.rstrip("\r\n"))
    return m.group(1), m.group(2), m.group(3)  # indent, mark, text


def is_blank(line):
    return item_parts(line)[2] == ""


def is_checked(line):
    return item_parts(line)[1] in "xX"


def set_mark(lines, j, mark):
    lines[j] = re.sub(r"\[[ xX-]\]", f"[{mark}]", lines[j], count=1)


def split_items(text, catalog):
    """'eggs, milk and bread' -> three items, unless the whole phrase is a known item."""
    text = " ".join(text.split()).strip(" .!?,;:")
    known = set(catalog) | {item_key(x) for x in NO_SPLIT}
    out = []
    for part in (p for p in re.split(r"\s*[,;]\s*", text) if p):
        part = re.sub(r"^(?:and|also|plus)\s+", "", part, flags=re.I)
        sub = [s for s in re.split(r"\s+(?:and|&)\s+", part, flags=re.I) if s.strip()]
        if item_key(part) in known or len(sub) < 2:
            out.append(part)
            continue
        merged, i = [], 0   # keep known "x and y" items together: "milk and mac and cheese"
        while i < len(sub):
            for k in range(len(sub), i + 1, -1):
                if item_key(" and ".join(sub[i:k])) in known:
                    merged.append(" and ".join(sub[i:k]))
                    i = k
                    break
            else:
                merged.append(sub[i])
                i += 1
        if len({item_key(s) for s in merged}) == len(merged):
            out.extend(merged)
        else:
            out.append(part)
    seen, items = set(), []
    for it in map(clean_item, out):
        if it and item_key(it) not in seen:
            seen.add(item_key(it))
            items.append(it)
    return items


# ---------- org files ----------

class Heading:
    def __init__(self, doc, i, level, kw, title):
        self.doc, self.i, self.level, self.kw, self.title = doc, i, level, kw, title
        self.parent, self.kids, self.props, self.excluded = None, [], {}, False

    def path(self):
        out, h = [], self
        while h:
            out.append(h.title)
            h = h.parent
        return out[::-1]

    def line(self):
        return self.doc.lines[self.i].rstrip("\r\n")

    def ref(self):
        return {"file": self.doc.name, "path": self.path(), "line": self.line()}

    def __repr__(self):
        return f"<{self.doc.name}: {' › '.join(self.path())}>"


class Doc:
    def __init__(self, path, cfg):
        self.path, self.name, self.cfg = path, os.path.basename(path), cfg
        with open(path, "r", encoding="utf-8", newline="") as f:
            self.text = f.read()
        self.lines = self.text.splitlines(keepends=True)
        self.eol = "\r\n" if self.lines and self.lines[0].endswith("\r\n") else "\n"
        if self.lines and not self.lines[-1].endswith(("\n", "\r")):
            self.lines[-1] += self.eol
        kws = "|".join(map(re.escape, cfg["todo_keywords"] + cfg["done_keywords"]))
        self.heading_re = re.compile(rf"^(\*+)\s+(?:({kws})(?:\s+|$))?(?:\[#\w\]\s+)?(.*?)\s*$")
        self.parse()

    def is_heading(self, line):
        return line.startswith("*") and bool(re.match(r"^\*+\s", line))

    def parse(self):
        self.file_props, self.heads, stack = {}, [], []
        for i, line in enumerate(self.lines):
            m = FILE_PROP_RE.match(line.rstrip("\r\n"))
            if m:
                self.file_props[m.group(1).upper()] = m.group(2)
                continue
            if not self.is_heading(line):
                continue
            m = self.heading_re.match(line.rstrip("\r\n"))
            title = COOKIE_RE.sub("", TAGS_RE.sub("", m.group(3))).strip().rstrip(":").strip()
            h = Heading(self, i, len(m.group(1)), m.group(2), title)
            while stack and stack[-1].level >= h.level:
                stack.pop()
            if stack:
                h.parent = stack[-1]
                h.parent.kids.append(h)
            h.props = self.read_props(i)
            h.excluded = (title.startswith("#") or h.props.get("VOICE", "").lower() == "off"
                          or bool(h.parent and h.parent.excluded))
            stack.append(h)
            self.heads.append(h)

    def read_props(self, i):
        j, props = i + 1, {}
        while j < len(self.lines) and PLANNING_RE.match(self.lines[j]):
            j += 1
        if j < len(self.lines) and self.lines[j].strip().upper() == ":PROPERTIES:":
            j += 1
            while j < len(self.lines) and not END_RE.match(self.lines[j]):
                m = PROP_RE.match(self.lines[j].rstrip("\r\n"))
                if m:
                    props[m.group(1).upper()] = m.group(2)
                j += 1
        return props

    def body_start(self, i):
        j = i + 1
        while j < len(self.lines) and PLANNING_RE.match(self.lines[j]):
            j += 1
        while j < len(self.lines) and DRAWER_RE.match(self.lines[j]):
            while j < len(self.lines) and not END_RE.match(self.lines[j]):
                j += 1
            j += 1
        return j

    def next_heading(self, i):
        return next((j for j in range(i + 1, len(self.lines)) if self.is_heading(self.lines[j])),
                    len(self.lines))

    def subtree_end(self, h):
        for j in range(h.i + 1, len(self.lines)):
            if self.is_heading(self.lines[j]):
                level = len(self.lines[j]) - len(self.lines[j].lstrip("*"))
                if level <= h.level:
                    return j
        return len(self.lines)

    def items(self, h):
        """Line numbers of h's own checkbox items, and where its body starts."""
        start, end = self.body_start(h.i), self.next_heading(h.i)
        return [j for j in range(start, end) if ITEM_RE.match(self.lines[j])], start

    def new_text(self):
        return "".join(self.lines)


# ---------- edits ----------

def stamp(now):
    return f"[{now:%Y-%m-%d} {DAYS[now.weekday()][:3]} {now:%H:%M}]"


def insert_heading(doc, parent, kw, title, date):
    """New heading at the bottom of parent's subtree; returns its line."""
    lines, eol = doc.lines, doc.eol
    end = doc.subtree_end(parent)
    while end - 1 > parent.i and not lines[end - 1].strip():
        end -= 1
    new = [f"{'*' * (parent.level + 1)} {kw} {title}{eol}"]
    if date:
        new.append(f"DEADLINE: <{org_date(date)}>{eol}")
    lines[end:end] = new
    return new[0].rstrip("\r\n")


def insert_item(doc, leaf, text, place, after=None):
    """New checkbox in leaf. top: first, just under a blank '- [ ]' placeholder.
    bottom: last, but above a blank placeholder at the bottom. Returns its line number."""
    lines, eol = doc.lines, doc.eol
    its, start = doc.items(leaf)
    indent = item_parts(lines[its[0]])[0] if its else ""
    if after is not None:
        pos = after + 1
    elif place == "top":
        pos = (its[0] + 1 if is_blank(lines[its[0]]) else its[0]) if its else start
    else:
        stop = doc.next_heading(leaf.i)
        last = max((j for j in range(start, stop) if lines[j].strip()), default=None)
        if last is None:
            pos = start
        else:
            m = ITEM_RE.match(lines[last].rstrip("\r\n"))
            pos = last if (m and m.group(3) == "") else last + 1
            indent = item_parts(lines[its[-1]])[0] if its else ""
    lines.insert(pos, f"{indent}- [ ] {text}{eol}")
    return pos


def add_deadline(doc, heading_text, date):
    lines, eol = doc.lines, doc.eol
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


def log_state(doc, i, old_kw, now):
    """Add '- State "DONE" from "<old>" [ts]' to the LOGBOOK, as Emacs does."""
    lines, eol = doc.lines, doc.eol
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


def set_last_repeat(doc, i, now):
    lines, eol = doc.lines, doc.eol
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


def mark_done(doc, h, now):
    """Complete heading h the way Emacs does. Returns the next date for a repeater, else None."""
    lines, eol = doc.lines, doc.eol
    i, today = h.i, now.date()
    plan = i + 1 if i + 1 < len(lines) and PLANNING_RE.match(lines[i + 1]) else None
    repeat = plan is not None and REPEAT_RE.search(lines[plan])
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
        log_state(doc, i, h.kw, now)
        set_last_repeat(doc, i, now)
    else:
        lines[i] = re.sub(rf"^(\*+\s+){re.escape(h.kw)}\b", r"\1DONE", lines[i], count=1)
        if plan is not None:
            lines[plan] = f"CLOSED: {stamp(now)} " + lines[plan].lstrip()
        else:
            lines.insert(i + 1, f"CLOSED: {stamp(now)}{eol}")
        log_state(doc, i, h.kw, now)
    return next_due


def deadline_of(doc, h):
    j = h.i + 1
    if j < len(doc.lines) and PLANNING_RE.match(doc.lines[j]):
        m = re.search(r"DEADLINE:\s*" + STAMP_RE.pattern, doc.lines[j])
        if m:
            return dt.date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
    return None


def bill_name(title):
    t = LINK_RE.sub(" ", title)
    t = re.sub(r"\([^)]*\)", " ", t)
    return " ".join(URL_RE.sub(" ", t).split()).strip(" -") or title


# ---------- file I/O ----------

def load_config(path):
    cfg = copy.deepcopy(DEFAULT_CONFIG)
    if os.path.exists(path):
        with open(path, "rb") as f:
            user = tomllib.load(f)
        for k, v in user.items():
            if isinstance(v, dict) and isinstance(cfg.get(k), dict):
                cfg[k].update(v)
            else:
                cfg[k] = v
    cfg["dir"] = os.path.dirname(os.path.abspath(path))
    return cfg


def atomic_write(path, content):
    """Replace path in one step, keeping owner and permissions so Syncthing can sync it."""
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


# ---------- the engine ----------

class Engine:
    def __init__(self, cfg, state, now, state_dir=STATE_DIR):
        self.cfg, self.state, self.now = cfg, state, now
        self.filler = set(cfg["filler"])
        self.done_kws = set(cfg["done_keywords"])
        self.keywords = {norm(k): v for k, v in cfg["keywords"].items()}
        self.state_dir = state_dir
        self.docs = [Doc(os.path.join(cfg["dir"], f), cfg) for f in cfg["files"]
                     if os.path.exists(os.path.join(cfg["dir"], f))]
        if not self.docs:
            raise Reply("I couldn't find your org files.")
        self.what = None   # description of the change, for undo; None = nothing written
        self._index = self._keys = None

    # --- structure and settings ---

    def all_heads(self):
        return [h for d in self.docs for h in d.heads if not h.excluded]

    def reparse(self):
        for d in self.docs:
            d.parse()
        self._index = self._keys = None

    def setting(self, h, key):
        """Effective VOICE_* value at h: heading, ancestors, file #+PROPERTY:, engine default."""
        if key in LOCAL_KEYS:
            return h.props.get(key)
        n = h
        while n:
            if key in n.props:
                return n.props[key]
            n = n.parent
        if key in h.doc.file_props:
            return h.doc.file_props[key]
        if key == "VOICE_TYPE":
            return "checklist" if h.doc.items(h)[0] else "heading"
        if key == "VOICE_PLACE":
            return "bottom"
        if key == "VOICE_KEYWORD":
            return "TODO"
        if key == "VOICE_DEADLINE":
            return "never" if self.is_checklist(h) else "optional"
        return None

    def is_checklist(self, h):
        return self.setting(h, "VOICE_TYPE").lower() == "checklist"

    def picks(self, h):
        return (self.setting(h, "VOICE_PICK") or "").lower() == "child"

    def is_default(self, h):
        return (self.setting(h, "VOICE_DEFAULT") or "").lower() == "t"

    def live_kids(self, h):
        return [k for k in h.kids if not k.excluded and k.kw not in self.done_kws]

    def eligible(self, h):
        """Can voice name h as a place? Headings without a keyword, children of a
        VOICE_PICK heading, and anything with a VOICE_NAME."""
        if h.excluded or h.kw in self.done_kws:
            return False
        return h.kw is None or "VOICE_NAME" in h.props or bool(h.parent and self.picks(h.parent))

    def name_key(self, text):
        toks = [t[0] for t in spoken_tokens(text)[1]]
        while toks and toks[0] in self.filler:
            toks.pop(0)
        if len(toks) > 1 and toks[-1] == "list":
            toks.pop()
        return " ".join(toks)

    def own_keys(self, h):
        names = [h.title, h.title.replace("_", "")]
        names += [a for a in (h.props.get("VOICE_NAME") or "").split(",") if a.strip()]
        return {k for k in map(self.name_key, names) if k}

    def keys(self, h):
        """Every way to say h: its own names, and each ancestor's names before them."""
        if self._keys is None:
            self._keys = {}
        if id(h) not in self._keys:
            own = self.own_keys(h)
            out = set(own)
            if h.parent and not h.parent.excluded:
                out |= {f"{p} {o}" for p in self.keys(h.parent) for o in own}
            self._keys[id(h)] = out
        return self._keys[id(h)]

    def index(self):
        if self._index is None:
            self._index = {}
            for h in self.all_heads():
                if self.eligible(h):
                    for k in self.keys(h):
                        self._index.setdefault(k, []).append(h)
        return self._index

    def default_target(self):
        heads = [h for h in self.all_heads() if self.is_default(h)]
        if not heads:
            raise Reply("I don't have a default list. Say where to put it.")
        return heads[0]

    def pick_one(self, heads):
        heads = list({id(h): h for h in heads}.values())
        if len(heads) == 1:
            return heads[0]
        defaults = [h for h in heads if self.is_default(h)]
        if len(defaults) == 1:
            return defaults[0]
        raise Ambiguous(heads)

    def by_id(self, ident):
        for h in self.all_heads():
            if h.props.get("ID") == ident:
                return h
        return None

    def resolve_ref(self, ref):
        cands = [h for d in self.docs if d.name == ref["file"] for h in d.heads
                 if h.path() == ref["path"] and not h.excluded]
        exact = [h for h in cands if h.line() == ref.get("line")]
        return (exact or cands or [None])[0]

    # --- finding the target in a sentence ---

    def find_target(self, text, prefix=True, suffix=True, need_rest=True):
        """Longest phrase at either end of text that names a place.
        suffix: '<rest> to/on/for/... <name>'; prefix: '<name> <rest>'.
        Returns (heads, remaining words) or ([], all words)."""
        words, toks = spoken_tokens(text)
        idx = self.index()
        found = []   # (tokens used, is_suffix, heads, word_lo, word_hi)
        if suffix:
            for p in range(1, len(toks)):
                if toks[p][0] not in PREPOSITIONS:
                    continue
                q = p + 1
                while q < len(toks) and toks[q][0] in self.filler:
                    q += 1
                e = len(toks)
                if e - q > 1 and toks[e - 1][0] == "list":
                    e -= 1
                key = " ".join(t[0] for t in toks[q:e])
                if key in idx:
                    found.append((e - q, True, idx[key], toks[p][1], len(words)))
                    break
        if prefix:
            q = 0
            while q < len(toks) and toks[q][0] in self.filler:
                q += 1
            for e in range(min(len(toks), q + MAX_NAME_TOKENS), q, -1):
                if e < len(toks) and toks[e][1] == toks[e - 1][2]:
                    continue   # don't split a word
                key = " ".join(t[0] for t in toks[q:e])
                if key in idx:
                    hi = toks[e - 1][2] + 1
                    if e < len(toks) and toks[e][0] == "list":
                        hi = toks[e][2] + 1
                    if not need_rest or hi < len(words):
                        found.append((e - q, False, idx[key], 0, hi))
                    break
        if not found:
            found = self.fuzzy_target(words, toks, prefix, suffix, need_rest)
        if not found:
            return [], words
        n, _, heads, lo, hi = max(found, key=lambda f: (f[0], f[1]))
        return heads, words[:lo] + words[hi:]

    def fuzzy_target(self, words, toks, prefix, suffix, need_rest):
        """Small slips ("neo vim", "barrista assistant") for names of 6+ letters."""
        cutoff = self.cfg["match"]["name_cutoff"]
        keys = [k for k in self.index() if len(k) >= 6]
        best, best_score = [], cutoff
        def consider(span_key, entry):
            nonlocal best, best_score
            for k in keys:
                if abs(len(k.split()) - len(span_key.split())) > 1:
                    continue
                s = difflib.SequenceMatcher(None, span_key, k).ratio()
                if s >= best_score:
                    best, best_score = [entry(k)], s
        if suffix:
            for p in range(1, len(toks)):
                if toks[p][0] in PREPOSITIONS:
                    q = p + 1
                    while q < len(toks) and toks[q][0] in self.filler:
                        q += 1
                    key = " ".join(t[0] for t in toks[q:])
                    if key:
                        consider(key, lambda k, p=p, q=q: (len(toks) - q, True, self.index()[k],
                                                           toks[p][1], len(words)))
        if prefix:
            for e in range(1, min(len(toks), MAX_NAME_TOKENS) + 1):
                hi = toks[e - 1][2] + 1
                if need_rest and hi >= len(words):
                    break
                key = " ".join(t[0] for t in toks[:e])
                consider(key, lambda k, e=e, hi=hi: (e, False, self.index()[k], 0, hi))
        return best

    def lookup_name(self, text):
        """Headings a whole phrase names ("the costco list", "bills"), exact then fuzzy."""
        key = self.name_key(text)
        idx = self.index()
        if key in idx:
            return idx[key]
        stripped = re.sub(r"\s+(?:todo|to do|item|task)$", "", key)
        if stripped in idx:
            return idx[stripped]
        close = difflib.get_close_matches(key, idx.keys(), n=1, cutoff=self.cfg["match"]["name_cutoff"])
        return idx[close[0]] if close else []

    def match_choice(self, choices, text, anywhere=False):
        """The choice named at the start of text (or anywhere in it). Spoken numbers
        are understood ("three twenty two"). -> (choice, words used up to, ambiguous list)."""
        words, toks = spoken_tokens(text)
        table = {}
        for c in choices:
            keys = set(self.own_keys(c))
            if anywhere:   # answers may name the choice by its parent ("Inbox or Todos?")
                keys |= self.keys(c)
                if c.parent:
                    keys |= self.own_keys(c.parent)
                else:   # a top-level choice is offered by its file: "Garden-Planner or Tasks?"
                    keys.add(self.name_key(os.path.splitext(c.doc.name)[0]))
            for k in keys:
                table.setdefault(k, []).append(c)
        starts = range(len(toks)) if anywhere else [0]
        for s in starts:
            for e in range(min(len(toks), s + MAX_NAME_TOKENS), s, -1):
                key = " ".join(t[0] for t in toks[s:e])
                hits = list({id(c): c for c in table.get(key, [])}.values())
                if len(hits) == 1:
                    return hits[0], toks[e - 1][2] + 1, []
                if len(hits) > 1:
                    return None, toks[e - 1][2] + 1, hits
        if anywhere:   # the start of a name: "garnish" for "Garnish & Extras"
            for s in starts:
                for e in range(min(len(toks), s + MAX_NAME_TOKENS), s, -1):
                    key = " ".join(t[0] for t in toks[s:e])
                    hits = list({id(c): c for k, cs in table.items()
                                 if k.startswith(key + " ") for c in cs}.values())
                    if len(hits) == 1:
                        return hits[0], toks[e - 1][2] + 1, []
        cutoff = self.cfg["match"]["name_cutoff"]
        for s in starts:
            for k, cs in table.items():
                n = len(k.split())
                if len(k) >= 6 and s + n <= len(toks) and len(cs) == 1:
                    span = " ".join(t[0] for t in toks[s:s + n])
                    if difflib.SequenceMatcher(None, span, k).ratio() >= cutoff:
                        return cs[0], toks[s + n - 1][2] + 1, []
        return None, 0, []

    # --- lists ---

    def leaves(self, h):
        """Sections whose items count when h is used as a list. A heading with no
        items of its own and several children is one combined list of them all."""
        kids = self.live_kids(h)
        if h.doc.items(h)[0] or not kids:
            return [h]
        default = [k for k in kids if self.is_default(k)]
        if len(default) == 1:
            return self.leaves(default[0])
        if len(kids) == 1:
            return self.leaves(kids[0])
        return [x for k in kids for x in self.all_leaves(k)]

    def all_leaves(self, h):
        kids = self.live_kids(h)
        out = [h] if h.doc.items(h)[0] or not kids else []
        return out + [x for k in kids for x in self.all_leaves(k)]

    def subtree(self, h):
        out, todo = [], [h]
        while todo:
            n = todo.pop()
            if not n.excluded:
                out.append(n)
                todo.extend(n.kids)
        return out

    def find_items(self, leaves, text, checked=None):
        """(heading, line) pairs whose item matches text: exact, else the closest fuzzy."""
        keys = item_keys(text)
        cands = [(h, j) for h in leaves for j in h.doc.items(h)[0]
                 if not is_blank(h.doc.lines[j])
                 and (checked is None or is_checked(h.doc.lines[j]) == checked)]
        exact = [(h, j) for h, j in cands if keys & item_keys(item_parts(h.doc.lines[j])[2])]
        if exact:
            return exact, True
        best, best_score = [], self.cfg["match"]["item_cutoff"]
        for h, j in cands:
            score = max((difflib.SequenceMatcher(None, a, b).ratio()
                         for a in keys for b in item_keys(item_parts(h.doc.lines[j])[2])), default=0)
            if score > best_score:
                best, best_score = [(h, j)], score
            elif score == best_score and best:
                best.append((h, j))
        return best, False

    def find_entries(self, heads, text):
        """Open keyword headings whose title matches text: exact, else closest fuzzy."""
        keys = item_keys(text)
        cands = [h for h in heads if h.kw and h.kw not in self.done_kws]
        exact = [h for h in cands if keys & item_keys(say_text(h.title))]
        if exact:
            return exact, True
        best, best_score = [], self.cfg["match"]["item_cutoff"]
        for h in cands:
            score = max((difflib.SequenceMatcher(None, a, b).ratio()
                         for a in keys for b in item_keys(say_text(h.title))), default=0)
            if score > best_score:
                best, best_score = [h], score
            elif score == best_score and best:
                best.append(h)
        return best, False

    def catalog(self):
        cat = set()
        for d in self.docs:
            for line in d.lines:
                if ITEM_RE.match(line):
                    cat |= item_keys(item_parts(line)[2])
        for h in self.all_heads():
            if h.kw:
                cat |= item_keys(say_text(h.title))
        return cat

    # --- speech ---

    def label(self, h):
        """How replies name h: 'the Costco list', '322', 'Todos'."""
        if self.is_checklist(h):
            t = say_text(h.title)
            p = h.parent
            if p and (len(self.index().get(self.name_key(h.title), [])) > 1 or
                      (self.is_checklist(p) and not p.doc.items(p)[0])):
                t = f"{say_text(p.title)} {t}"   # "the Bar Stock list", "the Barista-Assistant Backlog list"
            return f"the {t}" if t.lower().endswith("list") else f"the {t} list"
        alias = [a.strip() for a in (h.props.get("VOICE_NAME") or "").split(",") if a.strip()]
        if alias and h.parent and self.picks(h.parent):
            return alias[0]
        return say_text(h.title)

    def which(self, heads):
        """'System or Grocery?': each choice by its parent, or by the file at the top."""
        def tag(h):
            return say_text(h.parent.title) if h.parent else os.path.splitext(h.doc.name)[0]
        tags = [tag(h) for h in heads]
        if len(set(tags)) < len(tags):
            tags = [" ".join(say_text(t) for t in h.path()) for h in heads]
        return f"{cap(join_words(tags, 'or'))}?"

    def ask_pick(self, verb, text, choices, question, say=""):
        self.state["pending"] = {"verb": verb, "text": text, "question": question,
                                 "choices": [c.ref() for c in choices],
                                 "time": self.now.isoformat()}
        return " ".join(x for x in (say, question) if x), True

    # --- capture / add ---

    def do_capture(self, text, forced=None, extra_date=None):
        if forced is not None:
            return self.capture_into(forced, text.split(), extra_date)
        heads, rest = self.find_target(text)
        if not heads:
            if not clean_title(text):
                raise Reply("Sorry, I didn't catch what to capture.")
            return self.capture_into(self.default_target(), text.split(), extra_date)
        try:
            target = self.pick_one(heads)
        except Ambiguous as a:
            return self.ask_pick("capture", " ".join(rest), a.heads, self.which(a.heads))
        return self.capture_into(target, rest, extra_date)

    def capture_into(self, target, words, extra_date=None):
        if not clean_title(" ".join(words)):
            raise Reply("Sorry, I didn't catch what to capture.")
        if self.is_checklist(target):
            return self.add_items(target, " ".join(words))
        if self.picks(target):
            lead = 1 if words and norm(words[0]) in ("for", "to", "in", "into", "under") else 0
            child, used, amb = self.match_choice(self.live_kids(target), " ".join(words[lead:]))
            if child is not None:
                return self.capture_into(child, words[lead + used:], extra_date)
            rest = words[lead + used:] if amb else words[lead:]
            if not clean_title(" ".join(rest)):
                raise Reply("Sorry, I didn't catch what to capture.")
            return self.ask_pick("capture", " ".join(rest), amb or self.live_kids(target), "Which one?")
        kw = self.setting(target, "VOICE_KEYWORD")
        for k in (2, 1):
            if len(words) > k and norm(" ".join(words[:k])) in self.keywords:
                kw, words = self.keywords[norm(" ".join(words[:k]))], words[k:]
                break
        deadline = self.setting(target, "VOICE_DEADLINE").lower()
        title, date = clean_title(" ".join(words)), None
        if deadline != "never":
            title, date = split_trailing_date(title, self.now.date())
            date = date or extra_date
        title = clean_title(title)
        if not title:
            raise Reply("Sorry, I didn't catch what to capture.")
        line = insert_heading(target.doc, target, kw, title, date)
        self.what = f"adding {title}"
        where = self.label(target)
        if date:
            return f"Added {title} to {where}, due {spoken_date(date)}.", False
        if deadline == "ask":
            self.state["last"] = {"file": target.doc.name, "heading": line, "title": title,
                                  "time": self.now.isoformat()}
            return f"Added {title} to {where}. When's it due?", True
        return f"Added {title} to {where}.", False

    def add_items(self, target, text):
        doc = target.doc
        reactivate = (self.setting(target, "VOICE_REACTIVATE") or "").lower() == "t"
        items = split_items(text, self.catalog()) if reactivate else [clean_title(text)]
        items = [i for i in items if i]
        if not items:
            raise Reply("Sorry, I didn't catch what to add.")
        leaves = self.leaves(target)
        added, already, unplaced, last = [], [], [], None
        for it in items:
            if reactivate:
                found, _ = self.find_items(leaves, it)
                if found:
                    h, j = found[0]
                    if is_checked(h.doc.lines[j]):
                        set_mark(h.doc.lines, j, " ")
                        added.append(it)
                    else:
                        already.append(it)
                    continue
            if len(leaves) > 1:   # combined list: don't guess the sub-list
                unplaced.append(it)
                continue
            last = insert_item(doc, leaves[0], it, self.setting(leaves[0], "VOICE_PLACE").lower(), last)
            added.append(it)
        where = self.label(leaves[0] if len(leaves) == 1 else target)
        msg = []
        if added:
            msg.append(f"Added {join_words(added)} to {where}.")
            self.what = f"adding {join_words(added)}"
        if already:
            verb = "is" if len(already) == 1 else "are"
            msg.append(f"{cap(join_words(already))} {verb} already on {where}.")
        if unplaced:
            return self.ask_pick("capture", ", ".join(unplaced), leaves,
                                 f"Which list should {join_words(unplaced)} go on?", " ".join(msg))
        return " ".join(msg), False

    # --- done / paid ---

    def do_done(self, text, forced=None):
        if forced is not None:   # the answer to "Inbox or Todos?"
            return self.complete([forced])
        heads, rest = self.find_target(text, prefix=False)
        scope = None
        if heads:
            try:
                scope = self.pick_one(heads)
            except Ambiguous as a:
                return self.ask_pick("done-in", " ".join(rest), a.heads, self.which(a.heads))
            text = " ".join(rest)
        return self.done_in(scope, text)

    def done_in(self, scope, text):
        pool = self.subtree(scope) if scope else self.all_heads()
        entries = [h for h in pool if h is not scope]
        pieces = split_items(text, self.catalog())
        if not pieces:
            raise Reply("Sorry, I didn't catch what to check off.")
        checked, already, missing, finish, ambiguous = {}, [], [], [], None
        for piece in pieces:
            items, items_exact = self.find_items(pool, piece)
            heads, heads_exact = self.find_entries(entries, piece)
            use_items = items and (items_exact or not heads_exact)
            if use_items:
                where = []
                for h, j in items:
                    if not is_checked(h.doc.lines[j]):
                        set_mark(h.doc.lines, j, "X")
                        where.append(self.label(h))
                if where:
                    checked.setdefault(join_words(dict.fromkeys(where)), []).append(piece)
                else:
                    already.append(piece)
            elif len(heads) == 1:
                finish.append(heads[0])
            elif heads:
                ambiguous = ambiguous or (piece, heads)
            else:
                missing.append(piece)
        msg = []
        if checked:
            parts = [f"{join_words(ps)} on {where}" for where, ps in checked.items()]
            msg.append(f"Checked off {join_words(parts)}.")
            self.what = "checking off " + join_words([p for ps in checked.values() for p in ps])
        if already:
            msg.append(f"{cap(join_words(already))} "
                       f"{'was' if len(already) == 1 else 'were'} already checked off.")
        if finish:
            say, _ = self.complete(finish)
            msg.append(say)
        if missing:
            where = f" on {self.label(scope)}" if scope else ""
            msg.append(f"I couldn't find {join_words(missing, 'or')}{where}.")
        if ambiguous:
            return self.ask_pick("done", ambiguous[0], ambiguous[1], self.which(ambiguous[1]),
                                 " ".join(msg))
        return " ".join(msg), False

    def complete(self, heads):
        """Mark headings done, bottom-up so line numbers stay valid."""
        said = []
        for h in sorted(heads, key=lambda h: (h.doc.name, -h.i)):
            name = say_text(bill_name(h.title) if h.kw == "BILL" else h.title)
            nxt = mark_done(h.doc, h, self.now)
            verb = "paid" if h.kw == "BILL" else "done"
            said.append(f"Marked {name} as {verb}." +
                        (f" The next one is due {spoken_date(nxt)}." if nxt else ""))
        self.what = "marking " + join_words([say_text(h.title) for h in heads]) + " done"
        self.reparse()
        return " ".join(reversed(said)), False

    def do_paid(self, text, forced=None):
        if forced is not None:
            return self.complete([forced])
        sp = re.sub(r"^(?:i\s+)?paid\s+", "", norm(text))
        sp = re.sub(r"^(?:the|my|our)\s+", "", sp)
        sp = re.sub(r"\s*\b(?:bill|bills|payment)$", "", sp).strip()
        if not sp:
            raise Reply("Which bill did you pay?")
        cands = [(norm(bill_name(h.title)), h) for h in self.all_heads() if h.kw == "BILL"]
        hits = [h for k, h in cands if k == sp]
        if not hits:
            hits = [h for k, h in cands if set(sp.split()) <= set(k.split())]
        if not hits:
            scored = [(difflib.SequenceMatcher(None, sp, k).ratio(), h) for k, h in cands]
            best = max((s for s, _ in scored), default=0)
            hits = [h for s, h in scored if s == best and s >= 0.8]
        if not hits:
            raise Reply(f"I couldn't find an unpaid {sp} bill.")
        return self.complete([min(hits, key=lambda h: deadline_of(h.doc, h) or dt.date.max)])

    # --- out ---

    def do_out(self, text):
        t = clean_title(text)
        m = re.match(r"^(?:(.*?)\s+(?:is|are|s)\s+|(?:we|we're|were|we are|i'm|im|i am)\s+)?"
                     r"(?:all\s+)?out\s+of\s+(.+)$", t, re.I)
        place = None
        if m:
            place, t = m.group(1), m.group(2)
        if place:
            heads = self.lookup_name(place)
            if not heads:
                raise Reply(f"I couldn't find {clean_title(place)}.")
            scope = self.pick_one(heads)
        else:
            heads, rest = self.find_target(t, prefix=False)
            scope = self.pick_one(heads) if heads else None
            if heads:
                t = " ".join(rest)
        if scope:   # "the bar": its linked lists (Bar > Stock), else the place itself
            linked = [h for h in self.subtree(scope) if "VOICE_LINK" in h.props]
            stock = [x for h in (linked or [scope]) for x in self.all_leaves(h)]
        else:       # "we're out of": every linked list
            stock = [x for h in self.all_heads() if "VOICE_LINK" in h.props
                     for x in self.all_leaves(h)]
        items = split_items(t, self.catalog())
        if not items:
            raise Reply("Sorry, I didn't catch what you're out of.")
        marked, back_on, already, missing = [], [], [], []
        for it in items:
            found, _ = self.find_items(stock, it)
            if not found:
                if scope is None:   # not a stock item: put it back on whatever list has it
                    again, _ = self.find_items(self.all_heads(), it)
                    if again:
                        h, j = again[0]
                        if is_checked(h.doc.lines[j]):
                            set_mark(h.doc.lines, j, " ")
                            back_on.append((it, self.label(h)))
                        else:
                            already.append((it, self.label(h)))
                        continue
                missing.append(it)
                continue
            h, j = found[0]
            stock_text = item_parts(h.doc.lines[j])[2]
            if is_checked(h.doc.lines[j]):
                set_mark(h.doc.lines, j, " ")
            marked.append(it)
            link = self.setting(h, "VOICE_LINK") or ""
            target = self.by_id(link[3:]) if link.startswith("id:") else None
            if target:   # only reactivate an entry that's already there; never add
                tl = self.leaves(target)
                g = self.find_items(tl, it)[0] or self.find_items(tl, stock_text)[0]
                if g and is_checked(g[0][0].doc.lines[g[0][1]]):
                    set_mark(g[0][0].doc.lines, g[0][1], " ")
                    back_on.append((it, self.label(g[0][0])))
        msg = []
        if marked:
            msg.append(f"Marked {join_words(marked)} as out.")
        for where in dict.fromkeys(w for _, w in back_on):
            these = [i for i, w in back_on if w == where]
            msg.append(f"{cap(join_words(these))} {'is' if len(these) == 1 else 'are'} back on {where}.")
        for where in dict.fromkeys(w for _, w in already):
            these = [i for i, w in already if w == where]
            msg.append(f"{cap(join_words(these))} {'is' if len(these) == 1 else 'are'} already on {where}.")
        if missing:
            msg.append(f"I couldn't find {join_words(missing, 'or')}.")
        if marked or back_on:
            self.what = "marking " + join_words(marked + [i for i, _ in back_on]) + " as out"
        return " ".join(msg), False

    # --- read ---

    def do_read(self, text, forced=None):
        if forced is not None:
            target = forced
        elif not norm(text):
            target = self.default_target()
        else:
            heads = self.lookup_name(text)
            if not heads:
                raise Reply(f"I couldn't find a {clean_title(text)} list.")
            try:
                target = self.pick_one(heads)
            except Ambiguous as a:
                return self.ask_pick("read", text, a.heads, self.which(a.heads))
        limit = self.cfg["read_limit"]
        name = self.label(target)
        if self.is_checklist(target):
            todo = [say_text(item_parts(h.doc.lines[j])[2])
                    for h in self.leaves(target) for j in h.doc.items(h)[0]
                    if not is_checked(h.doc.lines[j]) and not is_blank(h.doc.lines[j])]
            return self.speak_list(name, todo, limit), False
        entries = self.open_entries(target)
        bills = [h for h in self.subtree(target) if h.kw == "BILL"]
        if bills and any(h.kw == "BILL" for h in entries):   # "read bills": every unpaid bill, by date
            return self.speak_bills(bills), False
        return self.speak_list(name, [say_text(h.title) for h in entries], limit), False

    def open_entries(self, h):
        """Open keyword children; with none, those of keyword-less groups beneath."""
        kids = [k for k in h.kids if not k.excluded]
        direct = [k for k in kids if k.kw and k.kw not in self.done_kws]
        if direct:
            return direct
        return [e for k in kids if not k.kw for e in self.open_entries(k)]

    def speak_list(self, name, items, limit):
        if not items:
            return f"{cap(name)} is empty."
        more = f", and {len(items) - limit} more" if len(items) > limit else ""
        n = len(items)
        return f"{cap(name)} has {n} item{'s' if n != 1 else ''}: {', '.join(items[:limit])}{more}."

    def speak_bills(self, bills):
        today = self.now.date()
        bills = sorted(bills, key=lambda b: deadline_of(b.doc, b) or dt.date.max)
        parts = []
        for b in bills[:10]:
            d, name = deadline_of(b.doc, b), say_text(bill_name(b.title))
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

    # --- answers ---

    def fresh(self, entry, minutes=None):
        if not entry:
            return False
        t = dt.datetime.fromisoformat(entry["time"])
        return (self.now - t) <= dt.timedelta(minutes=minutes or self.cfg["follow_up_minutes"])

    def do_answer(self, text):
        print(f"voice answer heard: {text!r}", file=sys.stderr)   # shows in the HA trace
        t = re.sub(r"^(?:its|it is|it s|that s|thats)\s+", "", norm(text))
        if not t:   # silence: leave everything as it is, say nothing
            self.state.pop("pending", None)
            self.state.pop("last", None)
            return "", False
        pending = self.state.get("pending")
        if self.fresh(pending):
            choices = [c for c in map(self.resolve_ref, pending["choices"]) if c]
            choice, used, amb = self.match_choice(choices, text, anywhere=True)
            if choice is None:
                if amb:
                    pending["choices"] = [c.ref() for c in amb]
                raise Reply(pending["question"], ask=True)
            self.state.pop("pending", None)
            extra = find_date_anywhere(text.split()[used:], self.now.date()) if text.split()[used:] else None
            return self.run_verb(pending["verb"], pending["text"], forced=choice, extra_date=extra)
        last = self.state.get("last")
        if self.fresh(last):
            if has_phrase(t, NO_DEADLINE):
                self.state.pop("last", None)
                return "OK, no deadline.", False
            d = find_date_anywhere(t.split(), self.now.date())
            if d is None:
                raise Reply("When's it due?", ask=True)
            doc = next((d_ for d_ in self.docs if d_.name == last["file"]), None)
            if doc is None:
                raise Reply("I couldn't find that entry any more, so I didn't add a deadline.")
            add_deadline(doc, last["heading"], d)
            self.state.pop("last", None)
            self.what = f"adding a deadline to {last['title']}"
            return f"Got it. {last['title']} is due {spoken_date(d)}.", False
        raise Reply("There's nothing waiting for an answer.")

    def run_verb(self, verb, text, forced=None, extra_date=None):
        if verb == "capture":
            return self.do_capture(text, forced, extra_date)
        if verb == "done":
            return self.do_done(text, forced)
        if verb == "done-in":
            return self.done_in(forced, text)
        if verb == "paid":
            return self.do_paid(text, forced)
        if verb == "out":
            return self.do_out(text)
        if verb == "read":
            return self.do_read(text, forced)
        if verb == "answer":
            return self.do_answer(text)
        raise Reply("Sorry, I don't know how to do that.")

    # --- describe ---

    def describe(self):
        idx = self.index()
        out = []
        for d in self.docs:
            for h in d.heads:
                if h.excluded:
                    continue
                rules = {}
                for key in ("VOICE_TYPE", "VOICE_KEYWORD", "VOICE_DEADLINE", "VOICE_PICK",
                            "VOICE_PLACE", "VOICE_REACTIVATE", "VOICE_DEFAULT", "VOICE_LINK"):
                    v = self.setting(h, key)
                    if v is not None:
                        rules[key] = v
                own = sorted(self.own_keys(h)) if self.eligible(h) else []
                shared = sorted(k for k in self.keys(h) if len(idx.get(k, [])) > 1) \
                    if self.eligible(h) else []
                out.append({"file": d.name, "line": h.i + 1, "path": h.path(), "kw": h.kw,
                            "target": self.eligible(h), "names": own, "shared": shared,
                            "rules": rules})
        return out


# ---------- state, undo, entry point ----------

def load_json(path):
    try:
        with open(path) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def save_json(path, data):
    try:
        with open(path, "w") as f:
            json.dump(data, f)
    except OSError as e:
        print(f"voice: could not save {path}: {e!r}", file=sys.stderr)


def do_undo(cfg, state, state_dir):
    undo, saved = state.get("undo"), load_json(os.path.join(state_dir, ".voice_undo.json"))
    if not undo or not saved.get("files"):
        raise Reply("There's nothing to undo.")
    for path, h in saved["hashes"].items():
        with open(path, encoding="utf-8", newline="") as f:
            if sha(f.read()) != h:
                raise Reply(f"{os.path.basename(path)} has changed since then, so I won't undo it.")
    for path, old in saved["files"].items():
        atomic_write(path, old)
    for k in ("undo", "last", "pending"):
        state.pop(k, None)
    return f"Undone: {undo['what']}."


def do_cancel(cfg, state, state_dir, now):
    """'Scratch that': drop a waiting question, or undo a change from the last few minutes."""
    pending = state.pop("pending", None)
    fresh = lambda e, m: e and now - dt.datetime.fromisoformat(e["time"]) <= dt.timedelta(minutes=m)
    if fresh(pending, cfg["follow_up_minutes"]):   # nothing written yet
        state.pop("last", None)
        return "OK, cancelled."
    state.pop("last", None)
    undo = state.get("undo")
    if fresh(undo, cfg["undo_minutes"]):
        try:
            do_undo(cfg, state, state_dir)
            return f"Scratched that. I undid {undo['what']}."
        except Reply as r:
            return f"I couldn't undo it. {r.say}"
    if pending:
        return "OK, cancelled."
    return "There's nothing to cancel."


VERBS = {"capture": "capture", "add": "capture", "inbox": "capture",
         "done": "done", "check": "done", "complete": "done", "got": "done",
         "paid": "paid", "pay": "paid", "out": "out", "read": "read", "bills": "read",
         "answer": "answer", "undo": "undo", "cancel": "cancel"}


def run(verb, text, config=CONFIG_FILE, state_dir=STATE_DIR, now=None, dry_run=False):
    """-> (reply dict, {path: (old text, new text)} for files that changed)."""
    now = now or dt.datetime.now()
    cfg = load_config(config)
    state_file = os.path.join(state_dir, ".voice_state.json")
    state = load_json(state_file)
    changes = {}
    if verb == "inbox":
        text = f"{text} into the inbox"
    if verb == "bills":
        text = "bills"
    verb = VERBS.get(verb, verb)
    try:
        if verb == "answer" and has_phrase(re.sub(r"^(?:just|please|ok|okay)\s+", "", norm(text)), CANCEL):
            verb = "cancel"
        if verb == "cancel":
            return {"say": do_cancel(cfg, state, state_dir, now), "ask": False}, changes
        if verb == "undo":
            return {"say": do_undo(cfg, state, state_dir), "ask": False}, changes
        eng = Engine(cfg, state, now, state_dir)
        say, ask = eng.run_verb(verb, text)
        if eng.what:
            changes = {d.path: (d.text, d.new_text()) for d in eng.docs if d.new_text() != d.text}
        if changes and not dry_run:
            save_json(os.path.join(state_dir, ".voice_undo.json"),
                      {"files": {p: old for p, (old, _) in changes.items()},
                       "hashes": {p: sha(new) for p, (_, new) in changes.items()}})
            state["undo"] = {"what": eng.what, "time": now.isoformat()}
            for path, (_, new) in changes.items():
                atomic_write(path, new)
        return {"say": say, "ask": ask}, changes
    except Ambiguous as a:   # a place name with no follow-up flow ("the bar is out of ...")
        return {"say": f"Which one: {eng.which(a.heads)}", "ask": False}, {}
    except Reply as r:
        return {"say": r.say, "ask": r.ask}, {}
    finally:
        if not dry_run:
            save_json(state_file, state)


def describe_text(rows):
    out = []
    for r in rows:
        if not r["target"]:
            continue
        rules = ", ".join(f"{k[6:].lower()}={v}" for k, v in r["rules"].items())
        line = f"{r['file']}:{r['line']}  {' › '.join(r['path'])}\n    names: {', '.join(r['names'])}"
        out.append(line + f"\n    rules: {rules}")
        if r["shared"]:
            out.append(f"    shared with other headings: {', '.join(r['shared'])}")
    return "\n".join(out)


def main():
    args = sys.argv[1:]
    dry = "--dry-run" in args
    args = [a for a in args if a != "--dry-run"]
    if args and args[0] == "describe":
        cfg = load_config(CONFIG_FILE)
        rows = Engine(cfg, {}, dt.datetime.now()).describe()
        print(json.dumps(rows, indent=1) if "--json" in args else describe_text(rows))
        return
    args += ["", ""]
    verb, text = args[0].strip().lower(), args[1]
    print(f"voice: {verb} {text!r}", file=sys.stderr)
    lock = open(os.path.join(STATE_DIR, ".voice.lock"), "w")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX)
        out, changes = run(verb, text, dry_run=dry)
    except Exception as e:   # never crash the automation
        print(f"voice failed: {e!r}", file=sys.stderr)
        out, changes = {"say": "Sorry, something went wrong updating your lists.", "ask": False}, {}
    finally:
        lock.close()
    print(json.dumps(out))
    if dry:
        for path, (old, new) in changes.items():
            sys.stdout.writelines(difflib.unified_diff(
                old.splitlines(keepends=True), new.splitlines(keepends=True),
                f"a/{os.path.basename(path)}", f"b/{os.path.basename(path)}"))


if __name__ == "__main__":
    main()
