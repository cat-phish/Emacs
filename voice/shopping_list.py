#!/usr/bin/env python3
"""Voice shopping list for Home Assistant -> org-mode checkbox lists.

Usage:
  shopping_list.py add   "<items>" ["<list name>"]
  shopping_list.py check "<items>" ["<list name>"]
  shopping_list.py read  ""        ["<list name>"]
  shopping_list.py out   "<items>"

add    New items go at the top of the list, just below the blank "- [ ] "
       entry if the list starts with one (no blank is ever created). An item
       that's already on the list but checked off is unchecked in place
       instead of added again; an unchecked one is left alone.
check  Marks matching unchecked items [X]. With no list name it checks the
       item off on every list it appears on.
read   Reads back the unchecked items.
out    "The bar is out of X": unchecks X wherever it is under Bar > Stock and,
       only if X is already on Grocery > General, unchecks it there too.
       Never adds anything.

With no list name, or "shopping"/"grocery", the list is Grocery > General.
A name can be any heading in the file ("costco", "trader joe's", "amazon")
or parent + heading ("bar wishlist"). A heading with no items of its own
uses its "General" child; otherwise it acts as one combined list of all the
lists under it (new items then need a specific sub-list).

Always exits 0 and prints one sentence for the voice assistant to speak;
details of any failure go to stderr. Nothing here depends on Home Assistant,
so it can later be wrapped in a small web service.
"""
import difflib
import os
import re
import sys
import tempfile

SHOPPING_FILE = os.environ.get("SHOPPING_FILE", "/drives/local/org/main/Shopping.org")
DEFAULT_LIST = ("Grocery", "General")
STOCK_LIST = ("Bar", "Stock")
FUZZY_CUTOFF = 0.88  # how close a misheard item name must be to still match
DEFAULT_NAMES = {"", "shopping", "grocery", "groceries"}
IGNORE_TOP = {"inbox", "todos"}  # Tasks.org headings, if the lists share that file
READ_LIMIT = 20
NO_SPLIT = {"mac and cheese", "macaroni and cheese", "half and half"}

HEADING_RE = re.compile(r"^(\*+)\s+(.*?)\s*(?::[\w@#%:]+:)?\s*$")
PLANNING_RE = re.compile(r"^\s*(SCHEDULED|DEADLINE|CLOSED):")
ITEM_RE = re.compile(r"^(\s*)[-+]\s+\[([ xX-])\]\s?(.*?)\s*$")
URL_RE = re.compile(r"https?://\S+")


class Reply(Exception):
    """A message to speak instead of doing anything."""


# ---------- org structure ----------

def heading(line):
    m = HEADING_RE.match(line.rstrip("\r\n"))
    return (len(m.group(1)), m.group(2).strip().rstrip(":").strip()) if m else None


def body_start(lines, i):
    j = i + 1
    if j < len(lines) and PLANNING_RE.match(lines[j]):
        j += 1
    if j < len(lines) and lines[j].strip().upper() == ":PROPERTIES:":
        while j < len(lines) and lines[j].strip().upper() != ":END:":
            j += 1
        j += 1
    return j


def section_end(lines, i):
    return next((j for j in range(i + 1, len(lines)) if heading(lines[j])), len(lines))


def sections(lines):
    out, stack = [], []
    for i, line in enumerate(lines):
        h = heading(line)
        if not h:
            continue
        level, title = h
        while stack and stack[-1]["level"] >= level:
            stack.pop()
        parent = stack[-1] if stack else None
        sec = {"i": i, "level": level, "title": title, "parent": parent,
               "top": stack[0]["title"] if stack else title,
               # a heading starting with # (and everything under it) is off limits
               "excluded": title.startswith("#") or bool(parent and parent["excluded"])}
        stack.append(sec)
        if sec["top"].lower() not in IGNORE_TOP and not sec["excluded"]:
            out.append(sec)
    return out


def items_in(lines, i):
    start, end = body_start(lines, i), section_end(lines, i)
    return [j for j in range(start, end) if ITEM_RE.match(lines[j])], start


def item_parts(line):
    m = ITEM_RE.match(line.rstrip("\r\n"))
    return m.group(1), m.group(2), m.group(3)  # indent, mark, text


def is_blank(line):
    return item_parts(line)[2] == ""


def is_checked(line):
    return item_parts(line)[1] in "xX"


def set_mark(lines, j, mark):
    lines[j] = re.sub(r"\[[ xX-]\]", f"[{mark}]", lines[j], count=1)


# ---------- text matching ----------

def norm(s):
    s = URL_RE.sub(" ", s.lower()).replace("&", " and ")
    s = re.sub(r"[.'’]", "", s)
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return " ".join("and" if t == "n" else t for t in s.split())


def singular(t):
    if len(t) <= 3 or t.endswith("ss"):
        return t
    if t.endswith("ies"):
        return t[:-3] + "y"
    if t.endswith(("oes", "ses", "xes", "ches", "shes")):
        return t[:-2]
    return t[:-1] if t.endswith("s") else t


def item_key(s):
    n = re.sub(r"^(?:a|an|the|some|more)\s+", "", norm(s))
    return " ".join(singular(t) for t in n.split())


def item_keys(text):
    base = URL_RE.sub("", text).strip()
    keys = {item_key(base)}
    m = re.match(r"^(.*?)\s*\((.*?)\)\s*$", base)  # "mayo (dukes)" ~ "dukes mayo"
    if m:
        x, y = m.group(1), m.group(2)
        for q in {y, y.split(",")[0]}:  # "(Green, or sub)" also matches as "green"
            keys |= {item_key(f"{q} {x}"), item_key(f"{x} {q}")}
    keys.discard("")
    return keys


def find_items(lines, leaves, text):
    """(sec, line) pairs whose item matches text: exact, else closest fuzzy."""
    keys = item_keys(text)
    cands = [(sec, j) for sec in leaves for j in items_in(lines, sec["i"])[0]
             if not is_blank(lines[j])]
    exact = [(s, j) for s, j in cands if keys & item_keys(item_parts(lines[j])[2])]
    if exact:
        return exact
    best, best_score = [], FUZZY_CUTOFF
    for s, j in cands:
        score = max(difflib.SequenceMatcher(None, a, b).ratio()
                    for a in keys for b in item_keys(item_parts(lines[j])[2]))
        if score > best_score:
            best, best_score = [(s, j)], score
        elif score == best_score and best:
            best.append((s, j))
    return best


def list_key(s):
    k = re.sub(r"^(?:the|my|our)\s+", "", norm(s))
    k = re.sub(r"\bwish list\b", "wishlist", k)
    k = re.sub(r"\s+list$", "", k)
    return re.sub(r"\bwish$", "wishlist", k)


def clean_item(s):
    s = " ".join(s.split()).strip(" .!?,;:")
    return re.sub(r"^(?:some|more|the|a|an)\s+", "", s, flags=re.I)


def split_items(text, catalog):
    text = " ".join(text.split()).strip(" .!?,;:")
    out = []
    for part in (p for p in re.split(r"\s*[,;]\s*", text) if p):
        part = re.sub(r"^(?:and|also|plus)\s+", "", part, flags=re.I)
        key = item_key(part)
        if re.search(r"\s(?:and|&)\s", part, re.I) and key not in catalog \
                and key not in {item_key(x) for x in NO_SPLIT}:
            sub = [s for s in re.split(r"\s+(?:and|&)\s+", part, flags=re.I) if s.strip()]
            if len(sub) > 1 and len({item_key(s) for s in sub}) == len(sub):
                out.extend(sub)
                continue
        out.append(part)
    seen, items = set(), []
    for it in map(clean_item, out):
        if it and item_key(it) not in seen:
            seen.add(item_key(it))
            items.append(it)
    return items


def cap(s):
    return s[:1].upper() + s[1:]


def join_words(words, conj="and"):
    words = list(words)
    if len(words) <= 2:
        return f" {conj} ".join(words)
    return ", ".join(words[:-1]) + f", {conj} " + words[-1]


# ---------- list resolution ----------

def label(sec):
    t = sec["title"]
    if t.lower() == "general" and sec["parent"]:
        t = sec["parent"]["title"]
    elif t.lower() in ("stock", "wishlist") and sec["parent"]:
        t = f"{sec['parent']['title']} {t}"  # "the Bar Stock list"
    return f"the {t}" if t.lower().endswith("list") else f"the {t} list"


def find_path(secs, path):
    top, child = (x.lower() for x in path)
    for s in secs:
        if s["title"].lower() == child and s["parent"] and s["parent"]["title"].lower() == top:
            return s
    raise Reply(f"I couldn't find {' > '.join(path)} in your shopping file.")


def find_default(lines, secs):
    return descend(lines, secs, find_path(secs, DEFAULT_LIST))


def descend(lines, secs, sec):
    """-> (leaves, label_sec). Leaves are the sections whose items count."""
    its, _ = items_in(lines, sec["i"])
    kids = [s for s in secs if s["parent"] is sec]
    if its or not kids:
        return [sec], sec
    general = [k for k in kids if k["title"].lower() == "general"]
    if general or len(kids) == 1:
        return descend(lines, secs, (general or kids)[0])
    leaves = []
    for k in kids:  # combined list of everything underneath
        leaves += descend_all(lines, secs, k)
    return leaves, sec


def descend_all(lines, secs, sec):
    kids = [s for s in secs if s["parent"] is sec]
    out = [sec] if items_in(lines, sec["i"])[0] or not kids else []
    for k in kids:
        out += descend_all(lines, secs, k)
    return out


def resolve(lines, secs, name):
    """Return the section for a spoken list name, or None if there's no match."""
    key = list_key(name)
    if key in DEFAULT_NAMES:
        return find_default(lines, secs)
    table = {}
    for s in secs:
        keys = {list_key(s["title"])}
        if s["parent"]:
            keys.add(list_key(f"{s['parent']['title']} {s['title']}"))
        for k in keys:
            table.setdefault(k, []).append(s)
    hits = table.get(key)
    if not hits:
        close = difflib.get_close_matches(key, table.keys(), n=1, cutoff=0.8)
        hits = table[close[0]] if close else []
    hits = list({id(s): s for s in hits}.values())
    if len(hits) > 1:
        raise Reply(f"I found more than one {name} list: "
                    f"{join_words([label(s) for s in hits], 'or')}. Which one?")
    return descend(lines, secs, hits[0]) if hits else None


def split_suffix(lines, secs, text):
    """'eggs from costco' -> ('eggs', 'costco') when the tail names a list."""
    m = re.match(r"^(.*\S)\s+(?:from|on|off|to|at|for)\s+(.+?)$", text.strip(" .!?"), re.I)
    if m:
        try:
            if resolve(lines, secs, m.group(2)) is not None:
                return m.group(1), m.group(2)
        except Reply:
            return m.group(1), m.group(2)  # let the caller ask which one
    return text, ""


# ---------- operations ----------

def do_add(lines, eol, secs, items, name):
    fallback = None
    target = resolve(lines, secs, name) if name else find_default(lines, secs)
    if target is None:
        fallback, target = name, find_default(lines, secs)
    leaves, lsec = target
    added, already, unplaced, last = [], [], [], None
    for it in items:
        found = find_items(lines, leaves, it)
        if found:
            sec, j = found[0]
            if is_checked(lines[j]):
                set_mark(lines, j, " ")
                added.append(it)
            else:
                already.append(it)
            continue
        if len(leaves) > 1:  # combined list: don't guess the category
            unplaced.append(it)
            continue
        its, start = items_in(lines, leaves[0]["i"])
        indent = item_parts(lines[its[0]])[0] if its else ""
        if last is not None:
            pos = last + 1
        elif its and is_blank(lines[its[0]]):
            pos = its[0] + 1  # just under the blank entry
        elif its:
            pos = its[0]      # top of the list, no blank added
        else:
            pos = start
        lines.insert(pos, f"{indent}- [ ] {it}{eol}")
        last = pos
        added.append(it)
    msg = []
    if fallback:
        msg.append(f"There's no {fallback} list, so I added {join_words(added)} to {label(lsec)}."
                   if added else f"There's no {fallback} list.")
    elif added:
        msg.append(f"Added {join_words(added)} to {label(lsec)}.")
    if already:
        verb = "is" if len(already) == 1 else "are"
        msg.append(f"{cap(join_words(already))} {verb} already on {label(lsec)}.")
    if unplaced:
        names = join_words([l["title"] for l in leaves], "or")
        msg.append(f"Which list should {join_words(unplaced)} go on: {names}?")
    return " ".join(msg), bool(added)


def do_check(lines, secs, items, name):
    if name:
        target = resolve(lines, secs, name)
        if target is None:
            raise Reply(f"I couldn't find a {name} list.")
        targets, lsec = target
    else:
        targets, lsec = secs, None
    done, already, missing = [], [], []
    for it in items:
        where, seen_checked = [], False
        for sec, j in find_items(lines, targets, it):
            if is_checked(lines[j]):
                seen_checked = True
            else:
                set_mark(lines, j, "X")
                where.append(label(sec))
        if where:
            done.append(f"{it} on {join_words(dict.fromkeys(where))}")
        elif seen_checked:
            already.append(it)
        else:
            missing.append(it)
    msg = []
    if done:
        msg.append(f"Checked off {join_words(done)}.")
    if already:
        msg.append(f"{cap(join_words(already))} "
                   f"{'was' if len(already) == 1 else 'were'} already checked off.")
    if missing:
        msg.append(f"I couldn't find {join_words(missing, 'or')}"
                   f"{' on ' + label(lsec) if name else ''}.")
    return " ".join(msg), bool(done)


def do_read(lines, secs, name):
    target = resolve(lines, secs, name) if name else find_default(lines, secs)
    if target is None:
        raise Reply(f"I couldn't find a {name} list.")
    leaves, sec = target
    todo = [URL_RE.sub("", item_parts(lines[j])[2]).strip(" -")
            for leaf in leaves for j in items_in(lines, leaf["i"])[0]
            if not is_checked(lines[j]) and not is_blank(lines[j])]
    if not todo:
        return f"{cap(label(sec))} is empty."
    shown, more = todo[:READ_LIMIT], len(todo) - READ_LIMIT
    count = f"{len(todo)} item{'s' if len(todo) != 1 else ''}"
    tail = f", and {more} more" if more > 0 else ""
    return f"{cap(label(sec))} has {count}: {', '.join(shown)}{tail}."


def do_out(lines, secs, items):
    stock, _ = descend(lines, secs, find_path(secs, STOCK_LIST))
    grocery, gsec = find_default(lines, secs)
    marked, back_on, missing = [], [], []
    for it in items:
        found = find_items(lines, stock, it)
        if not found:
            missing.append(it)
            continue
        sec, j = found[0]
        stock_text = item_parts(lines[j])[2]
        if is_checked(lines[j]):
            set_mark(lines, j, " ")
        marked.append(it)
        # Grocery > General: only reactivate an entry that's already there.
        g = find_items(lines, grocery, it) or find_items(lines, grocery, stock_text)
        if g and is_checked(lines[g[0][1]]):
            set_mark(lines, g[0][1], " ")
            back_on.append(it)
    msg = []
    if marked:
        msg.append(f"Marked {join_words(marked)} as out at the bar.")
    if back_on:
        msg.append(f"{cap(join_words(back_on))} {'is' if len(back_on) == 1 else 'are'} "
                   f"back on {label(gsec)}.")
    if missing:
        msg.append(f"I couldn't find {join_words(missing, 'or')} in the bar stock.")
    return " ".join(msg), bool(marked)


# ---------- file I/O ----------

def read_file(path):
    with open(path, "r", encoding="utf-8", newline="") as f:
        lines = f.read().splitlines(keepends=True)
    eol = "\r\n" if lines and lines[0].endswith("\r\n") else "\n"
    if lines and not lines[-1].endswith(("\n", "\r")):
        lines[-1] += eol
    return lines, eol


def write_file(path, lines):
    """Atomic replace that keeps owner/permissions so Syncthing can still sync it."""
    st = os.stat(path)
    d, base = os.path.split(path)
    fd, tmp = tempfile.mkstemp(dir=d, prefix=f".syncthing.{base}.", suffix=".tmp")
    try:
        with os.fdopen(fd, "w", encoding="utf-8", newline="") as f:
            f.writelines(lines)
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


def run(op, item_text, name, path=SHOPPING_FILE):
    lines, eol = read_file(path)
    secs = sections(lines)
    name = (name or "").strip(" .!?")
    if op == "read":
        return do_read(lines, secs, name)
    if op not in ("add", "check", "out"):
        raise Reply("Sorry, I don't know how to do that with the shopping list.")
    if not name and op != "out":
        item_text, name = split_suffix(lines, secs, item_text)
    catalog = set().union(*(item_keys(item_parts(l)[2]) for l in lines if ITEM_RE.match(l)))
    items = split_items(item_text, catalog)
    if not items:
        raise Reply("Sorry, I didn't catch what to add." if op == "add"
                    else "Sorry, I didn't catch what to check off.")
    if op == "out":
        msg, changed = do_out(lines, secs, items)
    elif op == "add":
        msg, changed = do_add(lines, eol, secs, items, name)
    else:
        msg, changed = do_check(lines, secs, items, name)
    if changed:
        write_file(path, lines)
    return msg


def main():
    args = sys.argv[1:] + ["", "", ""]
    try:
        print(run(args[0].strip().lower(), args[1], args[2]))
    except Reply as r:
        print(r)
    except Exception as e:  # report, but never crash the automation
        print(f"shopping_list failed: {e!r}", file=sys.stderr)
        print("Sorry, I couldn't update the shopping list.")


if __name__ == "__main__":
    main()
