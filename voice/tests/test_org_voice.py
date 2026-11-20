"""Phrase tests for org_voice.py, run against copies of the fixture Org files.

    python3 -m unittest discover voice/tests          # run
    python3 voice/tests/test_org_voice.py --show      # print actual replies and diffs

Each case is a conversation: (verb, text) steps run in order on fresh copies at a
fixed time. It checks the reply to every step, and the lines the whole
conversation adds (+) and removes (-) in each file, in file order.
"""
import datetime as dt
import difflib
import os
import shutil
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
import org_voice  # noqa: E402

NOW = dt.datetime(2026, 11, 20, 10, 0)   # a Friday
FILES = ("Tasks.org", "Shopping.org")

CASES = [
    # --- capture: default target, keywords, dates ---
    ("plain capture goes to the default target and doesn't ask",
     [("capture", "call the landlord", "Added call the landlord to Todos.")],
     {"Tasks.org": ["+** TODO call the landlord"]}),
    ("a trailing date becomes a deadline",
     [("capture", "call the landlord due friday",
       "Added call the landlord to Todos, due Friday, November 27th.")],
     {"Tasks.org": ["+** TODO call the landlord", "+DEADLINE: <2026-11-27 Fri>"]}),
    ("a spoken keyword sets the TODO keyword",
     [("capture", "chore descale the kettle", "Added descale the kettle to Todos.")],
     {"Tasks.org": ["+** CHORE descale the kettle"]}),
    ("target as a suffix: into my inbox",
     [("capture", "buy stamps into my inbox", "Added buy stamps to Inbox.")],
     {"Tasks.org": ["+** TODO buy stamps"]}),
    ("target as a prefix, with a keyword after it",
     [("capture", "inbox chore take out recycling", "Added take out recycling to Inbox.")],
     {"Tasks.org": ["+** CHORE take out recycling"]}),
    ("the legacy inbox verb",
     [("inbox", "buy stamps", "Added buy stamps to Inbox.")],
     {"Tasks.org": ["+** TODO buy stamps"]}),

    # --- VOICE_PICK: classes ---
    ("assignment with a class and a date",
     [("capture", "assignment for 317 lab five due sunday",
       "Added lab five to 317, due Sunday, November 22nd.")],
     {"Tasks.org": ["+*** ASSIGNMENT lab five", "+DEADLINE: <2026-11-22 Sun>"]}),
    ("class by spoken number as a suffix, then VOICE_DEADLINE ask",
     [("capture", "lab six for three twenty two", "Added lab six to 322. When's it due?"),
      ("answer", "due next friday", "Got it. lab six is due Friday, December 4th.")],
     {"Tasks.org": ["+*** ASSIGNMENT lab six", "+DEADLINE: <2026-12-04 Fri>"]}),
    ("TA class by alias",
     [("capture", "homework for ta one twenty two grade quiz seven due monday",
       "Added grade quiz seven to TA 122, due Monday, November 23rd.")],
     {"Tasks.org": ["+*** ASSIGNMENT grade quiz seven", "+DEADLINE: <2026-11-23 Mon>"]}),
    ("no class: ask, write nothing, then the answer picks it",
     [("capture", "assignment read chapter four", "Which one?"),
      ("answer", "which class 322 due tomorrow",
       "Added read chapter four to 322, due Saturday, November 21st.")],
     {"Tasks.org": ["+*** ASSIGNMENT read chapter four", "+DEADLINE: <2026-11-21 Sat>"]}),
    ("an unknown answer asks again",
     [("capture", "assignment read chapter four", "Which one?"),
      ("answer", "biology", "Which one?")],
     {}),
    ("no deadline",
     [("capture", "lab seven for 322", "Added lab seven to 322. When's it due?"),
      ("answer", "no deadline", "OK, no deadline.")],
     {"Tasks.org": ["+*** ASSIGNMENT lab seven"]}),
    ("silence leaves the entry without a deadline and says nothing",
     [("capture", "lab seven for 322", "Added lab seven to 322. When's it due?"),
      ("answer", "", "")],
     {"Tasks.org": ["+*** ASSIGNMENT lab seven"]}),

    # --- checklists in Tasks.org ---
    ("path name: system home lab, above the bottom placeholder",
     [("capture", "system home lab set up headscale", "Added set up headscale to the System Home-Lab list.")],
     {"Tasks.org": ["+- [ ] set up headscale"]}),
    ("project backlog by path",
     [("capture", "garden planner backlog compost tracker",
       "Added compost tracker to the Garden-Planner Backlog list.")],
     {"Tasks.org": ["+- [ ] compost tracker"]}),
    ("project heading with the todo word",
     [("capture", "garden planner todo fix the frost dates",
       "Added fix the frost dates to Garden-Planner.")],
     {"Tasks.org": ["+*** TODO fix the frost dates"]}),
    ("an empty checklist heading (VOICE_TYPE checklist), reached through a shared name",
     [("capture", "learn rust to the backlog", "Garden-Planner or Tasks?"),
      ("answer", "tasks", "Added learn rust to the Backlog list.")],
     {"Tasks.org": ["+- [ ] learn rust"]}),
    ("a combined list asks which sub-list",
     [("capture", "clean desk to system", "Which list should clean desk go on?"),
      ("answer", "general", "Added clean desk to the System General list.")],
     {"Tasks.org": ["+- [ ] clean desk"]}),
    ("a shared name asks which one",
     [("add", "honey to general", "System or Grocery?"),
      ("answer", "grocery", "Added honey to the Grocery General list.")],
     {"Shopping.org": ["+- [ ] honey"]}),
    ("VOICE: off hides a heading",
     [("read", "secret", "I couldn't find a secret list.")],
     {}),

    # --- shopping ---
    ("new items go at the top, under the placeholder",
     [("add", "bagels to the costco list", "Added bagels to the Costco list.")],
     {"Shopping.org": ["+- [ ] bagels"]}),
    ("several items, a checked one reactivated, a known 'and' kept whole",
     [("add", "string cheese, limes and mac and cheese to my shopping list",
       "Added string cheese, limes, and mac and cheese to the Grocery list.")],
     {"Shopping.org": ["+- [ ] string cheese", "-- [X] limes", "-- [X] mac & cheese",
                       "+- [ ] limes", "+- [ ] mac & cheese"]}),
    ("an open item isn't added twice; parentheticals match either way",
     [("add", "bread and dukes mayo to the grocery list",
       "Added dukes mayo to the Grocery list. Bread is already on the Grocery list.")],
     {"Shopping.org": ["-- [X] mayo (dukes)", "+- [ ] mayo (dukes)"]}),
    ("adding to a combined list asks which sub-list",
     [("add", "rye to bar stock", "Which list should rye go on?"),
      ("answer", "well", "Added rye to the Stock Well list.")],
     {"Shopping.org": ["+- [ ] rye"]}),

    # --- done ---
    ("check off with no list: every list it's open on",
     [("done", "eggs and butter", "Checked off eggs and butter on the Costco list.")],
     {"Shopping.org": ["-- [ ] eggs", "-- [ ] butter", "+- [X] eggs", "+- [X] butter"]}),
    ("check off on a named list",
     [("done", "dish soap off the house list", "Checked off dish soap on the House list.")],
     {"Shopping.org": ["-- [ ] dish soap", "+- [X] dish soap"]}),
    ("a heading becomes DONE with CLOSED and a LOGBOOK line",
     [("done", "change bank password", "Marked Change bank password as done.")],
     {"Tasks.org": ["-** TODO Change bank password", "-DEADLINE: <2026-11-24 Tue>",
                    "+** DONE Change bank password",
                    "+CLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-11-24 Tue>",
                    "+:LOGBOOK:", '+- State "DONE"       from "TODO"       [2026-11-20 Fri 10:00]',
                    "+:END:"]}),
    ("a repeating heading advances",
     [("done", "water the garden",
       "Marked Water the garden as done. The next one is due Sunday, November 29th.")],
     {"Tasks.org": ["-DEADLINE: <2026-11-22 Sun +1w>", "+DEADLINE: <2026-11-29 Sun +1w>",
                    "+:PROPERTIES:", "+:LAST_REPEAT: [2026-11-20 Fri 10:00]", "+:END:",
                    "+:LOGBOOK:", '+- State "DONE"       from "TODO"       [2026-11-20 Fri 10:00]',
                    "+:END:"]}),
    ("an unknown item",
     [("done", "unicorn tears", "I couldn't find unicorn tears.")],
     {}),

    # --- paid ---
    ("paying a repeating bill keeps BILL and moves the date",
     [("paid", "the power bill", "Marked Power as paid. The next one is due Saturday, December 26th.")],
     {"Tasks.org": ["-DEADLINE: <2026-11-26 Thu +1m>", "+DEADLINE: <2026-12-26 Sat +1m>",
                    "+:PROPERTIES:", "+:LAST_REPEAT: [2026-11-20 Fri 10:00]", "+:END:",
                    "+:LOGBOOK:", '+- State "DONE"       from "BILL"       [2026-11-20 Fri 10:00]',
                    "+:END:"]}),
    ("paying a one-off bill",
     [("paid", "internet", "Marked Internet as paid.")],
     {"Tasks.org": ["-*** BILL Internet ($50)", "-DEADLINE: <2026-12-08 Tue>",
                    "+*** DONE Internet ($50)",
                    "+CLOSED: [2026-11-20 Fri 10:00] DEADLINE: <2026-12-08 Tue>",
                    "+:LOGBOOK:", '+- State "DONE"       from "BILL"       [2026-11-20 Fri 10:00]',
                    "+:END:"]}),
    ("bills under a # heading are invisible",
     [("paid", "secret bill", "I couldn't find an unpaid secret bill.")],
     {}),

    # --- out ---
    ("out at the bar: uncheck there, and on the linked list only if it's already there",
     [("out", "the bar is out of bourbon and limes",
       "Marked bourbon and limes as out. Limes is back on the Grocery list.")],
     {"Shopping.org": ["-- [X] limes", "+- [ ] limes", "-- [X] Bourbon", "-- [X] Limes",
                       "+- [ ] Bourbon", "+- [ ] Limes"]}),
    ("we're out of: linked lists first, by parenthetical",
     [("out", "we're out of fever tree tonic", "Marked fever tree tonic as out.")],
     {"Shopping.org": ["-- [X] Tonic (Fever Tree)", "+- [ ] Tonic (Fever Tree)"]}),
    ("we're out of something not stocked: back on its list, never added",
     [("out", "we're out of mac and cheese", "Mac and cheese is back on the Grocery list.")],
     {"Shopping.org": ["-- [X] mac & cheese", "+- [ ] mac & cheese"]}),
    ("out never adds",
     [("out", "the bar is out of gin", "I couldn't find gin.")],
     {}),

    # --- read ---
    ("read the default target",
     [("read", "", "Todos has 3 items: take out the trash, Change bank password, Sharpen knives.")],
     {}),
    ("read a combined list",
     [("read", "bar stock", "The Bar Stock list has 1 item: Coke.")],
     {}),
    ("read bills: every unpaid bill by date, # headings left out",
     [("read", "bills", "You have 2 unpaid bills: Power, due Thursday, November 26th; "
                        "Internet, due Tuesday, December 8th.")],
     {}),
    ("read by alias",
     [("read", "my to do list",
       "Todos has 3 items: take out the trash, Change bank password, Sharpen knives.")],
     {}),

    # --- undo / cancel ---
    ("undo the last change",
     [("capture", "fix bike", "Added fix bike to Todos."),
      ("undo", "", "Undone: adding fix bike.")],
     {}),
    ("scratch that during a question drops it",
     [("capture", "assignment read chapter four", "Which one?"),
      ("answer", "scratch that", "OK, cancelled.")],
     {}),
    ("scratch that after a capture undoes it",
     [("capture", "fix bike", "Added fix bike to Todos."),
      ("answer", "scratch that", "Scratched that. I undid adding fix bike.")],
     {}),
]


def changed_lines(old, new):
    return [l for l in difflib.unified_diff(old.splitlines(), new.splitlines(), lineterm="", n=0)
            if l[:1] in "+-" and not l.startswith(("+++", "---"))]


def run_case(steps):
    work, state = tempfile.mkdtemp(), tempfile.mkdtemp()
    try:
        for f in FILES + (".voice.toml",):
            shutil.copy(os.path.join(HERE, "fixtures", f), work)
        before = {f: open(os.path.join(work, f)).read() for f in FILES}
        says = []
        for i, (verb, text, _) in enumerate(steps):
            out, _ = org_voice.run(verb, text, config=os.path.join(work, ".voice.toml"),
                                   state_dir=state, now=NOW + dt.timedelta(seconds=i))
            says.append(out["say"])
        after = {f: open(os.path.join(work, f)).read() for f in FILES}
        diffs = {f: changed_lines(before[f], after[f]) for f in FILES if before[f] != after[f]}
        return says, diffs
    finally:
        shutil.rmtree(work)
        shutil.rmtree(state)


class Phrases(unittest.TestCase):
    pass


def make_test(steps, expected):
    def test(self):
        says, diffs = run_case(steps)
        self.assertEqual(says, [s[2] for s in steps])
        self.assertEqual(diffs, expected)
    return test


for n, (name, steps, expected) in enumerate(CASES):
    setattr(Phrases, f"test_{n:02d}_" + "_".join(name.split()[:6]).replace(":", ""),
            make_test(steps, expected))


class Describe(unittest.TestCase):
    def test_describe_flags_shared_names(self):
        cfg = org_voice.load_config(os.path.join(HERE, "fixtures", ".voice.toml"))
        rows = org_voice.Engine(cfg, {}, NOW).describe()
        general = [r for r in rows if r["path"][-1] == "General"]
        self.assertEqual(len(general), 2)
        self.assertTrue(all("general" in r["shared"] for r in general))
        self.assertFalse(any(r["path"][-1] == "Secret" for r in rows))


if __name__ == "__main__":
    if "--show" in sys.argv:
        for name, steps, expected in CASES:
            says, diffs = run_case(steps)
            ok = says == [s[2] for s in steps] and diffs == expected
            print(f"{'ok  ' if ok else 'FAIL'} {name}")
            if not ok:
                for (v, t, want), got in zip(steps, says):
                    print(f"     {v} {t!r}\n       want: {want}\n       got:  {got}")
                print(f"     diff want: {expected}\n     diff got:  {diffs}")
    else:
        unittest.main()
