# Handoff: voice control for org files — current system and proposed redesign

## Who this is for
You're a Claude agent on the user's machine with access to their Emacs config and org files (`~/org/main/`), and possibly the server. This brief covers:

- what the voice system does today;
- the redesign we've agreed on so far;
- **the two configuration options (A and B)**, for the user to choose between.

The user's answers to **Decisions for the user** are recorded in §0. Implement in the order given under **Suggested order**.

## 0. Decisions (recorded 2026-11-20)
1. **Rules live in property drawers (option A)** and are inherited by the subtree. `VOICE_LINK` values are `id:<ID>`.
2. **Setting names and values stay as proposed** in §3. `VOICE_NAME`, `VOICE_PICK` and `VOICE_DEFAULT` apply only to the heading they are on. All other settings are inherited by the subtree.
3. **`VOICE_DEADLINE` defaults to `optional`** for keyword headings and `never` for checklists. Sections that should ask (School, for example) opt in with `ask`.
4. **Duplicate names** resolve as full spoken path, then the `VOICE_DEFAULT` target, then a short question.
5. **Engine-wide settings live in a hidden `~/org/main/.voice.toml`** (server: `/drives/local/org/main/.voice.toml`), read with stdlib `tomllib`. They are not in an org header. HA's shell command points at this file. Per-file defaults still use each file's `#+PROPERTY:` lines.
6. **Emacs helper: the full menu.** It is built in `init.org` (`** Voice Rules`, `SPC o v`). Its spoken-name view is a stand-in until the engine has `describe`.
7. **Runtime: keep `shell_command`**, standard library only. The FastAPI service stays optional for later.

Note: the live `Shopping.org` has its grocery items directly under `* Grocery`, with no `General` child, so examples below that say "Grocery › General" mean `* Grocery`.

---

## 1. How it works today

### The pieces
- **Hardware and speech.**
  - An Amazon Echo Dot 3 running [echo-dot-assist](https://github.com/Gamer92000/echo-dot-assist), set up in Home Assistant as an ESPHome voice satellite: `assist_satellite.echo_dot_3_e514bc_assist_satellite`.
  - Speech-to-text is Whisper (`small-int8`) and speech output is Piper, both running locally in the same Docker Compose stack as HA on an Unraid server. Nothing uses the cloud.
- **Org files.** They live in `/mnt/user/drives/local/org/main/` on the server, which is `/drives/local/org/main/` inside the HA container. Syncthing syncs them to the laptop (Emacs, at `~/org/main/`) and the phone (Orgzly Revived).
  - `Tasks.org`: tasks, the inbox, school, projects, system lists and bills.
  - `Shopping.org`: Grocery (General, Costco, Safeway, …), House, Amazon, Coffee, Bar (Stock, Wishlist), and wishlists.
- **Two Python scripts** (standard library only) in `/config/shell/` inside the HA container. HA calls them with `shell_command`, passing an operation and the spoken text.
  - **`shopping_list.py`**, with operations `add`, `check`, `read` and `out`.
  - **`tasks.py`**, with operations `capture`, `inbox`, `answer`, `paid`, `bills`, `read`, `undo` and `cancel`. It prints JSON `{"say": …, "ask": bool}`, where `ask` means the reply is a question waiting for an answer.
- **HA automations.** Each script has an automation of fixed phrase patterns ("capture {text}", "add {item} to the {name} list", "I paid {text}", …).
  - Each automation passes the operation and text to its script and speaks the reply.
  - When the shopping script can't find a list name, the shopping automation passes the read request on to `tasks.py`.
- **Follow-up questions** go through a separate HA script, **`script.tasks_follow_up`**.
  - It uses `assist_satellite.ask_question` and handles one question per run, in `queued` mode.
  - If a question gets no answer, it re-asks once, because the Echo sometimes speaks a question and then doesn't listen. After that it gives up quietly.
  - It allows at most three questions in a row.

### Behavior worth keeping (these are the user's rules)
- **Placement.**
  - In the **shopping lists**, new items go at the top, directly under a blank `- [ ]` placeholder if the list starts with one.
  - **Everywhere else**, new items and headings go at the **bottom** of their section, but above a blank placeholder that sits at the bottom.
  - **Never create a placeholder, and never remove one.**
- **No duplicates in shopping lists.** An existing checked item `[X]` is unchecked where it is, instead of being added again. An existing unchecked item is left alone, and the reply says it's already on the list.
- **Checking off.** Without a list name, an item is checked off on *every* list where it's open. With a name, only on that list.
- **"The bar is out of X".**
  - It unchecks X anywhere under Bar › Stock (all categories).
  - It *also* unchecks X on Grocery › General, but only if X is already there.
  - It never adds anything to either list.
- **Combined lists.** A heading whose items live in sub-headings (Bar › Stock with categories) works as one combined list for reading, checking and finding items. A *new* item there asks which sub-list to use.
- **New TODO-type headings ask "When's it due?"**
  - Answers like "due Friday" or "no deadline" are accepted. Silence leaves the item as it is, without a deadline.
  - The reply is spoken. Readbacks are kept short.
- **Assignments.**
  - "capture assignment …" asks "Which class?" if no class is given. The answer is matched to a course heading by number ("322", "three twenty two", "TA one twenty two") or by name ("misc").
  - If two classes share a number, it asks again.
  - Silence means nothing is written.
- **Paying bills.**
  - For a normal bill: `BILL` becomes `DONE`, `CLOSED: [ts]` is added at the front of the planning line, and the LOGBOOK gets `- State "DONE"       from "BILL"       [ts]` (Emacs's padded format).
  - For a repeating bill (`+1m`, `++1m`, `.+1m`): the keyword stays `BILL`, the date advances, `:LAST_REPEAT:` is set, and the LOGBOOK line is still added.
- **"Scratch that" / "undo that".**
  - During a question with nothing written yet, it drops the question.
  - Otherwise it undoes the last change made in the last 10 minutes.
  - Undo refuses if the file changed since (checked by hash).
- **Matching spoken text.**
  - Punctuation, case, hyphens and plurals are ignored.
  - A parenthetical can be said in either order, so `mayo (dukes)` matches "dukes mayo".
  - Fuzzy matching kicks in only when there's no exact match.
  - Number words become digits ("three twenty two" → 322).
  - The class or date is found **anywhere** in an answer, because Whisper sometimes includes the end of the Echo's own question.
- **Dates.** today, tomorrow, weekdays, "next Friday", "in two weeks", "the 30th", "November 22nd", "12/2", "end of the month".
- **Writing files.**
  - Writes are atomic: temp file `.syncthing.<name>.*.tmp`, then rename, keeping the owner and permissions.
  - Edit only the lines that need to change. Never reformat.
  - Never touch `:ID:`, other drawers or links.
- **Exclusions.** Headings whose title starts with `#`, and everything under them, are never read or written.
- **Folding** is done with `:VISIBILITY: folded` properties. It has no effect on matching.

### Known Emacs-side work already handed off separately
- **Folding after reverts.** Fold entries that became DONE when the buffer reverts from disk, without disturbing manual unfolds.
- **Auto-revert.** `global-auto-revert-mode` must stay on. If Emacs saves a stale buffer, it erases voice edits.

---

## 2. Problems with the current design
- **The script knows the structure by name.** It hardcodes `Inbox`, `Todos`/`General`, `School`, `Projects`, `Backlog`, `System`, `Bills`, `Repeaters`, `Grocery`/`General` and `Bar`/`Stock`, so restructuring `Tasks.org` breaks voice commands.
- **HA has one phrase family per feature.** The families overlap ("capture {task}" vs "capture assignment …"), which makes it unpredictable which automation wins. The shopping/tasks hand-off for reads is a workaround for exactly that.
- **Two scripts have two grammars,** with duplicated code and separate undo.

## 3. Agreed redesign (the user has approved these points)
1. **One engine for both files.** A single program reads `Tasks.org` and `Shopping.org` (and any others) with one grammar and one undo.
2. **Every heading is reachable by voice, unless excluded.** Exclusion is a `#` title or `:VOICE: off`; either covers the whole subtree.
3. **The org file describes its own voice rules.** Rules are attached to headings and inherited by everything beneath. **Where those rules are stored is the open decision (option A or B).**
4. **A few verbs instead of per-feature phrases.** HA needs only one trigger per verb, with the rest as free text:

   | Verb phrases | Action |
   |---|---|
   | "capture …", "add …" | write an item or heading |
   | "check off …", "done …", "I got …", "I paid …" | complete |
   | "… is out of …", "we're out of …" | un-complete, following links |
   | "what's on …", "read …", "what do I have to do" | read back |
   | answers, "scratch that", "undo that" | conversation control |

   The engine finds the target at **either end** of the text:
   - suffix: "add bagels **to the Costco list**", "testing **to the everything is computer backlog**", "X **into my inbox**";
   - prefix: "capture **system home lab** set up headscale".

   It takes the longest phrase that names a real heading. If nothing names one, it uses the default target.
5. **Name resolution.**
   - A heading's spoken names are its title (minus keyword, priority, statistics cookie, tags and a trailing `:`), any aliases, and "parent + title" paths ("bar stock", "grocery general").
   - Duplicate names are normal: `General` exists under Todos, System and Grocery. The engine prefers a spoken full path, then the default target, and otherwise asks a short question ("System or Grocery?").
6. **Tools for restructuring safely.**
   - `describe`: lists every reachable heading, its spoken names and its effective rules, and flags names that point to more than one heading.
   - `--dry-run "<phrase>"`: shows the diff a phrase would produce, without writing.
   - A test file of phrases and expected diffs, run against a copy of the org files.
7. **An Emacs helper for editing the voice rules** (see §5).

### Rule settings (names are a proposal; the user may rename them)

| Setting | Values | Replaces today's hardcoding |
|---|---|---|
| `VOICE_NAME` | comma-separated spoken aliases ("322, data structures") | course-number guessing, the "home lab"/"neovim" aliases |
| `VOICE_TYPE` | `checklist` \| `heading` | knowing that Backlog and System are checklists |
| `VOICE_KEYWORD` | a TODO keyword | TODO vs ASSIGNMENT vs CHORE logic |
| `VOICE_DEADLINE` | `ask` \| `optional` \| `never` | which captures ask "When's it due?" |
| `VOICE_PICK` | `child` | "Which class?" / "Which project?" |
| `VOICE_PLACE` | `top` \| `bottom` | the shopping-vs-tasks placement difference |
| `VOICE_REACTIVATE` | `t` | shopping's uncheck-instead-of-duplicate rule |
| `VOICE_DEFAULT` | `t` | "plain capture goes to Todos" |
| `VOICE_LINK` | a pointer to another heading | the hardcoded Bar Stock → Grocery rule |
| `VOICE` | `off` | per-heading exclusion without renaming |

**Inheritance.** The engine looks settings up when it reads the file and copies nothing into new headings. For a heading, it walks up through the ancestors and the nearest heading that sets the key wins. Then it uses the file's `#+PROPERTY:` line, and then the engine default. New subheadings, including ones made by voice or Orgzly, inherit at once.

**Exceptions:** `VOICE_NAME`, `VOICE_PICK` and `VOICE_DEFAULT` are never inherited:
- a class must not answer to School's aliases;
- `VOICE_PICK` on School must not make each class ask for a child;
- `VOICE_DEFAULT` on Todos must not make every child of Todos a default.

**Defaults when a heading has no settings:**
- `VOICE_TYPE` is guessed from content: checkbox items mean checklist, otherwise heading.
- `VOICE_PLACE` is `bottom`, and `VOICE_KEYWORD` is `TODO`.
- `VOICE_DEADLINE` is `optional` for keyword headings and `never` for checklists (decision 3).

**Completion is chosen by entry type:**
- a checkbox becomes `[X]`;
- a heading becomes `DONE` with a `CLOSED:` stamp and a LOGBOOK line;
- a repeating heading advances its date (the bill rules above).

"I paid …" can be limited to `BILL` headings with a setting or a verb rule.

---

## 4. The open decision: where the rules live

### Option A: in property drawers on the headings (the user is leaning this way)
```org
* School
:PROPERTIES:
:VOICE_NAME:     assignment, homework
:VOICE_PICK:     child
:VOICE_KEYWORD:  ASSIGNMENT
:VOICE_DEADLINE: ask
:END:
** CPT_S 322
:PROPERTIES:
:VOICE_NAME: 322, data structures
:END:

* Bar
** Stock
:PROPERTIES:
:VOICE_LINK: id:<ID of Grocery › General>
:END:
```
- **Rules sit on the heading they describe** and are inherited by everything beneath it. Inheritance means only section roots and the odd alias need a drawer.
- **Links use the target's `:ID:` as the value.** A name would break on rename, and `id:` is org's own link syntax, so `C-c C-o` jumps to the target.
- **Engine-wide settings** (files to read, filler words, matching cutoffs) go in the hidden `.voice.toml` next to the org files (decision 5):
  ```toml
  files  = ["Tasks.org", "Shopping.org"]
  filler = ["the", "my", "our", "some", "please"]
  [match]
  fuzzy_cutoff = 0.85
  ```
- **Per-file defaults** go in each file's `#+PROPERTY:` lines; for example `Shopping.org` sets `VOICE_PLACE top`, `VOICE_TYPE checklist` and `VOICE_REACTIVATE t`.
- **Outside org** there is only `.voice.toml` and its path in HA's shell command.
- **Value completion** in `org-set-property` (`C-c C-x p`) comes from `org-global-properties` in the Emacs config, so the org files need no `_ALL` lines.
- **Pros:**
  - Rules move with renames, refiles and copies.
  - They're visible and editable in Emacs and Orgzly.
  - They're synced along with the content.
  - Voice- and Orgzly-created headings need no IDs.
- **Cons:**
  - Drawers on more headings. They're folded by default, and Orgzly shows them only when a note is opened.
  - Engine-wide settings in file headers are a little awkward.

### Option B: a config file next to the engine, keyed by `:ID:`
```toml
[[target]]
id    = "3fc4…"            # CPT_S 322
names = ["322", "data structures"]

[[target]]
id    = "0913f9a6-…"       # System
pick  = "child"
type  = "checklist"

[engine]
files  = ["Tasks.org", "Shopping.org"]
filler = ["the", "my", "our", "some", "please"]
```
- **Pros:**
  - The org files stay clean.
  - All rules are in one place.
  - Renames are safe, because IDs don't change.
- **Cons:**
  - Headings without an ID can't be configured, and voice, Orgzly and some capture templates create headings without IDs.
  - Copy-pasting a heading duplicates its `:ID:`, so one rule matches two places.
  - Entries for deleted or archived headings silently dangle, and new sections need edits in a second file outside Emacs's org tools.
  - The comments naming each ID go stale.

### A hybrid, if the user wants one
Rules and aliases in drawers (as in A), cross-heading links stored as `id:` values in drawers, and only engine-wide settings in a small config file (as in B). The user has said they like A. Present the hybrid only as the alternative for engine-wide settings.

---

## 5. Emacs helper (needed for option A or the hybrid)
- **Value completion.** `#+PROPERTY: VOICE_*_ALL …` lines, or `org-global-properties` in the config, so `org-set-property` offers the allowed values.
- **A "Voice" command or menu (for example a transient) for the heading at point:**
  - show its effective `VOICE_*` settings, marking which ones are inherited and from where;
  - set or clear a setting through `org-set-property`;
  - pick a `VOICE_LINK` target from the list of headings, using `org-id-get-create` on the target if it has no ID;
  - toggle `:VOICE: off`;
  - show how voice addresses this heading: its spoken names, path and type. Ideally this comes from the engine's `describe` output so both sides agree.
- **Optional: one source of truth.** Build org-capture targets from the same `VOICE_*` properties, so Emacs capture and voice capture agree.
- **Check `org-use-property-inheritance`** for the `VOICE_*` keys if Emacs code reads them. The engine implements inheritance itself.

---

## 6. Bigger optional steps (for later)
- **Run the engine as a small service.** A FastAPI container in the HA Compose stack instead of `shell_command`. It allows proper file locking, keeps logs in one place, can serve a page for testing phrases, and other clients (phone shortcuts, Emacs) could call it.
- **A natural-language fallback.** A local AI model that turns free-form phrasing into the engine's structured request (action, target, text, date). The engine still validates everything. It needs capable hardware to respond quickly.

## 7. Suggested order
1. Merge the two scripts into one engine with the same behavior (§1). The user should notice no change.
2. Add generic target lookup and the verb grammar, then reduce HA to the verb triggers and the follow-up script.
3. Add rule settings from drawers and engine settings from `.voice.toml`, replacing the hardcoded names one section at a time.
4. Add `describe`, dry-run and the test file.
5. Build the Emacs helper. Done in `init.org`; swap its spoken-name view for `describe` output after step 4.
6. Optional: the service and the AI fallback.

Every step must keep the behavior in §1 unless the user changes it, and every step should come with a test run against copies of the org files, showing the diffs.

## 8. Decisions for the user
1. **Where do the rules live?** Option A (drawers, with engine settings in `#+PROPERTY:` headers), option B (a config file keyed by ID), or the hybrid?
2. **Setting names and values.** Are the `VOICE_*` names and values in §3 right, or should they change?
3. **Defaults.** Are the defaults in §3 right? In particular, should every keyword heading ask for a deadline by default?
4. **Duplicate names.** Is the order full path → default target → ask right?
5. **Which `#+PROPERTY:` file is the main one?** `Tasks.org`?
6. **Emacs helper scope.** A minimal command, or a full menu?
7. **The service.** Keep `shell_command` for now, or move to a FastAPI container as part of step 1?

## 9. Constraints
- **Test on copies.** Never test against the live synced files.
- **Org files stay Orgzly-compatible:** plain headings, checkboxes, drawers and timestamps only.
- **The engine runs inside the HA container,** so it must use only the Python standard library unless the user approves the service option.
- **The HA config changes go to the user as separate, labelled pieces:** `configuration.yaml`, automations and scripts. Pasting the wrong piece into the automation editor has already caused confusing errors.
- **Keep spoken replies short.** The user asked for less speech: no listing options unless asked.
