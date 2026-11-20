# Voice settings

Voice rules live in the Org files, as properties on the headings they describe. Inheritance means only the top of a section and the odd alias need a drawer:

```org
* School
:PROPERTIES:
:VOICE_PICK:     child
:VOICE_NAME:     assignment, homework
:VOICE_KEYWORD:  ASSIGNMENT
:VOICE_DEADLINE: ask
:END:
** CPT_S 322
:PROPERTIES:
:VOICE_NAME: 322
:END:
```

The easiest way to edit them is the Emacs menu, `SPC o v` (see [below](#the-emacs-menu-spc-o-v)). `C-c C-x p` also completes the allowed values.

## The settings

| Setting | Values | Default | Inherited | What it does |
|---|---|---|---|---|
| `VOICE_NAME` | comma-separated aliases | — | no | Extra spoken names: `322`, `shopping, groceries`. A heading with a `VOICE_NAME` can be named as a place even if it has a TODO keyword. |
| `VOICE_TYPE` | `checklist`, `heading` | `checklist` if the heading has checkbox items, else `heading` | yes | Whether captures become `- [ ]` items or child headings |
| `VOICE_KEYWORD` | a TODO keyword | `TODO` | yes | Keyword for captured headings. A spoken keyword ("chore …") overrides it. |
| `VOICE_DEADLINE` | `ask`, `optional`, `never` | `optional` for headings, `never` for checklists | yes | `ask`: ask "When's it due?" if no date was said. `optional`: take a date if one was said. `never`: don't look for dates, so "…due Friday" stays in the title. |
| `VOICE_PICK` | `child` | — | no | The heading's children are the real places (classes, projects). Naming the heading alone asks "Which one?". Children of a `VOICE_PICK` heading can always be named, even with a keyword (`ACTIVE Barista-Assistant`). |
| `VOICE_PLACE` | `top`, `bottom` | `bottom` | yes | Where new checkboxes go. `top` goes under a leading blank `- [ ]`; `bottom` goes above a trailing one. New headings always go at the bottom of the subtree. |
| `VOICE_REACTIVATE` | `t` | off | yes | Shopping-list behavior: split "eggs, milk and bread" into items, and uncheck an existing checked item instead of adding it again |
| `VOICE_DEFAULT` | `t` | — | no | Where captures go when no place is named, and the winner when a name fits several places. In a list of sub-lists, the child with `VOICE_DEFAULT` takes new items. |
| `VOICE_LINK` | `id:<ID>` | — | yes | For "out of": the list to also uncheck the item on, if it's there. The target needs an `:ID:`; the menu's `l` creates one. |
| `VOICE` | `off` | — | yes | Hides the heading and its subtree from voice, like a `#` at the start of the title |

**How a value is found.** The engine starts at the heading and walks up through its parents; the nearest one that sets the key wins. Next comes the file's `#+PROPERTY:` line, then the default above. Nothing is copied into new headings, so a heading made by voice, Orgzly or Emacs gets its section's rules straight away. The exceptions are `VOICE_NAME`, `VOICE_PICK` and `VOICE_DEFAULT`, which describe only their own heading:
- a class must not answer to School's aliases;
- `VOICE_PICK` on School must not make each class ask for a child;
- `VOICE_DEFAULT` on Todos must not make every task under it a default.

**Per-file defaults** go in `#+PROPERTY:` lines at the top of the file. `Shopping.org` uses:

```org
#+PROPERTY: VOICE_PLACE top
#+PROPERTY: VOICE_TYPE checklist
#+PROPERTY: VOICE_REACTIVATE t
```

## Which headings voice can name

A heading can be named as a place if it's visible (no `#` title, not under `:VOICE: off`), isn't `DONE` or `CANCELED`, and one of these is true:

- it has no TODO keyword (`* Inbox`, `** Costco`, `*** Backlog`);
- its parent has `VOICE_PICK child` (the projects under Projects);
- it has a `VOICE_NAME`.

This keeps "capture take out the trash" from filing a new task *under* an existing TODO called "take out the trash". "Done" and "paid" still find TODO headings by title; this rule is only about places.

**Its spoken names** are its title, minus keyword, priority, statistics cookie, tags and a trailing colon (plus a version without underscores: "cpts 317"), and its `VOICE_NAME` aliases. Each name can also be preceded by any of its parents' names: "bar stock", "system home lab", "school 322". When one name fits several places, the engine prefers a full path, then a `VOICE_DEFAULT` heading, and otherwise asks.

**Lists of sub-lists.** A checklist heading with no items of its own but with children acts as one combined list: for reading, checking off and "out of", it covers everything below. A new item goes to the child marked `VOICE_DEFAULT`, or to the only child; otherwise the engine asks which sub-list. Bar › Stock (Well, Liquor, Mixers, …) and System (General, Home-Lab, …) work this way.

## Your files today

`describe` is the source of truth (see [Running and fixing it](operations.md#checking-names-and-rules)). As of 2026-11-20:

| Where | Settings |
|---|---|
| `Tasks.org` `* Todos` | `VOICE_DEFAULT t`, `VOICE_NAME to do, tasks` |
| `Tasks.org` `* School` | `VOICE_PICK child`, `VOICE_NAME assignment, homework`, `VOICE_KEYWORD ASSIGNMENT`, `VOICE_DEADLINE ask` |
| each class under School | `VOICE_NAME` with its course number (`317`, `322`, `415`, `TA 122, 122`); Misc has `misc, miscellaneous` |
| `Tasks.org` `* Projects` | `VOICE_PICK child` |
| `* Backlog`, `* System`, each project's `*** Backlog` | `VOICE_TYPE checklist` |
| `System › Home-Lab`, `System › Neovim` | `VOICE_NAME home lab, homelab` and `neo vim` |
| `Shopping.org` | the three `#+PROPERTY:` lines above |
| `Shopping.org` `* Grocery` | `VOICE_NAME shopping, groceries` |
| `Shopping.org` `Bar › Stock` | `VOICE_LINK` to `* Grocery` |

## Recipes

- **A new section that voice should file into:** just create the heading. It's reachable by its title at once and inherits its parent's rules.
- **A shopping-style list in `Tasks.org`:** set `VOICE_TYPE checklist`, `VOICE_REACTIVATE t` and, if new items should go first, `VOICE_PLACE top`.
- **A new class:** add it under School. Give it `VOICE_NAME <number>` so "for three twenty two" works, plus a name ("data structures") if you like.
- **Hide something from voice:** start its title with `#`, or set `VOICE off` (`SPC o v o`) to keep the title.
- **Make "the X list" work for a name speech to text keeps mishearing:** add the misheard form to `VOICE_NAME`.
- **A second stock list (the coffee shelf, say):** set `VOICE_LINK` on it, pointing at the shopping list it should restock.

## Engine settings: `.voice.toml`

Settings for the whole engine sit next to the Org files in `~/org/main/.voice.toml`. The file is hidden and synced. Every key is optional.

| Key | Default | What it is |
|---|---|---|
| `files` | `["Tasks.org", "Shopping.org"]` | Org files to read, relative to the `.voice.toml` |
| `filler` | `["the", "my", "our", "some", "please"]` | Words ignored in front of a place name |
| `keywords` | `{todo = "TODO", "to do" = "TODO", chore = "CHORE"}` | Spoken words at the start of a capture that set the TODO keyword |
| `todo_keywords` | `TODO ASSIGNMENT BILL CHORE MEETING NEXT PLANNING REVIEW HOLD READY ACTIVE` | Open keywords. **Keep in sync with `org-todo-keywords` in `init.org`**, or headings with a new keyword aren't recognised. |
| `done_keywords` | `["DONE", "CANCELED"]` | Closed keywords |
| `read_limit` | `15` | Items read out before "…and N more" |
| `follow_up_minutes` | `10` | How long a question waits for an answer |
| `undo_minutes` | `10` | How far back "scratch that" reaches |
| `[match] item_cutoff` | `0.88` | How close (0 to 1) a misheard item must be to still match |
| `[match] name_cutoff` | `0.85` | The same for place names (only names of 6+ letters) |

Two environment variables matter to Home Assistant. `VOICE_CONFIG` is the path of `.voice.toml` (default `/drives/local/org/main/.voice.toml`). `VOICE_STATE_DIR` is where the question, undo and lock files go (default: the engine's own directory).

## The Emacs menu (`SPC o v`)

Open it on any heading in an Org buffer. The header shows the heading's path, whether voice can reach it, and each setting's effective value with where it comes from (*here*, *from School*, *file default*, *engine default*).

| Key | Does |
|---|---|
| `s` | set a setting, completing the allowed values |
| `c` | clear a setting on this heading |
| `n` | add a spoken name to `VOICE_NAME` |
| `l` | link to another heading (`VOICE_LINK`), giving it an `:ID:` if needed |
| `o` | toggle `VOICE off` |
| `j` | jump to the heading a value comes from |
| `g` | follow the `VOICE_LINK` |
| `d` | show how voice addresses this heading. This runs the engine's `describe`, so it shows exactly what voice will match. It reads the saved files, so save first. |

The code is in `init.org` under `** Voice Rules`. It reads `voice/org_voice.py` from `user-emacs-directory`, which needs `python3` on the `PATH`. Without it, `d` falls back to an Emacs-side approximation.
