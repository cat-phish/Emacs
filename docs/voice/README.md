# Voice control for Org files

Talk to the Echo Dot in the kitchen and your Org files change: things get added to the shopping list, tasks are captured and checked off, bills are marked paid. Emacs and Orgzly pick up the changes through Syncthing.

Everything runs locally on the Unraid server. Nothing goes to the cloud.

| Page | Read it when you want to… |
|---|---|
| [What you can say](phrases.md) | use it: every phrase, the follow-up questions, undo |
| [Voice settings](settings.md) | change where things go or how they're written: the `VOICE_*` properties, `.voice.toml`, the `SPC o v` menu |
| [Running and fixing it](operations.md) | deploy a new version, test a change, find out why something didn't work |

## How the pieces fit together

```
Echo Dot ──► Whisper (speech to text) ──► Home Assistant automation
                                               │  verb + the rest of the sentence
                                               ▼
                                  shell_command.org_voice
                                               │
                                               ▼
                              org_voice.py ◄── .voice.toml (engine settings)
                                   │  reads VOICE_* rules from the Org files
                                   ▼
                     Tasks.org, Shopping.org  ──Syncthing──►  Emacs, Orgzly
                                   │
                                   ▼
                     {"say": …, "ask": …} ──► Piper (text to speech) ──► Echo Dot
```

1. **Speech.** The Echo Dot runs [echo-dot-assist](https://github.com/Gamer92000/echo-dot-assist) and appears in Home Assistant as `assist_satellite.echo_dot_3_e514bc_assist_satellite`. Whisper (`small-int8`) turns speech into text, and Piper speaks the reply. Both run in the same Docker Compose stack as Home Assistant.
2. **Home Assistant** only decides the *verb*: capture, done, paid, out, read, answer, undo or cancel. The automation has one trigger per verb and passes the rest of the sentence through untouched.
3. **The engine**, `voice/org_voice.py`, does everything else. It works out which heading the sentence names ("…to the Costco list", "system home lab…"), reads that heading's voice rules, edits the file and returns a short reply.
4. **The rules live in the Org files**, in `VOICE_*` properties on the headings. Restructuring `Tasks.org` doesn't break voice commands, because the engine has no heading names built in. A new section works by voice as soon as it exists, and it inherits its parent's rules.
5. **Follow-up questions** ("Which one?", "When's it due?") go through `script.voice_follow_up`, which asks on the Echo and sends the answer back to the engine.

## Where the files are

| What | In this repo | On the server |
|---|---|---|
| Engine | `voice/org_voice.py` | `/config/shell/org_voice.py` in the HA container |
| Engine settings | — | `~/org/main/.voice.toml`, synced; `/drives/local/org/main/.voice.toml` inside the HA container |
| Home Assistant config | `voice/ha/1-configuration.yaml`, `2-automation.yaml`, `3-script.yaml` | pasted into HA |
| Tests | `voice/tests/` | — |
| Emacs menu | `init.org` › `** Voice Rules` (`SPC o v`) | — |
| Org files | — | `/mnt/user/drives/local/org/main/` (`/drives/local/org/main/` in the container) |
| Old scripts (replaced) | `voice/shopping_list.py`, `voice/tasks.py` | `/config/shell/`, kept for rollback |

The design history and the decisions behind it are in [`HANDOFF-voice-org-engine.md`](../../HANDOFF-voice-org-engine.md).

## Ground rules the engine follows

- **It edits only the lines it has to.** It never reformats a file and never touches `:ID:`s, links or drawers it isn't changing.
- **Writes are atomic.** The engine writes a temp file named `.syncthing.<name>.*.tmp`, then renames it, keeping the owner and permissions, so Syncthing never syncs a half-written file.
- **It never creates or removes a blank `- [ ]` placeholder.**
- **Headings whose title starts with `#`, and everything under them, are invisible to voice.** So is any subtree marked `:VOICE: off`.
- **Replies are short.** It doesn't read out options unless you ask.
- **Emacs must keep `global-auto-revert-mode` on.** If a buffer has unsaved changes when voice edits the file, Emacs merges the edit in rather than overwriting it (see *Merge External Edits Into Unsaved Org Buffers* in `init.org`).
