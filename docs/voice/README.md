# Voice control for Org files

Talk to Home Assistant and your Org files change: things get added to the shopping list, tasks are captured and checked off, bills are marked paid. Emacs and Orgzly pick up the changes through Syncthing.

It works from anything that talks to Home Assistant's Assist: a voice satellite, the phone app, or typed text. Everything runs locally on the Unraid server, and nothing goes to the cloud.

| Page | Read it when you want to… |
|---|---|
| [What you can say](phrases.md) | use it: every phrase, the follow-up questions, undo |
| [Voice settings](settings.md) | change where things go or how they're written: the `VOICE_*` properties, `.voice.toml`, the `SPC o v` menu |
| [Running and fixing it](operations.md) | deploy a new version, test a change, find out why something didn't work |

## How the pieces fit together

```
Assist device ──► speech to text ──► Home Assistant automation
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
                     {"say": …, "ask": …} ──► text to speech ──► Assist device
```

1. **Speech** is Home Assistant's Assist pipeline, and nothing here depends on which device or engines it uses. Today that's Whisper (`small-int8`) for speech to text and Piper for text to speech, in the same Docker Compose stack as Home Assistant. The main device is an Echo Dot running [echo-dot-assist](https://github.com/Gamer92000/echo-dot-assist).
2. **Home Assistant** only decides the *verb*: capture, done, paid, out, read, answer, undo or cancel. The automation has one trigger per verb and passes the rest of the sentence through untouched.
3. **The engine**, `voice/org_voice.py`, does everything else. It works out which heading the sentence names ("…to the Costco list", "system home lab…"), reads that heading's voice rules, edits the file and returns a short reply.
4. **The rules live in the Org files**, in `VOICE_*` properties on the headings. Restructuring `Tasks.org` doesn't break voice commands, because the engine has no heading names built in. A new section works by voice as soon as it exists, and it inherits its parent's rules.
5. **Follow-up questions** ("Which one?", "When's it due?") are asked on the device that heard the command. On a voice satellite, `script.voice_follow_up` asks the question, listens, and sends the answer back to the engine. On a device without a satellite (the phone app, typed text), the question is the reply, and you answer it as a new command: "it's for 322", "it's due Friday".

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

## Why it's built this way

- **The rules live in the Org files**, rather than in a config file keyed by `:ID:`. That way they move with renames, refiles and copies, show up in Emacs and Orgzly, and work for headings without an ID, including ones Orgzly or voice just created.
- **Settings for the whole engine sit in `.voice.toml`**, not in an Org file header, so no Org file has to be the "main" one.
- **Home Assistant runs the engine through `shell_command`**, using only the Python standard library, so nothing extra has to be installed in the container. A small web service would give locking and logs in one place, but it isn't needed yet.

## Ground rules the engine follows

- **It edits only the lines it has to.** It never reformats a file and never touches `:ID:`s, links or drawers it isn't changing.
- **Writes are atomic.** The engine writes a temp file named `.syncthing.<name>.*.tmp`, then renames it, keeping the owner and permissions, so Syncthing never syncs a half-written file.
- **It never creates or removes a blank `- [ ]` placeholder.**
- **Headings whose title starts with `#`, and everything under them, are invisible to voice.** So is any subtree marked `:VOICE: off`.
- **Replies are short.** It doesn't read out options unless you ask.
- **Emacs must keep `global-auto-revert-mode` on.** If a buffer has unsaved changes when voice edits the file, Emacs merges the edit in rather than overwriting it (see *Merge External Edits Into Unsaved Org Buffers* in `init.org`).
