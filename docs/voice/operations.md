# Running and fixing it

## Deploying a new engine version

1. Copy `voice/org_voice.py` to `/config/shell/` in the Home Assistant container. Nothing else needs a restart: `shell_command` starts the script fresh every time.
2. Check it on the server without writing anything:
   ```sh
   python3 /config/shell/org_voice.py --dry-run capture "lab five for 322"
   ```
   This prints the reply, then the diff it would have written.

The engine needs Python 3.11 or newer (for `tomllib`) and nothing outside the standard library.

## Setting up Home Assistant (once)

The config comes as three separate pieces. Each goes in a different place, and pasting one into the wrong editor causes confusing errors.

| Piece | Goes in | Then |
|---|---|---|
| `voice/ha/1-configuration.yaml` | `configuration.yaml` | restart Home Assistant |
| `voice/ha/3-script.yaml` | Settings › Scripts › new script › Edit in YAML | check the entity is `script.voice_follow_up` |
| `voice/ha/2-automation.yaml` | Settings › Automations › new automation › Edit in YAML | disable the old *Tasks* and shopping-list automations |

Add the script before the automation, because the automation calls it.

The pieces are based on the old *Tasks* automation and *Tasks follow-up* script, which already worked on the Echo Dot. The follow-up script's timing has been kept: it waits for the satellite to go idle, then pauses for a second before asking.

**Nothing is tied to one device.** The automation looks up the voice satellite that heard the command (`device_entities(trigger.device_id)`) and passes it to the script, so a question is asked on the same device. A command from the phone app or typed text has no satellite, so the question comes back as the reply instead. A new satellite needs no changes.

**Rolling back:** disable *Voice - Org files* and re-enable the old automations. The old `shell_command.tasks`, `tasks.py` and `shopping_list.py` are untouched. Note that the old shopping script fails on the current `Shopping.org`: it still looks for `Grocery › General`.

## Checking names and rules

`describe` lists every heading voice can name, with its spoken names, effective rules, and any name it shares with another heading:

```sh
python3 /config/shell/org_voice.py describe          # readable
python3 /config/shell/org_voice.py describe --json   # what the Emacs menu reads
```

To run it on the laptop against your synced files:

```sh
VOICE_CONFIG=~/org/main/.voice.toml python3 voice/org_voice.py describe
```

`SPC o v d` in Emacs shows the same thing for the heading at point.

## Testing a change

**Before changing the engine**, run the phrase tests. They use made-up Org files in `voice/tests/fixtures/` (the repo is public, so no real data goes in there):

```sh
python3 -m unittest discover -s voice/tests          # pass or fail
python3 voice/tests/test_org_voice.py --show         # failures, with replies and diffs
```

Each test is a short conversation: the phrases, the reply to each, and the lines the whole conversation adds and removes. When you add a behavior, add a case to `CASES` in `test_org_voice.py`.

**Before changing the rules in your real files**, try the phrases on a copy, never on the live synced files:

```sh
mkdir -p /tmp/voice-try && cp ~/org/main/{Tasks,Shopping}.org ~/org/main/.voice.toml /tmp/voice-try/
VOICE_CONFIG=/tmp/voice-try/.voice.toml VOICE_STATE_DIR=/tmp/voice-try \
  python3 voice/org_voice.py --dry-run add "bagels to the costco list"
```

`--dry-run` doesn't write the Org files or save the question and undo state, so it's also safe on the live files when you only want to see what a phrase would do.

## Troubleshooting

**Where to look.** Open the automation's trace in Home Assistant (Settings › Automations › *Voice - Org files* › Traces). The `shell_command` step shows `stdout` (the reply) and `stderr`, where the engine logs `voice: <verb> '<text>'` for every call and `voice answer heard: '…'` for answers. That's how you tell what speech to text actually heard.

| Symptom | Likely cause |
|---|---|
| "Sorry, something went wrong updating your lists." | The engine crashed. The traceback is in `stderr`. Reproduce it with `--dry-run` and the same text. |
| "Sorry, I couldn't update your lists." | The shell command didn't run or printed nothing. Check the path in `configuration.yaml` and that HA was restarted after adding it. |
| It went to Todos instead of the list you named | The name didn't match. Check `describe` for that list's names, and look in the trace for what speech to text heard. Add the misheard form with `SPC o v n`. |
| "System or Grocery?" every time | Two places share that name. Say the parent too ("system general"), or set `VOICE_DEFAULT` on the one you mean. |
| "Which one?" when you named a class | The class's number isn't in its `VOICE_NAME`, or two classes share it. |
| A new TODO keyword isn't recognised | Add it to `todo_keywords` in `.voice.toml`. |
| A satellite asks and never listens | Answer as a new command within 10 minutes: "it's due Friday", "it's for 322". |
| A question was asked on the wrong device, or not at all | The automation couldn't find a satellite entity on the device that heard the command. Check that the device has an `assist_satellite.*` entity. |
| "X has changed since then, so I won't undo it." | The file changed after the voice edit (in Emacs, Orgzly or another voice command). Undo by hand. |
| Emacs shows an old version of the file | Make sure `global-auto-revert-mode` is on. Emacs merges voice edits into buffers with unsaved changes. If you and voice edited the same lines, the buffer shows `smerge-mode` conflict markers and won't save until you resolve them (see *Merge External Edits Into Unsaved Org Buffers* in `init.org`). |

## Files the engine writes

Besides the Org files, the engine keeps three small files in its state directory (`/config/shell/` by default, or `VOICE_STATE_DIR`):

| File | Holds |
|---|---|
| `.voice_state.json` | the waiting question, the last capture (for "When's it due?") and what the last change was |
| `.voice_undo.json` | the previous contents of the files the last change touched, and a hash of each new version |
| `.voice.lock` | a lock, so two commands arriving at once don't edit the same file together (skipped if the directory isn't writable) |

Deleting them is safe: you only lose the waiting question and the undo.
