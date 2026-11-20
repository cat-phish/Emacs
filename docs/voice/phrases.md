# What you can say

Every command starts with a verb. Everything after it is free text, and the engine works out where it goes. Words in [brackets] are optional, and (a|b) means either word.

## Adding things

| Say | What happens |
|---|---|
| "capture call the landlord" | `** TODO call the landlord` at the bottom of Todos (the default place) |
| "capture call the landlord due Friday" | the same, with `DEADLINE: <…Fri>` |
| "capture chore descale the kettle" | `** CHORE descale the kettle`. *Chore* and *todo* are spoken keywords. |
| "add bagels to the Costco list" | `- [ ] bagels` at the top of Grocery › Costco |
| "add eggs, milk and mac and cheese to the shopping list" | three items. A known item with "and" in its name stays whole. |
| "capture buy stamps into my inbox" | a TODO in the Inbox |
| "capture system home lab set up headscale" | a checkbox in System › Home-Lab |
| "capture barista assistant backlog dark mode" | a checkbox in that project's Backlog |
| "add to the barista assistant backlog dark mode" | the same, with the backlog named first |
| "capture lab five for 322 due Sunday" | `*** ASSIGNMENT lab five` under CPT_S 322, with a deadline |
| "capture assignment lab five" | asks "Which one?". The class is the answer: "three twenty two". |

**Naming the place.** Say it at the end, after *to, into, in, on, onto, for, from, off, at* or *under* ("…to the Costco list"), or say it right after the verb ("capture system home lab…"). If you name nothing, the item goes to the default place (Todos). A place can be named by:

- its title: "costco", "trader joe's", "home lab";
- an alias set in `VOICE_NAME`: "shopping" for Grocery, "322" for CPT_S 322, "assignment" or "homework" for School;
- its parent's name in front: "bar stock", "system general", "barista assistant backlog".

Case, punctuation, hyphens, plurals, "the/my/our" and a trailing "list" don't matter, and spoken numbers become digits ("three twenty two" is 322). Small mishearings of longer names ("barrista assistant") still work.

**Dates** can be today, tomorrow, a weekday, "next Friday", "in two weeks", "the 30th", "November 22nd", "12/2" or "end of the month". They have to come after *due*, *by* or *on* ("…due Friday"), except a final *today*, *tonight* or *tomorrow*.

**When the engine asks back:**
- **"When's it due?"** comes only from sections set to ask (School). The entry is already written. Answer "due Friday" or "no deadline", or say nothing to leave it without one.
- **"Which one?"** means you named a section whose children are the choices (School, Projects) but not which child. Nothing is written until you answer.
- **"System or Grocery?"** means the name you used fits more than one place.
- **"Which list should rye go on?"** means you added something to a list made of sub-lists (Bar › Stock, System). Answer with the sub-list; the first word is enough ("garnish").

**Already on the list.** On shopping lists, an item that's already there and checked off is unchecked instead of added twice. If it's there and unchecked, you're told so and nothing changes.

## Checking things off

| Say | What happens |
|---|---|
| "check off eggs and butter" | checks them off on every list where they're open |
| "check off eggs on the Costco list" | only on that list |
| "I got the limes" | same as check off |
| "done change bank password" | a heading: `TODO` becomes `DONE`, with `CLOSED:` and a LOGBOOK line, as Emacs does it |
| "done water the garden" | a repeating heading moves to its next date and gets `:LAST_REPEAT:` |
| "done take out the trash" | if two headings have that title, asks "Inbox or Todos?" |
| "I paid the Avista bill", "mark PayPal Credit as paid" | only looks at `BILL` headings, and picks the soonest due if several match. A repeating bill keeps `BILL` and moves its date. |

## "We're out of…"

| Say | What happens |
|---|---|
| "the bar is out of bourbon" | unchecks bourbon in Bar › Stock. If bourbon is on the Grocery list and checked off, it's unchecked there too. |
| "we're out of limes" | the same, searching every stock list (any list with a `VOICE_LINK`) |
| "we're out of mac and cheese" | not a stock item: it's unchecked wherever it was checked off |

In stock lists, `[X]` means you have it. "Out of" never adds anything to any list.

## Reading lists back

| Say | Reads |
|---|---|
| "what do I have to do" | the default place (Todos): open headings |
| "what's in my inbox" | the Inbox |
| "what's on the Costco list", "read bar stock" | open items. A list of sub-lists reads them all. |
| "what's on the barista assistant backlog" | that backlog |
| "what bills are due", "what bills do I owe" | every unpaid bill, soonest first, with due dates |

Long lists stop after 15 items ("…and 12 more"), and bills after 10.

## Fixing mistakes

| Say | What happens |
|---|---|
| "scratch that", "never mind" | While a question is waiting: drops it, and nothing is written. Otherwise it undoes the last change, if that was in the last 10 minutes. |
| "undo that" | undoes the last change voice made, whenever it was |

Undo covers both files and refuses if a file has changed since (by Emacs, Orgzly or another voice command), so it never overwrites anything else.

## Answering as a new command

A question stays open for 10 minutes, and you can answer it as a new command: "it's due Friday", "it's for three twenty two", "no deadline". You need this in two cases:

- **On a device that can't be asked a question** (the phone app, typed text). The question comes back as the reply, and you answer it with one of those phrases.
- **When a voice satellite speaks the question but doesn't listen.** Some satellites (the Echo Dot running echo-dot-assist, at least) do this now and then. The question is asked once more first. If there's still no answer, it waits quietly for you to answer as a new command.

At most three questions are asked in a row.
