

# Introductio


## About This Config

This is my personal Org Mode first Emacs config. It is built on top of
the excellent Kickstart.emacs project by MiniApollo. Although this
config is set up for basic coding operations, I do not personally write
code in Emacs, as such, there are very few optimizations in this config
for that.

The goal for this config was to mirror my personal Neovim config as
closely as possible, while adding in some simple QOL functions and
mappings to make my Org Mode experience a little more intuitive and
seamless.

This config is heavily inspired by Doom Emacs. However, for my use case,
I found that Doom was overly comlicated for my use case, and generally
I tend to not prefer that level of abstraction in my configs. I did try
to implement my favorite aspects from Doom, while keeping things as
simple and focused as possible.

Feel free to borrow and build on this config if you'd like, or just
steal some snippets.

It is **recommended** to configure it from the **init.org** file. Saving the
init.org file should tangle the code into the init.el file automagically.


## First Timer Tips


### Ctrl & Meta

This is from the Emacs tutorial, but I put this here for safety.

Emacs commands generally involve the CONTROL key (often labeled CTRL)
or the META key (usually labeled ALT). Rather than writing that
in full each time, we'll use the following abbreviations:

C-<chr>  means hold the CONTROL key while typing the character <chr>.
		 Thus, C-f would be: hold the CONTROL key and type f.
M-<chr>  means hold the META or ALT key down while typing <chr>.
		 If there is no META or ALT key, instead press and release the
		 ESC key and then type <chr>.  We write <ESC> for the ESC key.


### Where to start?

First I recommend starting with the Emacs tutorial with the following keybinding: C-h t
Or running the command with M-x: help-with-tutorial
Even if you plan on using Vim keybindings later on, it's a good idea to learn the standard Emacs keybindings.
If you already know the standard Emacs keybindings, you can skip this step.

After you completed that, you should start learning Emacs lisp.
It is not necessary to know elisp, but I recommend it. It will open up a whole new world, trust me.
I used Emacs and Neovim without knowing much about the core language and just copy pasting packages or plugins.
But after learning the fundamentals you can customize basically anything and I really mean ANYTHING.

This will look daunting at first, but don't be scared. This is why I am here.
Elisp is not harder than lua or any other language.

If you don't know anything about elisp, I recommend taking some time to read through
a guide. One possible example which will only take 10-15 minutes:
<https://learnxinyminutes.com/elisp/>

Or if you want to know everything about elisp:
<https://www.gnu.org/software/emacs/manual/html_node/eintr/>


### Getting Help

If you get stuck or confused about something, you're in luck:
Because Emacs has a really good help functionality.
Everything under C-h is used for help keybindings.

Using the following keybind: C-h C-h
Or running the following command M-x: help-for-help
Will open up the Emacs help for help menu (I know it's a really creative name :D).
This is the central place where you can see all possible commands you can use when you are stuck.

The most used commands for me are:

-   C-h v Describe variable
-   C-h f Describe function
-   C-h k Describe key

Other also really useful ones:

-   C-h i Show all installed manuals
-   C-h r Emacs manual
-   C-h a Search for commands (see also M-x: Apropos)
-   C-h m Show help for current major and minor modes

These commands should be the first thing you use when you're stuck or confused with something.


### Searching the manual

The manual has a specific mode called info-mode.
Which has useful features you can use.
For more about info-mode: C-h f: info-mode

Basic navigation:

-   d key: Go back to the top-level Info Directory.
-   u key: Go up to the parent node in the document's hierarchy.
-   n key: for next node in the current manual.
-   p key: for previous node in the current manual.

**Note:** A Node is essentially a specific section or topic, much like a chapter or subsection in a book.

When you are browsing through the Emacs manual you can easily search with:

-   i key: for specific subjects in the current manual.
-   g key: for nodes (sections).
-   m key: for menu items within the current Info buffer.
-   s key: for regular expression patterns (like words) within the current Info buffer.

For evil users after setting up evil collection for info mode:

-   g-j: for next node in the current manual.
-   g-k: for previous node in the current manual.
-   g-G: for nodes (sections).
-   g-m: for menu items within the current Info buffer.
-   The other keybinds are the same

To see all the evil [keybindings](https://github.com/emacs-evil/evil-collection/blob/master/modes/info/evil-collection-info.el) for info mode.

Using goto-node:

-   Specify a Manual: If you want to search within a particular manual, place its name in parentheses like (emacs).
-   After that tell where you want to go in that manual. For example (emacs)Top, (emacs)specific section.
-   If you don't type a manual name, goto-node will search the currently opened manual.


# Core Setup & Performance

These are essential settings and small tweaks that must load before any packages.
They impact the entire configuration, skipping these could result in significantly
slower configuration and potential breakage of your setup.


## Startup Performance

Make startup faster by reducing the frequency of garbage collection. This will be set back when startup finishes.


## Auto-tangle Configuration file

Auto-Tangle Org configuration file for better startup times, it refreshes the package-quickstart file.
We'll cover package quickstart in the package manager section later.

If you like to auto tangle an Org file, don't forget to add the following line to the top of your Org document:
(#+PROPERTY: header-args:emacs-lisp :tangle ./init.el :mkdirp yes)

**Remember**, if this code can't be loaded (errors before this code), the init.el file won't update on change!
To fix this, you need to find this file (C-x C-f), fix the error and press C-c C-v t to tangle it manually.

This snippet adds a hook to org-mode buffers so that start/org-babel-tangle-config gets executed each time such a buffer gets saved.
This function checks to see if the file being saved is the init.org file you’re looking at right now, and if so,
automatically exports the configuration here to the associated output files.


## Show startup time


## Package manager

We use the default built in package manager package.el.

Alternative package managers (straight.el, elpaca, etc.) are useful if you want:

-   Faster package installation.
-   Lock file support for recovery if something goes wrong.
-   Packages that are cloned as Git (or other) repositories, not as opaque tarballs.
-   UI
-   Async support

If you are interested in using other package managers, check out their git repositories.
To learn more about why something like [doomemacs uses](https://github.com/doomemacs/doomemacs/blob/master/docs/faq.org#why-does-doom-use-straightel-and-not-packageel) straight.el not package.el.

In my experience Package.el is not slow and gets the job done.

To update/upgrade packages, use the package-upgrade-all command.


### Use-package

A macro that allows you to isolate package configuration in your .emacs file in a way that is both performance-oriented and, well, tidy.
We use it because it makes package configuration really easy.

With Emacs 29 use-package is now built-in.

This code eliminates the need to type :ensure t for each package download.
Instead, you'll only need to use :ensure nil when you want to explicitly prevent a package from being downloaded.

1.  Most used parts of use-package

    Here is some notes on what each use-package keyword does.
    For more check out the use-package documentation to see how powerful it is:
    C-h i: g for goto-node: Type (use-package)Top
    
    This code block is not tangled, it is just an example.


### Setting package repositories

Like Linux distributions, Emacs uses repositories to manage its packages.


### Package quickstart

Improves startup times by allowing Emacs to precompute and generate a single, large autoload file.
Instead of re-computing them on every startup.

The larger your configuration, the more it will be felt at startup.

However, if you enable this, you'll need to manually run the package-quickstart-refresh
command whenever your package activations change, such as when you modify the package-load-list value.
We put it inside Auto-tangle hook so when we save this file it runs it automatically.

As I tested, it makes startup about 0.1 seconds faster.
Avg:

-   Off: 0.66 sec
-   On:  0.59 sec

Package quickstart only works with package.el.
If you plan to use a different package manager, remember to remove this section and the package-quickstart-refresh line in the Auto-tangle hook.


## Good Defaults


# Keybindings (Load First)

Packages that we want to load first so we have them as soon as possible if something breaks.


## Evil Mode

An extensible vi/vim layer for Emacs.
For users who find Emacs's native keybindings less intuitive.
It integrates Vim's editing style into Emacs, giving you the best of both worlds.

If you want to use vim keybindings I left the following comments in the General Keybindings section to which lines to uncomment ;; <- evil

If you don't want to interfere with the original keybindings.
You can also try out [meow](https://github.com/meow-edit/meow) which is Yet another modal editing on Emacs.

Notes:

-   You can toggle evil mode with C-z.
-   To paste without yank select the text and use P. This line is especially for ThePrimeagen :)

**To use it, remove :tangle no from the beginning of the source code block.**


## General Keybindings

A keybinding framework to set keybindings easily.

We use general because it gives:

-   a convenient method for binding keys.
-   easy leader key integration.
-   good evil-mode and which-key support.
-   a consistent and unified interface for managing keybinds.

And it is also really customizable.

Note: The Leader key is what you will press when you want to access your keybindings: C-SPC + .  Find file


## Creating keybindings the built in way

If you want to use the built in methods I recommend using these ones:
This code block is not tangled, it is just an example.
To read more about using the built in methods, check out this awesome article from [masteringemacs](https://www.masteringemacs.org/article/mastering-key-bindings-emacs).


# Appearance


## Set Theme

Set gruvbox theme, if you want some themes try out doom-themes.
Use consult-theme to easily try out themes (**Epilepsy** Warning).


## Transparency

With Emacs version 29, true transparency has been added.


## Setting Fonts


## Doom Modeline

le A fancy, fast and customizable mode-line.


## Nerd Icons

This is an icon set that can be used with dired, ibuffer and other Emacs packages.
Don't forget nerd-icons-install-fonts to install the resource fonts.

We use nerd-icons because it supports both GUI and TUI unlike all-the-icons.
Also Doom modeline requires nerd icons.


## Dashboard


# Helper Functions


## Line/Heading Manipulation


## Smart Delete Line/Heading with Contents

This function acts like the normal delete line, unless it is a folded
heading, in which case it will delete the heading and it's contents.


## Heading/List Insertion


### Insert Item Below


### Insert Subitem Below


### Insert Parent Heading Below


## Smart RET Key


# Org Mode


## Org Mode Overview

Here's the meat and potatos of this config. The workflow is heavily inspired
by Doom Emacs. However, it is also heavily opiniated and there are a number
of custom functions to simplify and speedup the workflow. Including context
aware functions for keybinds to create consistant and intuitive behavior.

These optimizations include:

-   of Org Mode buffers, with exemptions, like this init.org.
-   that are easily customizable.
-   and colors have been expanded and are easily customizeable.
-   has been made concise and readable.
    
    -   All agenda items show their parent headers inline. Keep your TODOs short and sweet,
    
    while still getting their context at a glance.
    
    -   High priority and NEXT items show at the top.
    -   Daily overview shows Overdue items first, then items scheduled/deadlined for today.
    -   Upcoming shows items scheduled/deadlined for the next 7 days.
    -   Backlog shows only unscheduled/undeadlined TODOs specifically.
-   when refiling to folded headings.
-   on save.
-   fold cycling added to keybinds.
-   items show as collapsed on launch to keep them out of the way.
-   on launch based on keywords.
-   items when they are marked as done.
-   key handles check boxes, links, tables, and headings.
-   with M-RET.
-   with C-RET, handles check boxes, links, tables, headings,

and TODOs. 

-   with C-S-RET, handles check boxes, links, tables, headings,

and TODOs. 

-   on launch, quit with q.


## Org Mode Config


## Evil-Org-Mode


## Table of Contents


## Org Superstar

Prettify headings and plain lists in Org mode. Modern version of org-bullets.


## Org-Modern

This package implements a modern style for your Org buffers using font locking and
text properties. The package styles headlines, keywords, tables and source blocks.
The styling is configurable, you can disable or modify the style of each syntax element
individually via the org-modern customization group.


## Org-Table-Sticky-Header

Gives you frozen headers when scrolling large tables


## Org-Super-Agenda


## Source Code Block Tag Expansion

Org-tempo is not a separate package but a module within org that can be enabled.
Org-tempo allows for '<s' followed by TAB to expand to a begin<sub>src</sub> tag.


## Org-Roam


## Org-Roam-UI

\#+begin<sub>src</sub> emacs-lisp
(use-package org-roam-ui
  :after org-roam
  :custom
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
 )
(with-eval-after-load 'org-roam-ui
)
\#+end<sub>src</sub>>


## Org-Download


# Development


## Projectile

Project interaction library for Emacs.

Emacs has a built in project manager called project.el, but we don't use it.
You can try it out with the keybinds under C-x p because project.el does not require any special setup to use.

We use projectile because it:

-   supports more features and project types.
-   has better integration with projects.
-   has better documentation.
-   is developed faster.

More [reasons](https://docs.projectile.mx/projectile/projectile_vs_project.html) to use projectile.

You can also make the [consult-dir](https://github.com/karthink/consult-dir) package list all the directories you [specified](https://github.com/MiniApollo/config/blob/main/emacs/init.org#consult-dir) and search from them like in tmux-sessionizer.


## Eglot (LSP)

Built in Emacs client for the Language Server Protocol.
We use Eglot because it is fast and minimal.
For more: C-h i: g: (eglot)Top

Eglot does not automatically download LSP servers. It requires separate download.
The easiest way to install LSP servers is with a package manager.

If you can't use a package manager you can do the following:

-   Download the server (e.g. from github)
-   Add the binary/executable to your path.
-   Or customize the eglot-server-programs list.

To control how a LSP server is started customize the eglot-server-programs list.

There are many alternative LSP clients, one of them is LSP-mode.
Which has more features and supports automatic language server installation.
But it's bigger, so it has more moving parts.

We don't use it because Eglot is more than enough for most people.
If you want to use LSP mode check out their [documentation](https://emacs-lsp.github.io/lsp-mode/) or the [project wiki](https://github.com/MiniApollo/kickstart.emacs/wiki) page for more information.


## Sideline-flymake

Show flymake errors with sideline.


## Yasnippet

A template system for Emacs. And yasnippet-snippets is a snippet collection package.
To use it write out the full keyword (or use autocompletion) and press Tab.


## Tree-Sitter (Tect Objects)

A parser generator tool and an incremental parsing library.
Check out TJ's [video](https://www.youtube.com/watch?v=09-9LltqWLY) to learn why you should use it.

With Emacs 29 Tree-Sitter is now built-in. You may need to compile Emacs from source to have it enabled.
You also need to have a compiler installed so Emacs can compile the parsers into a shared library.
For more info about how to use Tree-Sitter check out this [masteringemacs](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter) article.

Using Tree-Sitter is somewhat hacky because it requires you to:

-   manually manage a source list of the parsers you want to use.
-   remap the major modes you want to use.

You can also use treesit-auto, but it is updated quite slowly so we don't use it.

To explore the current buffer's syntax tree, use the treesit-explore-mode command.

**To use it, remove :tangle no from the beginning of the source code block.**


## Language modes

Emacs contains many “editing modes” that alter its basic behavior in
useful ways. These are divided into “major modes” and “minor modes”.
For more: C-h i: g: (emacs)Modes

Some programming languages require the installation of specific modes to fully integrate and function within Emacs.
These packages are often necessary for features like syntax highlighting, code formatting, linting, and language-specific features.


### Lua mode

Example, how to setup a language mode.
Use C-SPC tab to uncomment the lines.


## Terminal


### Eat

Eat(Emulate A Terminal) is a terminal emulator within Emacs.
It's more portable and less overhead for users over like vterm or eshell.
We setup eat with eshell, if you want to use bash, zsh etc., check out their git [repository](https://codeberg.org/akib/emacs-eat) how to do it.

If you want a faster and more responsive terminal emulator try out vterm.


# Multi File Example


## Adding the lisp directory to load-path

Adds the lisp directory to Emacs's load path to search for elisp files.
This is necessary because Emacs does not search the entire user-emacs-directory.
The directory name can be anything, just add it to the load-path.


## Sourcing the files

To use the elisp files we need to load it.
Notes:

-   Don't forget the file and the provide name needs to be the same.
-   When naming elisp files, functions, it is recommended to use a group name (e.g. init-, start- or any custom name), so it does not get mixed up with other names, functions.


## Using the file

And now we can use everything from that file.


# Version Control


## Magit

Complete text-based user interface to Git.


## Diff-hl

Highlights uncommitted changes on the left side of the window (area also known as the "gutter"), allows you to jump between and revert them selectively.


# Completion


## Corfu

Enhances in-buffer completion with a small completion popup.
Corfu is a small package, which relies on the Emacs completion facilities and concentrates on providing a polished completion.
For more configuration options check out their [git repository](https://github.com/minad/corfu).
Notes:

-   To enter Orderless field separator, use M-SPC. testiness


## Cape

Provides Completion At Point Extensions which can be used in combination with Corfu, Company or the default completion UI.
Notes:

-   The functions that are added later will be the first in the completion list.
-   Be aware when adding Capfs (Completion-at-point-functions) to the list since each of the Capfs adds a small runtime cost.

Read the [configuration section](https://github.com/minad/cape#configuration) in Cape's readme for more information.


## Orderless

Completion style that divides the pattern into space-separated components and matches candidates that match all of the components in any order.
Recommended for packages like vertico, corfu.


## Vertico and Marginalia

-   Vertico: Provides a performant and minimalistic vertical completion UI based on the default completion system.
-   Savehist: Saves completion history.
-   Marginalia: Adds extra metadata for completions in the margins (like descriptions).
-   Nerd-icons-completion: Adds icons to completion candidates using the built in completion metadata functions.

We use these packages because they use Emacs native functions. Unlike Ivy or Helm.
One alternative is ivy and counsel, check out the [project wiki](https://github.com/MiniApollo/kickstart.emacs/wiki) for more inforomation.


# Navigation


## Flash

A flash.nvim clone written in elisp. Allows you to jump around the visible
portion of the buffer by typing in characters matching the spot you want to jump to.

Located in the lisp folder in the config folder. Does not auto-update (not installed
with the package manager).

[Github Repo](https://github.com/JiaweiChenC/flash-emacs)


## Avy (Disabled)

(currently disabled in favor of Flash.el)
Avy allows you to jump to visible text using a char-based decision tree.


## Ivy


# Utilities


## Grease


## Consult

Provides search and navigation commands based on the Emacs completion function.
Check out their [git repository](https://github.com/minad/consult) for more awesome functions.


## Evil Terminal Cursor Changer

Evil Terminal Cursor Changer changes the cursor shape in terminal Emacs.


## Rainbow-Mode (Show Colors) FIXME:

\#+begin<sub>src</sub> emacs-lisp
(use-package rainbow-mode
:ensure t
:hook (org-mode
        emacs-lisp-mode
        css-mode
        conf-mode)
\#+end<sub>src</sub>>


## PDF Tools

\#+begin<sub>src</sub> emacs-lisp
(use-package pdf-tools
    :ensure t
    :mode ("\\\\.pdf\\\\'" . pdf-view-mode)
    :config
    ;; Initialize pdf-tools
    (pdf-tools-install)

;; Use 'Midnight Mode' by default (Dark mode for PDFs)
;; You can toggle this with 'C-c C-r'
(setq-default pdf-view-midnight-colors '("#abb2bf" . "#282c34")) ; One Dark colors

;; Better rendering
(setq pdf-view-use-scaling t
		pdf-view-use-imagemagick nil)

    ;; Setup Evil bindings for PDF view
    (with-eval-after-load 'evil
        (evil-set-initial-state 'pdf-view-mode 'motion)))
\#+end<sub>src</sub>>


## Super Save (Disabled)

This was intended to be used for auto save. But I disabled in favor of
using the built in auto-save hook for Org buffers only (in the Org Mode
:config section above)
\#+begin<sub>src</sub> emacs-lisp
(use-package super-save
:disabled t
:ensure t
:custom
(super-save-auto-save-idle t) ;; Ensure idle saving is explicitly ON
;; Save after 5 seconds of idle time
(super-save-idle-duration 5)
;; Don't save remote files (TRAMP) to avoid lag
(super-save-remote-files nil)
:config
(super-save-mode +1)
(setq super-save-silent nil))
\#+end<sub>src</sub>>


## Undo-Fu-Session

This stores undo history between sessions per-machine in .local/state


## Command Log Mode (Deprecated?)

Not entirely sure what this does but I think it logs command messages.
More info from this stream:
<https://youtu.be/74zOY-vgkyw?t=1541>
;; TODO: come back to this


## Helpful

An alternative to the built-in Emacs help that provides much more contextual information.


## Diminish (Hide Modes in Modeline)

This package implements hiding or abbreviation of the modeline displays (lighters) of minor-modes.
With this package installed, you can add ‘:diminish’ to any use-package block to hide that particular mode in the modeline.


## Rainbow Delimiters (Parenthesis Colors)

Adds colors to brackets.


## Which-Key

Which-key is a helper utility for keychords (which key to press).


## Ws-butler (Remove Whitespace)

Removes whitespace from the ends of lines.


# Runtime Performance

Dial the GC threshold back down so that garbage collection happens more frequently but in less time.
We also increase Read Process Output Max so Emacs can read more data.

