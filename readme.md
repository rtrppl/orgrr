# orgrr

orgrr is a minimalist but complete note-taking system for Emacs. Its intended purpose is to write, analyze, and manage notes. 

These are the primary functions orgrr provides:

- **orgrr-find** will find and open a note ([more on the basic design of notes in orgrr](#basic-design-of-a-note)) in the `org-directory`. If the title (or alias) entered does not exist, a new note is created ([more on orgrr-find](#orgrr-find)). 

- **orgrr-insert** will insert a link to another note in the `org-directory`. If the title (or alias) entered does not exist, a new note is created ([more on orgrr-insert](#orgrr-insert)).

- **orgrr-show-sequence** will show a sequence of notes ("Folgezettel") for a selected note in a buffer ([more on orgrr-show-sequence](#orgrr-show-sequence)). Use **orgrr-compile-sequence** to create a temporary buffer with the content from a sequence of notes  ([more on orgrr-compile-sequence](#orgrr-compile-sequence)).

- **orgrr-show-backlinks** will show all backlinks (=links from other notes to the note in the current buffer) in a buffer ([more on orgrr-show-backlinks](#orgrr-show-backlinks)). This is what you see in the GIF below.

- **orgrr-show-related-notes** will show all related notes in a buffer ([more on orgrr-show-related-notes](#orgrr-show-related-notes)). For the underlying concept of "relationship", see [orgrr-related-notes](#orgrr-related-notes). 

- **orgrr-add-to-project** and **orgrr-open-project** are for note management and quick access to a limited number of notes.

**Version 0.9.3**:
- Fixes to better deal with spaces in directory names

**Version 0.9.2**:
- Added `orgrr-compile-sequence`

**Version 0.9.1**:
- Better use-package config
- Bug fix for orgrr-check-for-container-file

**Version 0.9**:  
- Optimizing/Rewrite of code for straight package management (in preparation of an potential MELPA release)
- Changes to orgrr-toggle-window-mode
- `C-u orgrr-show-related-notes` now considers backlinks for all containers
- `C-u orgrr-show-backlinks` now considers backlinks for all containers
- Added install instructions for straight.el
- Added `orgrr-open-ref-url`
------------------------------

## Table of Contents

- [Installation](#installation)
- [orgrr's way of dealing with notes](#orgrr's-way-of-dealing-with-notes)
  - [Origin story](#origin-story)
  - [Basic design of a note](#basic-design-of-a-note)
  - [orgrr-projects](#orgrr-projects)
  - [orgrr-related-notes](#orgrr-related-notes)
  - [orgrr-containers](#orgrr-containers)
- [Basic functions](#basic-functions)
  - [orgrr-find](#orgrr-find)
  - [orgrr-insert](#orgrr-insert)
  - [orgrr-rename](#orgrr-rename)
  - [orgrr-delete](#orgrr-delete)
  - [orgrr-move-note](#orgrr-move-note)
  - [orgrr-random-note](#orgrr-random-note)
- [Zettel functions](#zettel-functions)
  - [orgrr-find-zettel](#orgrr-find-zettel)
  - [orgrr-no-find-zettel](#orgrr-no-find-zettel)
  - [orgrr-add-zettel](#orgrr-add-zettel)
  - [orgrr-open-next-zettel](#orgrr-open-next-zettel)
  - [orgrr-open-previous-zettel](#orgrr-open-previous-zettel)
- [Functions to show relationships](#functions-to-show-relationship)
  - [orgrr-show-sequence](#orgrr-show-sequence)
  - [orgrr-show-backlinks](#orgrr-show-backlinks)
  - [orgrr-show-related-notes](#orgrr-show-related-notes)	
- [Functions for project management and writing](#functions-for-project-managemant)
  - [orgrr-add-to-project and orgrr-open-project](#orgrr-add-to-project-and-orgrr-open-project)
  - [orgrr-compile-sequence](#orgrr-compile-sequence)
  - [orgrr-container-commands](#orgrr-container-commands)
- [Quality of life functions](#quality-of-life-functions)
  - [orgrr-toggle-single-window-mode](#orgrr-toggle-single-window-mode)
  - [orgrr-fix-all-links-buffer and orgrr-fix-all-links-container](#orgrr-fix-all-links-buffer-and-orgrr-fix-all-links-container)
- [orgrr extensions](#orgrr-extensions)
  - [orgrr-save-website](#orgrr-save-website)
  - [orgrr-show-findlike](#orgrr-show-findlike)
- [FAQ](#faq)

## Installation

**In order to use orgrr you'll need [rg](https://github.com/BurntSushi/ripgrep) installed on your machine.** The easiest way to do so might be [homebrew](https://brew.sh), i.e. `brew install rg`.

### Manual install

Clone the repository:

```git clone https://github.com/rtrppl/orgrr```

To run orgrr, you need to load the package by adding it to your .emacs or init.el:

```org
(load "/path/to/orgrr/orgrr.el") ;; You actually only need orgrr.el
```

If you don't already have done so, you also should set an org-directory as the location for your notes.

```org
(setq org-directory "~/path/to/org-directory")
```

Finally, you may also want to set keybindings for the main functions (I have bound the Mac-command key to super/s):

```org
(global-set-key (kbd "M-s-f") 'orgrr-find)
(global-set-key (kbd "M-s-l") 'orgrr-show-backlinks)
(global-set-key (kbd "M-s-i") 'orgrr-insert)
(global-set-key (kbd "M-s-I") 'orgrr-insert-project)
(global-set-key (kbd "M-s-A") 'orgrr-add-to-project)
(global-set-key (kbd "M-s-P") 'orgrr-open-project)
(global-set-key (kbd "M-s-r") 'orgrr-show-related-notes)
(global-set-key (kbd "M-s-o") 'orgrr-change-container)
(global-set-key (kbd "M-s-a") 'orgrr-add-zettel)
(global-set-key (kbd "M-s-z") 'orgrr-find-zettel)
(global-set-key (kbd "M-s-s") 'orgrr-show-sequence)
(global-set-key (kbd "M-s-p") 'orgrr-open-previous-zettel)
(global-set-key (kbd "M-s-n") 'orgrr-open-next-zettel)
(global-set-key (kbd "M-s-N") 'orgrr-no-find-zettel)
(global-set-key (kbd "M-S-O") 'orgrr-open-ref-url)
(global-set-key (kbd "M-S-c") 'orgrr-compile-sequence)
```

This IMHO works best for linux and Emacs on the command line:

```org
(global-set-key (kbd "C-o") nil)
(global-set-key (kbd "C-o f") 'orgrr-find)
(global-set-key (kbd "C-o l") 'orgrr-show-backlinks)
(global-set-key (kbd "C-o i") 'orgrr-insert)
(global-set-key (kbd "C-o I") 'orgrr-insert-project)
(global-set-key (kbd "C-o A") 'orgrr-add-to-project)
(global-set-key (kbd "C-o P") 'orgrr-open-project)
(global-set-key (kbd "C-o r") 'orgrr-show-related-notes)
(global-set-key (kbd "C-o o") 'orgrr-change-container)
(global-set-key (kbd "C-o a") 'orgrr-add-zettel)
(global-set-key (kbd "C-o z") 'orgrr-find-zettel)
(global-set-key (kbd "C-o s") 'orgrr-show-sequence)
(global-set-key (kbd "C-o p") 'orgrr-open-previous-zettel)
(global-set-key (kbd "C-o n") 'orgrr-open-next-zettel)
(global-set-key (kbd "C-o N") 'orgrr-no-find-zettel)
(global-set-key (kbd "C-o O") 'orgrr-open-ref-url)
(global-set-key (kbd "C-o c") 'orgrr-compile-sequence)
```

### straight.el

Via use-package and straight a typical configuration of orgrr could look like this:

```org

(use-package orgrr
  :straight (:host github :repo "rtrppl/orgrr"
		   :branch "main")
  :bind
  (:map global-map
	("C-o" . nil)
	("C-o f" . orgrr-find)
	("C-o l" . orgrr-show-backlinks)
	("C-o i" . orgrr-insert)
	("C-o I" . orgrr-insert-project)
	("C-o A" . orgrr-add-to-project)
	("C-o P" . orgrr-open-project)
	("C-o r" . orgrr-show-related-notes)
	("C-o o" . orgrr-change-container)
	("C-o a" . orgrr-add-zettel)
	("C-o z" . orgrr-find-zettel)
	("C-o s" . orgrr-show-sequence)
	("C-o p" . orgrr-open-previous-zettel)
	("C-o n" . orgrr-open-next-zettel)
	("C-o N" . orgrr-no-find-zettel)
	("C-o O" . orgrr-open-ref-url)
	("C-o c" . orgrr-compile-sequence)))
	
```

As above, if you don't already have done so, you also should set an org-directory as the location for your notes.

```org
(setq org-directory "~/path/to/org-directory")
```

## orgrr's way of dealing with notes

### Origin story

orgrr began as a nearly feature-complete replica of the core functionality of [org-roam v1](https://github.com/org-roam/org-roam-v1), built using [ripgrep](https://github.com/BurntSushi/ripgrep) (rg), a lot of regex and hashtables. It does recognize alternative note titles (`#+roam_alias`) and tags (`#+roam_tags`) as introduced by org-roam. orgrr currently only works with [org-files](https://orgmode.org) (i.e. in a technical sense: files ending in .org).

**A crucial difference between org-roam and orgrr is the use of databases. orgrr only relies on rg to update it's data about org-files, which is stored in hashtables. The aim is to have no dependencies on sql or related software. A second difference is that orgrr sticks to the ideal of every note being a single file (no nodes!). The final difference is relative minimalism - orgrr should have all necessary features to write, analyze, and manage notes - and draw on org-mode/Emacs for everything else.**

This package primarily address my own needs and I have been using orgrr almost daily for more than a year now (September 2024). My main container has more than 4000 notes and orgrr is much faster than org-roam. Even on a Rasberry Pi 5, rg needs less than a second to extract all of the meta-data! It may be among the fastest of the Zettelkasten packages available for Emacs.

**As no database is involved, orgrr works great with [Dropbox](https://www.dropbox.com/), [Google Drive](https://drive.google.com/) or other file-syncing solutions.** 

### Basic design of a note

In orgrr, all notes are assumed to follow a certain logic regarding metadata. The design principles used here are similar to org-roam v1 and interoperation between orgrr and org-roam v1 is possible (and was intended). Filenames themselves are used as unique identifiers and changing them without adjusting backlinks will break the connection between two notes (see also [orgrr-rename](#orgrr-rename)).

At the very minimum, **a note file for orgrr is an .org file that includes the following line**:

```org
#+title:       title of a note
```

One of the unique strengths of org-roam v1 was the inclusion of `alias`, a potential alternative title for a note. This allows to add abbreviations or translated names to the original note. orgrr also recognizes alias, which have to be in quotation marks.

```org
#+roam_alias:  "alias 1" "alias 2"
```

As demonstrated above, there can be more than one alias.

orgrr also recognizes tags in the same way as org-roam v1 did, separate from regular [org-tags](https://orgmode.org/manual/Tags.html). In v2, org-roam began to use org-tags. I still prefer the approach of v1.

Tags can be very useful to add a limited set of meta-data to your notes. There is no auto-completion for tags, so stick to a few that you can remember.

Tags are added without quotation marks, separated by space.

```org
#+roam_tags:   tag1 tag2 tag3
```

There is an ongoing debate on whether or not a true Zettelkasten-system also needs to respect Luhmann's emphasis on the importance of the sequence of notes ("Folgezettel"). For an in-depth discussion of the topic see [here](https://zettelkasten.de/folgezettel/).

To be honest, initially, I did not see the need to add this. After all, didn't Luhmann use zettel IDs primarily for linking and this could be much more efficiently handled with org-links? Over time, however, the idea grew on me for a particular reason: This is another great way to show relationships between notes! It is also the only option to create a hierarchy of notes. Both aspects are very useful when you have more than a few hundred notes. 

Values for zettel (i.e. zettel IDs) are added without quotation marks (and you should use [orgrr-add-zettel](#orgrr-add-zettel) for this). Using zettel values in orgrr makes most sense if you stick to [Luhmann's naming scheme](https://niklas-luhmann-archiv.de/bestand/zettelkasten/zettel/ZK_1_NB_1-5_V), e.g. 1a, 1a1, 1a2, 1a2a, 1a3, 1a3a...., as orgrr is using lexical sorting for these values.

```org
#+zettel:   value
```

A final piece of official meta-data also has its roots in org-roam v1: 

```org
#+roam_key:   url
```

This is the place to store a link to a source webpage, a Zotero entry or any other URL that orgmode knows to open. If you visit a note with a value for `#+roam_key`, executing `M-x orgrr-open-ref-url` will directly open this link.

In total, orgrr therefore recognizes these five lines of meta-data in an org-file:

```org
#+title:       title of a note
#+roam_alias:  "alias 1" "alias 2"
#+roam_tags:   tag1 tag2 tag3
#+roam_key:    URL
#+zettel:      value
```

Only the title is required, though. 

### orgrr-projects

A feature that seemed absent in orgrr (and org-roam) was a way to bring together a collection of notes and snippets from backlinks and many other places to create a "desktop" of notes. In the imagery of the Zettelkasten, this would be a place to look at several "Zettel" / notes at the same time. orgrr-projects is an approach to deal with this problem. 

On the most basic level, an orgrr-project is any note that has the tag `orgrr-project`:

```org
#+title:     title of the note/collection
#+roam_tags: orgrr-project
```

The function [orgrr-add-to-project](#orgrr-add-to-project) takes the current paragraph visited at point (the cursor) in an org-file or in orgrr-backlinks and appends it to a chosen project. A source-link is added, to allow for follow-up. If the project name does not yet exist, a new orgrr-project is created in the org-directory (similar to the way orgrr-find and orgrr-insert operate). This feature works across [orgrr-containers](#orgrr-containers). 

orgrr-projects thereby enable rapid access to a set of paragraphs and are the main holding area for work in progress in orgrr. It also allows for writing projects in different containers than the location of the original note. 

### orgrr-related-notes

There are many different attempts to surface related notes in note-taking systems in Emacs (and outside of it). Most of them draw on some variation of text-analysis and algorithmic determination of proximity. I always felt that the links one personally adds to notes are an underused asset for assessing the proximity between notes. This function collects all notes related (via links) to the current note to the second degree - it collects the backlinks for the backlinks and the outgoing links mentioned by outgoing links. Using a family as analogy, orgrr considers all parents and grandparents as well as all children and grandchildren of a note. All links to a specific note are counted and the resulting list is ranked by frequency. This is much quicker (about 10 times) than the excellent [org-similarity](https://github.com/brunoarine/org-similarity) and still produces very interesting results. See the example below:

![orgrr-show-related-notes](/orgrr-show-related-notes.png)

See also [orgrr-show-related-notes](#orgrr-show-related-notes).

### orgrr-containers

Another feature that felt missing in orgrr (and org-roam v1) was the option to keep several different sets of data. [Obsidian](https://obsidian.md)'s [vaults](https://help.obsidian.md/Getting+started/Create+a+vault) is an example of this idea and has been the inspiration for orgrr-containers. Each orgrr-container is a folder containing org-files (that may have sub-folders with org-files of their own). 

Please note that this works by changing the "org-directory" through `orgrr-change-container`. The file `~/.orgrr-container-list` contains a list of all containers (to which you can add and remove containers by using `orgrr-create-container` and `orgrr-remove-container`, see also [orgrr-container-commands](#orgrr-container-commands)). If your setup is anything like mine, this also will affect your org-agenda (for me this is a feature). 

If you have set an org-directory in your .emacs, this will always be the starting point for orgrr upon a restart of emacs.

## Basic functions

### orgrr-find

This function searches the org-directory (and all its subdirectories) for a note. It should work with all [completing-read frameworks](https://www.emacswiki.org/emacs/CategoryCompletion) but I only tested with [Helm](https://github.com/emacs-helm/helm) and [Vertico](https://github.com/minad/vertico). You can search for any combination of tags, zettel ID and title (or alias). A [marked region](https://www.gnu.org/software/emacs/manual/html_node/emacs/Mark.html) is recognized to narrow search.

If the note does not exist, then a new one with the chosen title will be created in the `org-directory`, i.e. the current container. The naming scheme of the new file is similar to org-roam v1. In other words, you should use orgrr-find and orgrr-insert to create new notes. If you want to abort the creation process you should invoke `kill-current-buffer`.

### orgrr-insert

This will search the org-directory (and all its subdirectories) for a note and then inserts a link to this note at point. You can search for any combination of tags, zettel ID and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. The link to the new note will also be added at point. As above mentioned, you should use orgrr-find and orgrr-insert to create new notes and should use `kill-current-buffer` to abort.

A special variant of this function is `orgrr-insert-project`, which allows to insert a link to any orgrr-project in the current container (see [orgrr-projects](#orgrr-projects)).

### orgrr-rename

orgrr uses file names as unique indentifiers. Therefore changing them will break the connection between notes - changing the `#+title` of a note (or any other meta-data about a note), however, will not cause any harm. In theory there should be no need to ever change the name of a file (or its location) after its creation. But sometimes there are stupid typos or naming conventions and the need to change a file name arises.

This function allows to change the name of the file/note the current buffer visits and all corresponding links in other notes in the org-directory. Use it with caution!

### orgrr-delete

This function deletes the current note and displays the previous buffer. 

### orgrr-move-note

This function allows to move the current note to one of the other containers. All links in the note and all containers (!) will be adjusted accordingly.

### orgrr-random-note

This function opens a random note in the current container (org-directory). 

## Zettel functions

### orgrr-find-zettel

Similar to [orgrr-find](#orgrr-find), but limited to notes that have a zettel value. This is useful if you store some primary sources and your notes in the same container and only want to search your notes (as they should be the only ones with a zettel value).

### orgrr-no-find-zettel

Similar to [orgrr-find-zettel](#orgrr-find-zettel), but limited to notes that do not have a zettel value yet. Wrote this a few days after working with zettel values as a way to quickly find notes that still need a zettel ID. The naming of this function (and it's code) could be improved.

### orgrr-add-zettel

This function helps you to find a zettel value, i.e. a zettel ID, for the current note. Hitting enter on an entry in the mini-buffer will narrow the focus of your search. Only if you input a value that does not yet exist, the selection is accepted and an entry for "#+zettel:" is added to the front-matter. 

### orgrr-open-next-zettel

Opens the note/zettel following the current one. The notes' order is in accordance with Luhmann's sorting, e.g. 1a1, 1a1a, 1a1b etc.

### orgrr-open-previous-zettel

Opens the note/zettel coming before the current one. The notes' order is in accordance with Luhmann's sorting, e.g. if this function is invoked while visiting 1a1a, it might take you to 1a1.

## Functions to show relationships

### orgrr-show-sequence

One of the main benefits of adding zettel values to notes becomes the ability to explore a sequence of notes. After you have selected a starting point, this functions displays a sequence of notes in a buffer (in accordance with your settings for orgrr-window-management, see [orgrr-toggle-single-window-mode](#orgrr-toggle-single-window-mode)). [orgrr-compile-sequence](#orgrr-compile-sequence) has a somewhat similar functionality but also collects the content of all notes of the sequence.

### orgrr-show-backlinks

![orgrr-show-backlinks](/2024-orgrr-show-backlinks.gif)

This displays all backlinks for the note in the current buffer in a side-window. The buffer here is temporary. You can navigate it as you would with any other org buffer and, for example, jump between headlines by `org-next-visible-headline` or `org-previous-visible-headline` (or pressing "n" and "p"). The headline link takes you to the line of the snippet in the source document. Invoke the command again to close the side-window (while visiting this buffer).

### orgrr-show-related-notes

This displays all related notes for the note in the current buffer (see also [orgrr-related-notes](#orgrr-related-notes)). The orgmode buffer with the results is temporary. You can navigate it as you would with any other org buffer and, for example, jump between headlines by `org-next-visible-headline` or `org-previous-visible-headline` (or pressing "n" and "p"). Invoke the command again to close the side-window or buffer (while visiting this buffer).

If called with C-u, backlinks of first and second order in all containers are considered. This may take a while, please be patient. 

## Functions for project management

### orgrr-add-to-project and orgrr-open-project

`orgrr-add-to-project` appends the current line to an orgrr-project and includes a source-link to allow for follow-up. All links within this snippet are corrected to work in the new location. This function works for all org-files and the `orgrr-backlinks` buffer.

`orgrr-open-project` provides quick access to all orgrr-projects.

### orgrr-compile-sequence

`orgrr-compile-sequence` creates a temporary buffer with the content of a sequence of notes. It relies on the values for `#+zettel`in these notes. Start- and end-point are set by the user. See also [orgrr-show-sequence](#orgrr-show-sequence) for related functionality. If called with `C-u`, i.e. `C-u orgrr-compile-sequence`, the temporary buffer is created without the titles of the individual notes as headlines. The intention behind this function was to not only create a quick view on a stack of notes but also to facilitate long-form writing.

Clicking on the headlines or any other link in the temporary buffer will open the linked note in `other-window` (can be turned off by setting `orgrr-compile-open-link-other-window` to `nil`).

### orgrr-container-commands

`orgrr-create-container` promptes for or creates a directory to be used as a container, adds this container to the container list and then switches to it. `orgrr-remove-container` removes a specific container from the list and switches back to "main". `orgrr-change-container` allows switching between containers. 

## Quality of life functions

### orgrr-toggle-window-mode

This function switches between `multi-window` and `single-window-mode`, affecting the display of "backlinks", "related notes" and "sequence of notes" (and the way links are opened). Attention: In version 0.9 the default (now `single-window-mode`) and name of this function changed (from `orgrr-toggle-single-window-mode`).

### orgrr-fix-all-links-buffer and ogrr-fix-all-links-container

These functions were a byproduct of rewriting orgrr-move-note and correct links either in the current buffer or the current container. Fixing hereby means: if you move org files to a different folder, org-links with hard-wired directories in them may cease to work. The functions will fix these links as long as there is a file with the same filename in the known containers. Even using the speedy rg, orgrr-fix-all-links-container may take a while to complete.

## orgrr extensions

orgrr extensions are additions to the core functionality of orgrr that may introduce new dependencies to other packages or external software. orgrr will always run fine without them and if you want a minimalist setup, you don't need these. In order to use the extensions you may have to add this file to your load-path (see above installation info):

```org
(load "/path/to/orgrr/orgrr-extensions.el") 
```

### orgrr-save-website

This function saves a website as an org-file in the current org-directory (remember that the [orgrr-container-commands](#orgrr-container-commands) allow you to easily change the org-directory). It uses [org-web-tools](https://github.com/alphapapa/org-web-tools) and [Pandoc](https://pandoc.org/) to create these org-files. I use this to store primary source material (=websites) for later analysis. The function saves the website at point - if there is a URL or an org-link. If no such link is provided, it will prompt for an URL.

### orgrr-show-findlike

This function uses the command line tool [findlike](https://github.com/brunoarine/findlike) by [Bruno Arine](@brunoarine@hachyderm.io), which you need to install before, to create a list of ten related notes in a side-window. Invoke the command again to close the side-window (while visiting the "orgrr findlike" buffer).

## FAQ

- Isn't this a ridiculous waste of time? Why bother?

Probably. The way how org-roam v1 operated really resonated for me. I liked the idea of my notes being a collection of many small text files. The mandatory use of org-id in org-roam v2 made it challenging to understand where links in the notes would be ultimately direct to. A potential conversion to Markdown or something else would also be much harder - in short (and I might be wrong on this) the changes between v1 and v2 seemed to make org-roam less future-proof, while offering little additional benefit for my personal use-case.

If you don't like orgrr but are interested in similar note-taking systems, you may want to check out [org-roam](https://www.orgroam.com), [Denote](https://github.com/protesilaos/denote) or [ZK](https://github.com/localauthor/zk). Also, [minaduki](https://github.com/kisaragi-hiu/minaduki) is a seemingly well-maintained real (fork) of org-roam v1, if that is what you are looking for.  There is at least one more project that uses rg, [gkroam](https://github.com/Kinneyzhang/gkroam), which I only discovered after orgrr was done. Another interesting project with a (very) long history is [howm](https://github.com/kaorahi/howm), which also can be set to use rg (see [here](https://leahneukirchen.org/blog/archive/2022/03/note-taking-in-emacs-with-howm.html)).

- But you could have continued to use org-roam v1!

This is what I did for a long time. But every time I (re-)installed my setup, e.g. installing org-roam v1 on a new machine or upgrading Emacs itself, I encountered some obscure issues with my time-frozen setup of org-roam v1. In particular in Emacs 29 I had issues with emacsql and everything database related. Emacs 29.1 finally broke org-roam v1 beyond repair.

- Is that all?

No. I also wanted to learn more elisp. A small project like this seemed to be a good way to start. I'm still amazed that only about 1000 lines of code are necessary to write a note-taking system that is not too far off from Denote or org-roam. That this thing works, provides me with independence from note-taking trends or specific operating systems, which is one of the reasons I don't see myself stopping to work on this project.

- What does orgrr stand for?

orgrr started as an acronym for "org-roam ripgrep" or "org-roam replica", as org-roam calls itself a [roam-research](https://roamresearch.com) replica. After many months of very slow but steady additions, the differences between orgrr and its inspiration are growing.

- Is this a subtle criticism direct at the org-roam approach?

Not at all! The project was born out of admiration for the pioneering work done by Jethro Kuan and others. You should check out the real org-roam. 
