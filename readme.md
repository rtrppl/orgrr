# orgrr 

Orgrr is an almost feature-complete replica of the core functioniality of [org-roam v1](https://github.com/org-roam/org-roam-v1) built around [ripgrep](https://github.com/BurntSushi/ripgrep) (rg), a lot of regex and hashtables. It does recognize `#+roam_alias`, and `#+roam_tags`. Orgrr only works with [org-files](https://orgmode.org) (i.e. files ending in `.org`).

Orgrr provides these functions:

- **orgrr-find** will find a `.org` note file in the `org-directory` ([see here](#orgrr-find))

- **orgrr-insert** will insert a like to another note find in the `org-directory` ([see here](#orgrr-insert))

- **orgrr-show-backlinks** will show all backlinks (=links from other notes to the note in the current buffer) in a side-window  ([see here](#orgrr-show-backlinks))

_Note: In order to use this you'll need [rg](https://github.com/BurntSushi/ripgrep) installed on your machine. The easiest way to do so might be homebrew, i.e. "brew install rg"._

------------------------------

## Table of Contents

- [Installation](#installation)
- [Orgrr's way of dealing with notes](#orgrr's-way-of-dealing-with-notes)
  - [Basic design of a note](#basic-design-of-a-note)
  - [On the use of databases](#on-the-use-of-databases)
- [Functions](#functions)
  - [orgrr-find](#orgrr-find)
  - [orgrr-insert](#orgrr-insert)
  - [orgrr-show-backlinks](#orgrr-show-backlinks)
- [FAQ](#faq)

## Installation

Clone the repository:

```git clone https://github.com/rtrppl/orgrr```

Then load the package by adding it to your load-path in your .emacs or init.el:

```org
(load "/path/to/orgrr/orgrr.el")
```

You may also want to set keybindings for the three main functions:

```org
(global-set-key (kbd "M-s-f") 'orgrr-find)
(global-set-key (kbd "M-s-i") 'orgrr-insert)
(global-set-key (kbd "M-s-l") 'orgrr-show-backlinks)
```

## Orgrr's way of dealing with notes

### Basic design of a note

In orgrr all notes are assumed to follow a certain logic in that they include some metadata. The design principles used here are similar to org-roam v1 and interoperation between orgrr and org-roam v1 is possible. 

At the very minimum, a note file for orgrr is an .org file that includes the following line:

```org
#+title:       title of a note
```

One of the unique strengths of org-roam v1 was the inclusion of `alias` for the title of a note. This allows to add abbreviations or translated names to the original note. Orgrr also recognizes these alias, which have to be in quotation marks.

```org
#+roam_alias:  "alias 1" "alias 2"
```

Orgrr also recognizes tags in the same way as org-roam v1 did, in a way that is separate from org-tags used throughout the document. This has changed in org-roam v2. I use this to add a very limited set of meta-data for my notes (type of source, date reading the source, status of processing the info). Tags are added without quotation marks, separated by space.

```org
#+roam_tags:   tag1 tag2 tag3
```

In total, orgrr therefore recognizes these three lines of meta-data in an org-file:

```org
#+title:       title of a note
#+roam_alias:  "alias 1" "alias 2"
#+roam_tags:   tag1 tag2 tag3
```

### On the use of databases

A crucial difference between org-roam and orgrr is the use of databases. Orgrr only relies on rg to update it's data about the org-files and their meta-data. I have about 3000 notes and the speed between org-roam and orgrr is comparable. 

## Functions

### orgrr-find

This will search the org-directory (and all its subdirectories) for a note. I use [Helm](https://github.com/emacs-helm/helm) for completion and this works smooth. You can search for a combination of tags and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, then a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. 

### orgrr-insert

This will search the org-directory (and all its subdirectories) for a note and then insert a link to this note at point. You can search for a combination of tags and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, then a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. The link to the new note will also be added at point.

### orgrr-show-backlinks

This will show all backlinks for the note in the current buffer in a side-window. The buffer here is temporary. You can navigate this just as a regular org document and, for example, jump between headlines by pressing "n" and "p". The link takes you to the line of the snippet in the source document. 

## FAQ

- Isn't this a rediciulous waste of time? Why bother?

Certainly. Newer versions of org-roam are far more advanced that this system. And if one does not like org-roam for one reason or another, there still is Denote or ZK to try out.

Personally, the way how org-roam v1 operated really clicked for me. I liked the idea that my notes would be a collection of many small text files. The mandatory use of org-id in org-roam v2 made it difficult to know where links in the notes would be directing to. A potential conversion to Markdown or something else would also be much harder - in short (and might be incorrect about this) the changes between v1 and v2 seemed to make org-roam less future-proof, while offering little additional benefit for me.

- But you could have continued to use org-roam v1!

True and that is what I did for a long time. But everytime I (re-)installed my setup and with every change of Emacs I encountered some obscure issues with my time-frozen setup of org-roam v1. In particular I had issues with emacsql and everything database related.

- Is that all?

No. I also wanted to learn more elisp. Doing a small project like this seems to be best way doing this. 

- What does orgrr stand for?

Orgrr is an acronmy for "org-roam ripgrep" or "org-roam replica", as org-roam calls itself a [roam-research](https://roamresearch.com) replica. The point of this package is not at all to critize org-roam, it rather is a work of adminiration.
