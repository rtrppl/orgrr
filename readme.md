# orgrr 

![orgrr-show-backlinks](/orgrr-show-backlinks.png)

Orgrr is an almost feature-complete replica of the core functioniality of [org-roam v1](https://github.com/org-roam/org-roam-v1) built around [ripgrep](https://github.com/BurntSushi/ripgrep) (rg), a lot of regex and hashtables. It does recognize `#+roam_alias` and `#+roam_tags` and only works with [org-files](https://orgmode.org) (i.e. files ending in .org).

Orgrr provides these primary functions:

- **orgrr-find** will find a `.org` note file in the `org-directory` ([see here](#orgrr-find)). If there is no note of this name, a new one is created. 

- **orgrr-insert** will insert a like to another note in the `org-directory` ([see here](#orgrr-insert)). If there is no note of this name, a new one is created. 

- **orgrr-show-backlinks** will show all backlinks (=links from other notes to the note in the current buffer) in a side-window  ([see here](#orgrr-show-backlinks)).

If you are interested in a less minimalist and more comprehensive note taking experience you may want to check out [org-roam](https://www.orgroam.com), [Denote](https://github.com/protesilaos/denote) or [ZK](https://github.com/localauthor/zk). 

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
  - [orgrr-rename](#orgrr-rename)
- [FAQ](#faq)

## Installation

Clone the repository:

```git clone https://github.com/rtrppl/orgrr```

Then load the package by adding it to your .emacs or init.el:

```org
(load "/path/to/orgrr/orgrr.el") ;; You actually only need orgrr.el
```

If you don't already have done so, you also have to set an org-directory.

```org
(setq org-directory "~/path/to/org-directory")
```

In order to use Orgrr you'll need [rg](https://github.com/BurntSushi/ripgrep) installed on your machine. The easiest way to do so might be homebrew, i.e. `brew install rg`.

Finally, you may also want to set keybindings for the three main functions:

```org
(global-set-key (kbd "M-s-f") 'orgrr-find)
(global-set-key (kbd "M-s-i") 'orgrr-insert)
(global-set-key (kbd "M-s-l") 'orgrr-show-backlinks)
```

## Orgrr's way of dealing with notes

### Basic design of a note

In orgrr all notes are assumed to follow a certain logic in that they include some metadata. The design principles used here are similar to org-roam v1 and interoperation between orgrr and org-roam v1 is possible. Hence filenames themselves are used as unique identifiers and changing them without adjusting backlinks will break the connection between two notes (see also [orgrr-rename](#orgrr-rename)).

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

If the note does not exist, then a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. In other words, you should use orgrr-find and orgrr-insert to create new notes.

### orgrr-insert

This will search the org-directory (and all its subdirectories) for a note and then insert a link to this note at point. You can search for a combination of tags and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, then a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. The link to the new note will also be added at point. As above mentioned, you should use orgrr-find and orgrr-insert to create new notes.

### orgrr-show-backlinks

This will show all backlinks for the note in the current buffer in a side-window. The buffer here is temporary. You can navigate this just as a regular org document and, for example, jump between headlines by pressing "n" and "p". The link takes you to the line of the snippet in the source document. 

### orgrr-rename

Orgrr uses file names as unique indentifiers. Therefore changing them lightly will break the connection between links - but changing the title of a note (or any other meta-data about a note) will not cause any harm. In theory there should be no need to change the name of a file (or its location) after its creation. But sometimes there are stupid typos or naming convetions and the need to change filename arises.

This function allows to change the name of the file/note the current buffer visits and all corresponding links in other notes in the org-directory. Use it carefully.

## FAQ

- Isn't this a rediciulous waste of time? Why bother?

Certainly. Newer versions of org-roam are far more advanced that this system. And if one does not like org-roam for one reason or another, there still are Denote or ZK to try out.

Personally, the way how org-roam v1 operated really clicked for me. I liked the idea that my notes would be a collection of many small text files. The mandatory use of org-id in org-roam v2 made it difficult to know where links in the notes would be directing to. A potential conversion to Markdown or something else would also be much harder - in short (and I might be incorrect about this) the changes between v1 and v2 seemed to make org-roam less future-proof, while offering little additional benefit for my personal use-case.

- But you could have continued to use org-roam v1!

True and that is what I did for a long time. But every time I (re-)installed my setup, e.g. installing org-roam v1 on a new machine or upgrading Emacs itself, I encountered some obscure issues with my time-frozen setup of org-roam v1. In particular in Emacs 29 I had issues with emacsql and everything database related.

- Is that all?

No. I also wanted to learn more elisp. Executing a small project like this seemed to be a good way to do so. 

- What does orgrr stand for?

Orgrr is an acronmy for "org-roam ripgrep" or "org-roam replica", as org-roam calls itself a [roam-research](https://roamresearch.com) replica. 

- Is this subtile critizism on the org-roam approach?

Not at all! The project was born out of the admiration of the pioneering work done by Jethro Kuan and others. You should check out the real org-roan. 
