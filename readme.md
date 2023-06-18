# orgrr - org-roam-ripgrep

![orgrr-show-backlinks](/orgrr-show-backlinks.png)

Orgrr is a minimalist but complete note-taking system for Emacs. Its intended purpose is the creation and management of a [Zettelkasten-like system](https://www.youtube.com/watch?v=qRSCKSPMuDc). 

These are the primary functions orgrr provides:

- **orgrr-find** will find and open a note, i.e. a `.org` file with a #+title, in the `org-directory` ([see here](#orgrr-find)). If the title (or alias) entered does not exist, a new note is created. 

- **orgrr-insert** will insert a link to another note in the `org-directory` ([see here](#orgrr-insert)). If the title (or alias) entered does not exist, a new note is created.

- **orgrr-show-backlinks** will show all backlinks (=links from other notes to the note in the current buffer) in a side-window ([see here](#orgrr-show-backlinks)). This what you see in the image above.

- **orgrr-show-related-notes** will show all related notes in a side-window ([see here](#orgrr-show-related-notes)). For the applied concept of "relationship", see [orgrr-related-notes](#orgrr-related-notes). 

- **orgrr-add-to-project** and **orgrr-open-project** are for note management and quick access to a limited number of notes.

------------------------------

## Table of Contents

- [Installation](#installation)
- [Orgrr's way of dealing with notes](#orgrr's-way-of-dealing-with-notes)
  - [Origin story](#origin-story)
  - [Basic design of a note](#basic-design-of-a-note)
  - [orgrr-projects](#orgrr-projects)
  - [orgrr-related-notes](#orgrr-related-notes)
  - [orgrr-containers](#orgrr-containers)
- [Functions](#functions)
  - [orgrr-find](#orgrr-find)
  - [orgrr-insert](#orgrr-insert)
  - [orgrr-show-backlinks](#orgrr-show-backlinks)
  - [orgrr-rename](#orgrr-rename)
  - [orgrr-delete](#orgrr-delete)
  - [orgrr-add-to-project and orgrr-open-project](#orgrr-add-to-project-and-orgrr-open-project)
  - [orgrr-container-commands](#orgrr-container-commands)
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

In order to use Orgrr you'll need [rg](https://github.com/BurntSushi/ripgrep) installed on your machine. The easiest way to do so might be [homebrew](https://brew.sh), i.e. `brew install rg`.

Finally, you may also want to set keybindings for the main functions:

```org
(global-set-key (kbd "M-s-f") 'orgrr-find)
(global-set-key (kbd "M-s-i") 'orgrr-insert)
(global-set-key (kbd "M-s-l") 'orgrr-show-backlinks)
(global-set-key (kbd "M-s-a") 'orgrr-add-to-project)
(global-set-key (kbd "M-s-p") 'orgrr-open-project)
(global-set-key (kbd "M-s-r") 'orgrr-show-related-notes)
```

## Orgrr's way of dealing with notes

### Origin story

Orgrr is an almost feature-complete replica of the core functionality of [org-roam v1](https://github.com/org-roam/org-roam-v1) built using [ripgrep](https://github.com/BurntSushi/ripgrep) (rg), a lot of regex and hashtables. It does recognize alternative note titles (`#+roam_alias`) and tags (`#+roam_tags`) as introduced by org-roam. Orgrr currently only works with [org-files](https://orgmode.org) (i.e. files ending in .org).

A crucial difference between org-roam and orgrr is the use of databases. Orgrr only relies on rg to update it's data about org-files and their meta-data. I have about 3000 notes and the speed between org-roam and orgrr is comparable. 

This is a very basic package to address my needs. If you are interested in a less minimalist and more comprehensive note taking experience you may want to check out [org-roam](https://www.orgroam.com), [Denote](https://github.com/protesilaos/denote) or [ZK](https://github.com/localauthor/zk). 

### Basic design of a note

In orgrr all notes are assumed to follow a certain logic regarding metadata. The design principles used here are similar to org-roam v1 and interoperation between orgrr and org-roam v1 is possible (and was intended). Hence filenames themselves are used as unique identifiers and changing them without adjusting backlinks will break the connection between two notes (see also [orgrr-rename](#orgrr-rename)).

At the very minimum, a note file for orgrr is an .org file that includes the following line:

```org
#+title:       title of a note
```

One of the unique strengths of org-roam v1 was the inclusion of `alias` for the title of a note. This allows to add abbreviations or translated names to the original note. Orgrr also recognizes these alias, which have to be in quotation marks.

```org
#+roam_alias:  "alias 1" "alias 2"
```

Orgrr also recognizes tags in the same way as org-roam v1 did, separate from regular org-tags. This has changed in org-roam v2, when org-roam began to use org-tags. I still prefer the approach of v1.

Tags can be very useful to add a very limited set of meta-data to your notes (type of source, date reading the source, status of processing the info). There is no auto-completion for tags, so stick to a few you remember.

Tags are added without quotation marks, separated by space.

```org
#+roam_tags:   tag1 tag2 tag3
```

In total, orgrr therefore recognizes these three lines of meta-data in an org-file:

```org
#+title:       title of a note
#+roam_alias:  "alias 1" "alias 2"
#+roam_tags:   tag1 tag2 tag3
```

### orgrr-projects

One feature that felt missing in orgrr (and org-roam) was a way to bring together a collection of notes and snippets from backlinks and many other places to create a "desktop" of notes. In the imagery of the Zettelkasten this would be a place to look at several "Zettel" / notes at the same time. Orgrr-projects is an approach to deal with this problem. 

On the most basic level, an orgrr-project is any note that has the tag `orgrr-project`:

```org
#+title:     title of the note/collection
#+roam_tags: orgrr-project
```

The function [orgrr-add-to-project](#orgrr-add-to-project) takes the current line visited at point (the cursor) and appends it to the chosen project. A source-link is added, to allow for follow-up. If the project name does not yet exist, a new orgrr-project is created in the org-directory (similar to the way orgrr-find and orgrr-insert operate).

Orgrr-projects provide rapid access to a set of current notes and are the main holding area for work in progress in orgrr. 

### orgrr-related-notes

There are many different attempts to surface related notes in note-takings systems in Emacs (and outside of it). Most of them draw on some variation of text-analysis and algorithmic determination of proximity. I always felt that the links I personally add to notes represent an underused asset for determining proximity. This function collects all notes related (via links) to the current note to the second degree - it collects the backlinks for the backlinks and the outgoing links mentioned by outgoing links. To use the image of a family, it considers all parents and grandparents as well as all children and grandchildren of a note. All links to a specific note are counted and the resulting list is ranked by frequency. This is much quicker (about 10 times) than the excellent [org-similarity](https://github.com/brunoarine/org-similarity) and still produces very interesting results. See the example below:

![orgrr-show-related-notes](/orgrr-show-related-notes.png)

### orgrr-containers

Another feature that felt missing in orgrr (and org-roam v1) was the option to keep several different sets of data. [Obsidian](https://obsidian.md)'s [vaults](https://help.obsidian.md/Getting+started/Create+a+vault) is an example of this idea and has been the inspiration for orgrr-containers. Each orgrr-container is a folder containing org-files (that may have sub-folders with org-files of their own). 

Please note that this works by changing the "org-directory" through `orgrr-change-container`. The file `~/orgrr-container-list` contains a list of all containers (to which you can add and remove containers by using `orgrr-create-container` and `orgrr-remove-container`, see also [orgrr-container-commands](#orgrr-container-commands)). If your setup is anything like mine, this also will affect your org-agenda (for me this is a feature). 

If you have set a org-directory in your .emacs, this will always be the starting point for orgrr after a restart of emacs.

## Functions

### orgrr-find

This will search the org-directory (and all its subdirectories) for a note. I use [Helm](https://github.com/emacs-helm/helm) for completion and this works smoothly. You can search for a combination of tags and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, then a new one with the chosen title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. In other words, you should use orgrr-find and orgrr-insert to create new notes.

I have decided against the use of org-capture to create new notes as this adds a lot of complexity for very little gain. If you want to abort the creation process you should invoke `kill-current-buffer`.

### orgrr-insert

This will search the org-directory (and all its subdirectories) for a note and then insert a link to this note at point. You can search for a combination of tags and title (or alias). A marked region is recognized to narrow search.

If the note does not exist, a new one with the same title will be created in the `org-directory`. The naming scheme of the new file is similar to org-roam v1. The link to the new note will also be added at point. As above mentioned, you should use orgrr-find and orgrr-insert to create new notes and should use `kill-current-buffer` to abort.

### orgrr-show-backlinks

This will show all backlinks for the note in the current buffer in a side-window. The buffer here is temporary. You can navigate this just as a regular org document and, for example, jump between headlines by `org-next-visible-headline` or `org-previous-visible-headline` (or pressing "n" and "p"). The headline link takes you to the line of the snippet in the source document. You can invoking the command again to close the side-window (while visiting this buffer).

### orgrr-show-related-notes

This will show all related notes for the note in the current buffer in a side-window (see also [orgrr-related-notes](#orgrr-related-notes)). The buffer here is temporary. You can navigate this just as a regular org document and, for example, jump between headlines by `org-next-visible-headline` or `org-previous-visible-headline` (or pressing "n" and "p"). The headline link takes you to the source document. You can invoking the command again to close the side-window (while visiting this buffer).

### orgrr-rename

Orgrr uses file names as unique indentifiers. Therefore changing them will break the connection between notes - changing the #+title of a note (or any other meta-data about a note) will not cause any harm. In theory there should be no need to ever change the name of a file (or its location) after its creation. But sometimes there are stupid typos or naming conventions and the need to change a file name arises.

This function allows to change the name of the file/note the current buffer visits and all corresponding links in other notes in the org-directory. Use it with caution.

### orgrr-delete

This function deletes the current note and shows the previous buffer. Links are not changed.

### orgrr-add-to-project and orgrr-open-project

`Orgrr-add-to-project` appends the current line to an orgrr-project and includes a source-link to allow follow-up. All links within this snippet are corrected to work in the new location. This function works for all org-files in `org-directory` and in the `orgrr-backlinks` buffer.

`orgrr-open-project` provides quick access to all orgrr-projects.

### orgrr-container-commands

`orgrr-create-container` offers to select or create a directory to be used as a container, adds this container to the container list switches to it. `orgrr-remove-container` removes a specific container from the list and switches back to "main". `orgrr-change-container` allows to switch between the containers. 

## FAQ

- Isn't this a ridiciulous waste of time? Why bother?

Certainly. The newest version of org-roam is far more advanced that this system. And if one does not like org-roam, for one reason or another, there still are Denote or ZK to try out. There is at leasr one more project that uses rg, [gkroam](https://github.com/Kinneyzhang/gkroam), which I only learned about after orgrr was done. 

Personally, the way how org-roam v1 operated really clicked for me. I liked the idea that my notes would be a collection of many small text files. The mandatory use of org-id in org-roam v2 made it difficult to know where links in the notes would be directing to. A potential conversion to Markdown or something else would also be much harder - in short (and I might be incorrect about this) the changes between v1 and v2 seemed to make org-roam less future-proof, while offering little additional benefit for my personal use-case.

- But you could have continued to use org-roam v1!

True and that is what I did for a long time. But every time I (re-)installed my setup, e.g. installing org-roam v1 on a new machine or upgrading Emacs itself, I encountered some obscure issues with my time-frozen setup of org-roam v1. In particular in Emacs 29 I had issues with emacsql and everything database related.

- Is that all?

No. I also wanted to learn more elisp. A small project like this seemed to be a good way to start. 

- What does orgrr stand for?

Orgrr is an acronym for "org-roam ripgrep" or "org-roam replica", as org-roam calls itself a [roam-research](https://roamresearch.com) replica. 

- Is this a subtile criticism direct at the org-roam approach?

Not at all! The project was born out of the admiration of the pioneering work done by Jethro Kuan and others. You should check out the real org-roam. 
