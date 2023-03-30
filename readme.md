# orgrr 

Orgrr is an almost feature-complete replica of the core functioniality of org-roam v1 built around ripgrep (rg), a lot of regex and hashtables. It does recognize `#+roam_alias`, and `#+roam_tags`. Orgrr only works with org-files (i.e. files ending in `.org`).

Orgrr's only functions are:

- orgrr-find will find a `.org` note file in the `org-directory` ([see here](#chatgpt-in-org-mode))

- orgrr-insert will insert a like to another note find in the `org-directory` ([see here](#chatgpt-in-org-mode))

- orgrr-backlinks will show all backlinks (=links from other notes to this note) in a sidebuffer  ([see here](#chatgpt-in-org-mode))

_Note: In order to use this you'll need [rg](https://github.com/BurntSushi/ripgrep) installed on your machine. The easiest way to do so might be homebrew, i.e. "brew install rg"._

------------------------------

## Table of Contents

- [Orgrr (and org-roam v1) approach to notes](#Orgrr-(and-org-roam-v1)-approach-to-notes)
- [FAQ](#faq)




## Orgrr (and org-roam v1) approach to notes

### Basic design of a note

In orgrr all notes are assumed to follow a certain logic in that they include some metadata. At the very minimum, a note file for orgrr is an .org file that includes the following line:

```org
#+title:       title of a note
```

One of the unique strengths of org-roam v1 was the inclusion of `alias` for this title. This allows to add abbreviations or translated names to the original note. Orgrr also recognizes these alias, which have to be in quotation marks.

```org
#+roam_alias:  "alias 1" "alias 2"
```

Orgrr also recognizes tags in the same way as org-roam v1 did, separate from org-tags used throughout the document. I use this to add a very limited set of meta-data to my notes (type of source, date reading the source, status of processing the info). Tags are added without quotation marks, separated by space.

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


## FAQ

- Isn't this a rediciulous waste of time? Why bother?

Certainly. Newer versions of org-roam are far more advanced that this system. And if one does not like org-roam for one reason or another, there still is Denote or ZK to try out.

Personally, the way how org-roam v1 operated really clicked for me. I liked the idea that my notes would be a collection of many small text files. The mandatory use of org-id in org-roam v2 made it difficult to know where links in the notes would be directing to. A potential conversion to Markdown or something else would also be much harder - in short (and might be incorrect about this) the changes between v1 and v2 seemed to make org-roam less future-proof, while offering little additional benefit for me.

- But you could have continued to use org-roam v1!

True and that is what I did for a long time. But everytime I (re-)installed my setup and with every change of Emacs I encountered some obscure issues with my time-frozen setup of org-roam v1. In particular I had issues with emacsql and everything database related.

- Is that all?

No. I also wanted to learn more elisp. Doing a small project like this seems to be best way doing this. 
