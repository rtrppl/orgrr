;; orgrr.el --- org-roam-replica or org-roam-ripgrep -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: emacs "26", rg
;; Keywords: org-roam notes 

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; orgrr is an almost feature-complete replica of the core functions 
;; of org-roam v1 (insert, find, and show-backlinks) built around 
;; ripgrep (rg), a lot of regex and hashtables. It does recognize 
;; #+roam_alias, and #+roam_tags. A typical note in org-roam v1 and 
;; orgrr starts like this:
;;
;;  #+title:       title of a note
;;  #+roam_alias:  "alias 1" "alias 2"
;;  #+roam_tags:   tag1 tag2 tag3
;;  #+roam_key:    somekey
;;
;; #+roam_key: is not yet incorporated. 
;;
;;
;; This is my first Elisp package, be kind :)
;;
;;; News
;;
;;  Version 0.1.0
;;  Initial version
;;
;;; Code:


(defun orgrr-show-backlinks ()
  "Shows all backlinks in org-directory to the current org-file."
;; TODO: add unlinked references below backlinks!
  (interactive)
  (orgrr-get-meta)
  (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
     (pcase (org-collect-keywords '("TITLE"))
       (`(("TITLE" . ,val))
          (setq title (car val))))
 (setq backlinks 0)
 (setq orgrr-counter-quote (make-hash-table :test 'equal))
 (setq orgrr-counter-filename (make-hash-table :test 'equal))
     (with-temp-buffer
       (insert (shell-command-to-string (concat "rg -e '" (file-name-nondirectory filename) "' " org-directory " -n -g \"*.org\"")))
       (let ((result '())
            (current-entry "")
            (lines (split-string (buffer-string) "\n" t)))
	 (dolist (line lines)
           (if (string-match "^\\(.*?\\):\\(.*\\)$" line)
               (progn
 		(setq backlinks (+ backlinks 1))
		(puthash backlinks (match-string 1 line) orgrr-counter-filename)
		(puthash backlinks (match-string 2 line) orgrr-counter-quote))))))
;; match-string 2 includes the line number!
      (with-current-buffer (get-buffer-create "*Orgrr Backlinks*")
        (let ((inhibit-read-only t))
          (erase-buffer)
           (insert (concat "\*\[\[file:" filename "\]\[" title "\]\]\*\n\n"))
           (if (= backlinks 1)
               (insert "* 1 Backlink\n\n")      
           (insert (concat "* " (number-to-string backlinks) " Backlinks\n\n")))
;; Going through the backlinks
           (dolist (counter (hash-table-keys orgrr-counter-filename))
	     (setq entry (gethash counter orgrr-counter-filename))
             (when (and (stringp entry)
                        (not (string= entry filename)))
               (let ((key entry)
                     (value (gethash counter orgrr-counter-quote)))
                 (when (stringp value)
		   (let ((result (gethash (concat "\\" key) orgrr-filename-title)))
		     (string-match "^\\(.*?\\):\\(.*\\)$" value)
                     (insert (concat "\*\* \[\[file:" key "::" (match-string 1 value) "\]" "\[" result "\]\]:\n\n" (match-string 2 value) "\n\n"))))))))
         (display-buffer-in-side-window
          (current-buffer)
          '((side . right)
            (slot . -1)
            (window-width . 60)))
 (with-current-buffer "*Orgrr Backlinks*"
       (org-mode))))
 (let ((window (get-buffer-window "*Orgrr Backlinks*")))
  (when window
    (select-window window)
    (beginning-of-buffer)
    (next-line 4)))
(clrhash orgrr-counter-quote)
(clrhash orgrr-counter-filename)
(clrhash orgrr-filename-title))

(defun orgrr-get-meta ()
  "Gets the value for #+TITLE:/#+title, #+roam_alias and #+roam_tags for all org-files and adds them to hashtables."
  (setq current-entry "")
  (setq orgrr-title-filename (make-hash-table :test 'equal))
  (setq orgrr-filename-title (make-hash-table :test 'equal))
  (setq orgrr-filename-tags (make-hash-table :test 'equal))
  (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -i --sort modified \"^\\#\\+(title:.*)|(roam_alias.*)|(roam_tags.*)\" " org-directory " -g \"*.org\"")))
      (goto-char (point-min))
          (while (not (eobp))
	    (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
;; The following checks if this is a #+title line and is so, adds the title + filename to orgrr-title-filename and filename + title to orgrr-filename-title.
	    (if (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	     (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	     (setq title (car (cdr (cdr line))))
	     (puthash title filename orgrr-title-filename)
	     (puthash (concat "\\" filename) title orgrr-filename-title)))
;; The following checks if this is a #+roam_alias line and if so, adds all alias + filename to orgrr-alias-filename.
	    (if (string-match "\\(#\\+roam_alias:\\|#+ROAM_ALIAS:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	       (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	       (with-temp-buffer
		 (setq alias "")
		 (insert current-entry)
		 (goto-char (point-min))
		 (while (re-search-forward "\"\\(.*?\\)\\\"" nil t)
		   (puthash (match-string 1) filename orgrr-title-filename)))))
;; The following checks if the line is about tags and if so copies the tags to orgrr-tags-filename.
	     (if (string-match "\\(#\\+roam_tags:\\|#+ROAM_TAGS:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	     (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	     (setq tags (car (cdr (cdr line))))
	     (puthash (concat "\\" filename) tags orgrr-filename-tags)))
(forward-line))))

(defun orgrr-selection ()
 "This function prepares the variable orgrr-selection for completing-read and sends the result in selection to orgrr-find and orgrr-insert. It prepends tags in front of title and alias."
 (setq orgrr-selection-list ())
 (orgrr-get-meta)
 (setq titles (hash-table-keys orgrr-title-filename))
 (setq filenames-for-titles (hash-table-values orgrr-title-filename))
 (setq filenames-for-tags (hash-table-keys orgrr-filename-tags))
 (dolist (title titles)
  (setq filename (gethash title orgrr-title-filename)) 
  (if (member (concat "\\" filename) filenames-for-tags) 
      (setq orgrr-selection-list (cons (concat "(" (gethash (concat "\\" filename) orgrr-filename-tags) ")" " " title) orgrr-selection-list))
   (setq orgrr-selection-list (cons title orgrr-selection-list))))
(setq orgrr-selection-list (reverse orgrr-selection-list))
(if (region-active-p)
    (setq selection (completing-read "" orgrr-selection-list nil nil  (buffer-substring-no-properties (region-beginning)(region-end))))
(setq selection (completing-read "" orgrr-selection-list)))
(if (string-match "^\(" selection)
    (setq selection (replace-regexp-in-string "\(.*?\) " "" selection))))

(defun orgrr-find ()
  "Finds org-file in org-directory via mini-buffer completion. If the selected file name does not exist, a new one is created."
  (interactive)
  (orgrr-selection)
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection))))
	 (find-file (concat filename ".org"))
	 (insert (concat "#+title: " selection "\n\n"))))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-filename-tags))

(defun orgrr-insert ()
  "Links to org-file in org-directory via mini-buffer completion. If the selected file name does not exist, a new one is created."
  (interactive)
  (setq path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))
  (orgrr-selection)
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (setq filename (file-relative-name filename path-of-current-note))
      (insert (concat "\[\[file:" filename "\]\[" selection "\]\]")))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection))))
         (insert (concat "\[\[file:" filename ".org" "\]\[" selection "\]\]"))
	 (find-file (concat filename ".org"))
	 (insert (concat "#+title: " selection "\n\n"))))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-filename-tags))

(defun orgrr-rename ()
  "Renames current file and changes all backlinks. Don't change directories!"
  (interactive)
  (let ((old-filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
    (let ((new-filename (read-from-minibuffer "Filename to change: " old-filename)))      
	      (rename-file old-filename new-filename)
	      (set-visited-file-name new-filename)
	      (shell-command-to-string (concat "rg -e '" (file-name-nondirectory old-filename) "' " org-directory "-r " (file-name-nondirectory new-filename))))))
