;; orgrr.el --- org-roam-replica or org-roam-ripgrep -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: 
;; Version: 0.2.0
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

;; Orgrr is a minimalist but complete note-taking system for Emacs. Its
;; intended purpose is the creation and management of a Zettelkasten-like,
;; e.g. many small notes that can easily be linked together. 
;;
;;
;;
;;; News
;;
;;  Version 0.2.1
;;  Added: the buffer *Orgrr Backlinks* closes, when orgrr-show-backlinks 
;;  is invoked while *Orgrr Backlinks* is the active buffer. Fixed handling
;;  of colons in title.
;;  Version 0.2.0
;;  Added support for orgrr-projects
;;
;;; Code:


(defun orgrr-show-backlinks ()
  "Shows all backlinks in org-directory to the current org-file."
;; TODO: add unlinked references below backlinks!
  (interactive)
  (if (not (equal (buffer-name (current-buffer)) "*Orgrr Backlinks*"))
      (progn
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
	    (setq default-directory org-directory)
	    (beginning-of-buffer)
	    (next-line 4)))
	(clrhash orgrr-counter-quote)
	(clrhash orgrr-counter-filename)
	(clrhash orgrr-filename-title))
    (delete-window)))

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
	       (let* ((line (split-string current-entry "\\(:#\\+title:\\|:#+TITLE:\\)\\s-*\\(.+\\)" t))
		      (filename (car line)))
		 (let* ((line (split-string current-entry "^.+\\(#\\+title:\\|:#+TITLE:\\)\\s-*" t))  
			(title (car line)))
		   (puthash title filename orgrr-title-filename)
		   (puthash (concat "\\" filename) title orgrr-filename-title)))))
;; The following checks if this is a #+roam_alias line and if so, adds all alias to orgrr-title-filename.
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
	     (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		    (filename (car line))
		    (tags (car (cdr (cdr line)))))
	       (puthash (concat "\\" filename) tags orgrr-filename-tags))))
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
         (filename (concat org-directory time "-" (replace-regexp-in-string "[\"':;\\\s]" "_" selection))))
	 (find-file-other-window (concat filename ".org"))
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
      (if (region-active-p)
	  (kill-region (region-beginning) (region-end)))
      (insert (concat "\[\[file:" filename "\]\[" selection "\]\]")))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat time "-" (replace-regexp-in-string "[\"':;\\\s]" "_" selection))))
      (if (region-active-p)
	  (kill-region (region-beginning) (region-end)))
      (insert (concat "\[\[file:" filename ".org" "\]\[" selection "\]\]"))
      (find-file-other-window (concat filename ".org"))
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

(defun orgrr-delete ()
  "Deletes the current note and shows the previous buffer."
  (interactive)
  (if (buffer-file-name)
      (if (yes-or-no-p (format "Are you sure you want to delete the note %s? " (buffer-file-name)))
	  (progn
	    (delete-file (buffer-file-name))
	    (message "Note deleted!")
	    (kill-current-buffer)))
  (message "This is not a note!")))

(defun orgrr-open-project ()
  "Finds or creates new project."
  (interactive)
  (orgrr-pick-project)
  (setq titles (hash-table-keys orgrr-title-filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S")))
         (setq filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection) ".org")))
	 (with-current-buffer (find-file-other-window filename)
	 (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))
(clrhash orgrr-counter-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-project_filename-title))

(defun orgrr-collect-project-snippet ()
  "Adds the current line in orgrr-backlinks or buffer to a project."
  (if (not (string= (buffer-name) "*Orgrr Backlinks*"))
    (progn
      (save-excursion
      (setq line-number (line-number-at-pos))
      (setq filename (buffer-file-name))
      (pcase (org-collect-keywords '("TITLE"))
	(`(("TITLE" . ,val))
         (setq title (car val))))
      (beginning-of-line)
      (set-mark-command nil)
      (end-of-line)
      (setq snippet (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq snippet (concat "\*\* \[\[file:" filename "::" (number-to-string line-number)  "\]" "\[" title "\]\]:\n" snippet))
      (deactivate-mark)))
    (progn
      (let ((start (save-excursion
                 (org-back-to-heading)
                 (point)))
        (end (save-excursion
               (org-end-of-subtree)
               (point))))
    (setq snippet (buffer-substring-no-properties start end))))))

(defun orgrr-add-to-project ()
  "Adds the current paragraph in orgrr-backlinks or buffer to a project."
  (interactive)
  (orgrr-collect-project-snippet)
  (orgrr-format-project-snippet snippet)
  (orgrr-pick-project)
  (setq titles (hash-table-keys orgrr-title-filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (find-file-noselect filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S")))
         (setq filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection) ".org")))
	 (with-current-buffer (find-file-noselect filename)
	 (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))
    (with-current-buffer (find-file-noselect filename)
  (setq path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))
   (goto-char (point-max))
   (setq footnote-link (file-relative-name (replace-regexp-in-string "^file:" "" footnote-link) path-of-current-note))
   (insert (concat "\n\"" (string-trim (orgrr-adjust-links project-snippet)) "\"" "\t" "(Source: \[\[file:" footnote-link "\]\[" footnote-description "\]\]" ")"))
   (save-buffer))
(clrhash orgrr-filename-title)
(clrhash orgrr-project_filename-title))

(defun orgrr-pick-project ()
  "Provides a list of all projects to add the new snippet, with the option to create a new one."
  (orgrr-get-meta)
  (setq orgrr-selection-list ())
  (setq orgrr-project_filename-title (make-hash-table :test 'equal))
     (with-temp-buffer
       (insert (shell-command-to-string (concat "rg -i --sort modified -l -e  \"^\\#\\+roam_tags:.+orgrr-project\" " org-directory)))
       (let ((result '())
            (current-entry "")
            (lines (split-string (buffer-string) "\n" t)))
	 (dolist (line lines)
	   (let ((title (gethash (concat "\\" line) orgrr-filename-title)))
	   (puthash (concat "\\" line) title orgrr-project_filename-title)
	    (setq orgrr-selection-list (cons title orgrr-selection-list)))))
       (setq selection (completing-read "" orgrr-selection-list))
       (if (string-match "^\(" selection)
	   (setq selection (replace-regexp-in-string "\(.*?\) " "" selection))))
(clrhash orgrr-project_filename-title)
(clrhash orgrr-filename-title))

(defun orgrr-format-project-snippet (snippet)
  "Formats an orgrr-project snippet."
  (with-temp-buffer
    (insert snippet)
    (goto-char (point-min))
    (while (not (eobp))
      (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
	  (if (string-match "^\\*\\* \\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" current-entry)
	  (progn
          (setq footnote-link (match-string 1 current-entry))
          (setq footnote-description (match-string 2 current-entry))
	  (kill-whole-line 1))
	(forward-line)))
    (setq project-snippet (buffer-string))))
	 
(defun orgrr-adjust-links (string)
  "Adjusts/corrects all links of copied text relative to the position of the note"
  (setq current-buffer-directory (file-name-directory (buffer-file-name)))
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
      (let* ((filename (file-name-nondirectory (match-string 1)))
	    (new-filename (string-trim (shell-command-to-string (concat "rg -g \"" filename "\" --files " org-directory)))))
	(replace-match (concat "file:" (file-relative-name new-filename current-buffer-directory)))))
    (buffer-string)))
    
(defun orgrr-info ()
 "This function shows the amount of titles considered by orgrr."
 (interactive)
 (let ((result (benchmark-run-compiled 1
                 (progn
                   (orgrr-get-meta)
                   (setq titles (hash-table-keys orgrr-title-filename))))))
   (message "Orgrr considers %d titles (this includes titles and alias). Collecting all titles took %s seconds to complete." (length titles) (format "%.5f" (car result)))))
    
