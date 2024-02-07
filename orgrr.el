;; orgrr.el --- org-roam-replica or org-roam-ripgrep -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.7.2
;; Package-Requires: emacs "26", rg
;; Keywords: org-roam notes zettelkasten

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
;; intended purpose is the creation and management of a Zettelkasten-like system,
;; e.g. many small notes that can easily be linked together.
;;
;;
;;
;;; News
;;
;; 0.7.2
;; - Adds the function orgrr-fix-all-links-container
;; 
;; 0.7.1
;; - When moving notes between containers, links are now adjusted.
;;   The function orgrr-fix-all-links-buffer may be used to fix links,
;;   if a file is manually moved between containers.
;;
;;; Code:

(defvar orgrr-window-management "multi-window")

(defun orgrr-open-file (filename)
  "A wrapper to open FILENAME either with find-file or find-file-other-window."
  (if (equal orgrr-window-management "multi-window")
      (find-file-other-window filename)
    (if (equal orgrr-window-management "single-window")
        (find-file filename)
      (find-file-other-window filename))))

(defun orgrr-toggle-single-window-mode ()
  "Switch between single-window-mode and multi-window mode (which uses side-buffers)."
  (interactive)
  (if (equal orgrr-window-management "multi-window")
      (progn
	(setq old-org-link-frame-setup org-link-frame-setup)
	(setq orgrr-window-management "single-window")
	(setq org-link-frame-setup '((file . find-file))))
    (progn
      (setq orgrr-window-management "multi-window")
      (setq org-link-frame-setup old-org-link-frame-setup))))

(defun on-macos-p ()
  "Check if Emacs is running on macOS. This became necessary due to some normalization issues with filenames that contain non-ascii characters and require NCD-formating."
  (eq system-type 'darwin))

(defun orgrr-show-backlinks ()
  "Show all backlinks in `org-directory' to the current org-file."
;; TODO: add unlinked references below backlinks!
  (interactive)
  (orgrr-get-all-filenames)
  (if (not (string-match-p "backlinks for *" (buffer-name (current-buffer))))
      (progn
	(orgrr-get-meta)
	(let ((filename (if (equal major-mode 'dired-mode)
                            default-directory
			  (buffer-file-name))))
	  (pcase (org-collect-keywords '("TITLE"))
	    (`(("TITLE" . ,val))
             (setq title (car val))))
	  (setq backlink-buffer (concat "backlinks for *" title "*"))
	  (setq backlinks 0)
	  (setq orgrr-counter-quote (make-hash-table :test 'equal))
	  (setq orgrr-counter-filename (make-hash-table :test 'equal))
	  (with-temp-buffer
	    (if (on-macos-p)
	    (insert (shell-command-to-string (concat "rg -e '" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "' " org-directory " -n --sort accessed -g \"*.org\"")))
	    (insert (shell-command-to-string (concat "rg -e '" (file-name-nondirectory filename) "' " org-directory " -n --sort accessed -g \"*.org\""))))
	    (let ((lines (split-string (buffer-string) "\n" t)))
	      (dolist (line lines)
		(if (string-match "^\\(.*?\\):\\(.*\\)$" line)
		    (progn
 		      (setq backlinks (+ backlinks 1))
		      (puthash backlinks (match-string 1 line) orgrr-counter-filename)
		      (puthash backlinks (match-string 2 line) orgrr-counter-quote))))))
	  ;; match-string 2 includes the line number!
	  (with-current-buffer (get-buffer-create backlink-buffer)
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
			(let* ((line-number (match-string 1 value))
			       (snippet (match-string 2 value))
			       (snippet (orgrr-adjust-links snippet))
			       (snippet (string-trim-left (string-trim-left snippet "*"))))
			(insert (concat "\*\* \[\[file:" key "::" line-number "\]" "\[" result "\]\]:\n\n"  snippet "\n\n")))))))))
            (if (equal orgrr-window-management "multi-window")
		(progn
		  (display-buffer-in-side-window
		   (current-buffer)
		   '((side . right)
		     (slot . -1)
		     (window-width . 60)))))
	    (if (equal orgrr-window-management "single-window")
		  (switch-to-buffer backlink-buffer))
	    (with-current-buffer backlink-buffer
	      (org-mode))))
	(let ((window (get-buffer-window backlink-buffer)))
	  (when window
	    (select-window window)
	    (setq default-directory org-directory)
	    (beginning-of-buffer)
	    (org-next-visible-heading 2)
	    (deactivate-mark)))
	(clrhash orgrr-counter-quote)
	(clrhash orgrr-counter-filename)
	(clrhash orgrr-filename-title))
     (if (equal orgrr-window-management "multi-window")
	 (delete-window))
     (if (equal orgrr-window-management "single-window")
	 (previous-buffer))))

(defun orgrr-get-meta ()
  "Gets the value for #+TITLE:/#+title, #+roam_alias and #+roam_tags for all org-files and adds them to hashtables."
  (setq current-entry "")
  (setq orgrr-title-filename (make-hash-table :test 'equal))
  (setq orgrr-filename-title (make-hash-table :test 'equal))
  (setq orgrr-filename-tags (make-hash-table :test 'equal))
  (setq orgrr-short_filename-filename (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert (shell-command-to-string (concat "rg -i --sort accessed \"^\\#\\+(title:.*)|(roam_alias.*)|(roam_tags.*)\" " org-directory " -g \"*.org\"")))
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
		(puthash (concat "\\" filename) title orgrr-filename-title)
		(puthash (concat "\\" (file-name-nondirectory filename)) filename orgrr-short_filename-filename)))))
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
;; The following checks if the line contains tags and if so copies the tags to orgrr-tags-filename.
	     (if (string-match "\\(#\\+roam_tags:\\|#+ROAM_TAGS:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	     (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		    (filename (car line))
		    (tags (car (cdr (cdr line)))))
	       (puthash (concat "\\" filename) tags orgrr-filename-tags))))
(forward-line))))

(defun orgrr-selection ()
  "Prepare the symbol orgrr-selection for completing-read and send the result in selection to orgrr-find and orgrr-insert. Prepends tags in front of title and alias."
  (setq orgrr-selection-list ())
  (orgrr-get-meta)
  (setq titles (hash-table-keys orgrr-title-filename))
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
  "Find org-file in `org-directory' via mini-buffer completion. If the selected file name does not exist, a new one is created."
  (interactive)
  (orgrr-selection)
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (orgrr-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[\"':;\\\s\/]" "_" selection))))
      (orgrr-open-file (concat filename ".org")))
    (insert (concat "#+title: " selection "\n")))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-short_filename-filename)
(clrhash orgrr-filename-tags))

(defun orgrr-insert ()
  "Links to org-file in `org-directory' via mini-buffer completion. If the selected file name does not exist, a new one is created."
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
         (filename (concat org-directory time "-" (replace-regexp-in-string "[\"':;\\\s\/]" "_" selection))))
      (if (on-macos-p)
	  (setq filename (ucs-normalize-HFS-NFD-string filename)))
      (if (region-active-p)
	  (kill-region (region-beginning) (region-end)))
      (insert (concat "\[\[file:" (file-relative-name filename path-of-current-note) ".org" "\]\[" selection "\]\]"))
      (orgrr-open-file (concat filename ".org"))
      (insert (concat "#+title: " selection "\n"))))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-short_filename-filename)
(clrhash orgrr-filename-tags))

(defun orgrr-random-note ()
  "Opens random org-file in `org-directory'."
  (interactive)
  (orgrr-get-meta)
  (let* ((titles (hash-table-keys orgrr-title-filename))
	 (filenames-for-titles (hash-table-values orgrr-title-filename))
	 (random-title (elt titles (random (length titles))))
         (filename (gethash random-title orgrr-title-filename)))
    (orgrr-open-file filename))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-short_filename-filename)
(clrhash orgrr-filename-tags))

(defun orgrr-rename ()
  "Rename current file and change all backlinks. Does not work across directories."
  (interactive)
  (let ((old-filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
    (let ((new-filename (read-from-minibuffer "Filename to change: " old-filename)))
	      (rename-file old-filename new-filename)
	      (set-visited-file-name new-filename)
	       (if (on-macos-p)
		   (shell-command-to-string (concat "rg -e '" (ucs-normalize-HFS-NFD-string (file-name-nondirectory old-filename)) "' " org-directory "-r " (ucs-normalize-HFS-NFD-string (file-name-nondirectory new-filename))))
		 (shell-command-to-string (concat "rg -e '" (file-name-nondirectory old-filename) "' " org-directory "-r " (file-name-nondirectory new-filename)))))))

(defun orgrr-delete ()
  "Delete current note and show the previous buffer."
  (interactive)
  (if (buffer-file-name)
      (if (yes-or-no-p (format "Are you sure you want to delete the note %s? " (buffer-file-name)))
	  (progn
	    (delete-file (buffer-file-name))
	    (message "Note deleted!")
	    (kill-current-buffer)))
  (message "This is not a note!")))

(defun orgrr-move-note ()
  "Move current note to one of the other containers."
  (interactive)
  (setq orgrr-name-container (make-hash-table :test 'equal))
  (if (buffer-file-name)
      (setq filename (buffer-file-name)))
  (with-temp-buffer
    (insert-file-contents "~/.orgrr-container-list")
    (if (fboundp 'json-parse-buffer)
	(setq orgrr-name-container (json-parse-buffer))))
  (setq containers (hash-table-keys orgrr-name-container))
  (setq selection (completing-read "Move note to which countainer? " containers))
  (if (member selection containers)
      (progn
	(setq new-container (gethash selection orgrr-name-container))
	(if (yes-or-no-p (format "Are you sure you want to move the note %s? " (buffer-file-name)))
	    (progn
	      (kill-buffer)
	      (rename-file filename (concat new-container "/" (file-name-nondirectory filename)))
	      (orgrr-adjust-backlinks-in-current-container filename)
	      (setq org-directory new-container)
	      (orgrr-open-file (concat new-container "/" (file-name-nondirectory filename))) 
	      (orgrr-fix-all-links-buffer)
	      (save-some-buffers t)
	    (message "Note has been moved and links have been adjusted!"))
	  (message "Note not moved!")))
    (message "Container does not exist."))
  (clrhash orgrr-name-container))

(defun orgrr-open-project ()
  "Find existing project or create a new one."
  (interactive)
  (orgrr-pick-project)
  (setq titles (hash-table-keys orgrr-title-filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S")))
         (setq filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s\/]" "_" selection) ".org")))
	 (with-current-buffer (orgrr-open-file filename)
	 (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))
(clrhash orgrr-counter-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-short_filename-filename)
(clrhash orgrr-project_filename-title))

(defun orgrr-collect-project-snippet ()
  "Prepare snippet for `orgrr-add-to-project'."
  (if (not (string-match-p "backlinks for *" (buffer-name (current-buffer))))
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
  "Add the current line in the buffer (including orgrr-backlinks buffer) to an existing project."
  (interactive)
  (orgrr-get-meta)
  (orgrr-collect-project-snippet)
  (orgrr-format-project-snippet snippet)
  (orgrr-pick-project)
  (orgrr-get-all-filenames)
  (setq titles (hash-table-keys orgrr-title-filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (find-file-noselect filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S")))
         (setq filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s\/]" "_" selection) ".org")))
	 (with-current-buffer (find-file-noselect filename)
	 (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))
    (with-current-buffer (find-file-noselect filename)
  (setq path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))
  (setq footnote (car (split-string (replace-regexp-in-string "^file:" "" footnote-link) "::")))
  (setq footnote (file-relative-name footnote default-directory))
  (setq footnote-line (string-to-number (car (cdr (split-string (replace-regexp-in-string "^file:" "" footnote-link) "::")))))
  (goto-char (point-max))
   (insert (concat "\n\"" (string-trim (orgrr-adjust-links project-snippet)) "\"" "\t" "(Source: \[\[file:" (concat footnote "::" (number-to-string footnote-line)) "\]\[" footnote-description "\]\]" ")"))
   (save-buffer))
(clrhash orgrr-filename-title)
(clrhash orgrr-short_filename-filename)
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
	   (setq selection (replace-regexp-in-string "\(.*?\) " "" selection)))))

(defun orgrr-format-project-snippet (snippet)
  "Formats an orgrr-project SNIPPET."
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

(defun orgrr-get-all-filenames ()
  "Collects the name all of org-files across all containers and adds them to the hashtable orgrr-short_filename-filename. This is needed to correct the links of a snippet created in one container for use in another via orgrr-add-to-project. 

A use case could be to add snippets to a writing project, which is located in a different container than the main database."
  (orgrr-check-for-container-file)
  (ogrr-get-list-of-containers)
  (setq orgrr-short_filename-filename (make-hash-table :test 'equal))
  (let* ((containers (nreverse (hash-table-values orgrr-name-container))))
    (dolist (container containers) 
      (with-temp-buffer
	(insert (shell-command-to-string (concat "rg -i --sort accessed \"^\\#\\+(title:.*)\" " container " -g \"*.org\"")))
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((current-entry (buffer-substring (line-beginning-position) (line-end-position))))
	    (if (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
		(progn
		  (let* ((line (split-string current-entry "\\(:#\\+title:\\|:#+TITLE:\\)\\s-*\\(.+\\)" t))
		       (filename (car line)))
		    (puthash (concat "\\" (file-name-nondirectory filename)) filename orgrr-short_filename-filename)))))
(forward-line))))))
	 
(defun orgrr-adjust-links (string)
  "Adjusts/corrects all links of STRING relative to the position of the note."
  (setq path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))
  (setq default-directory path-of-current-note)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
      (let* ((filename (file-name-nondirectory (match-string 1))))
	     (if (member (concat "\\" filename) (hash-table-keys orgrr-short_filename-filename))
		 (progn 
		   (let* ((new-filename (gethash (concat "\\" filename) orgrr-short_filename-filename)))
		     (replace-match (concat "file:" (file-relative-name new-filename path-of-current-note))))))))
    (buffer-string)))
    
(defun orgrr-info ()
 "Show the amount of titles considered by orgrr."
 (interactive)
 (let ((result (benchmark-run-compiled 1
                 (progn
                   (orgrr-get-meta)
                   (setq titles (hash-table-keys orgrr-title-filename))))))
   (message "Orgrr considers %d titles in this container (this includes titles and alias). Collecting all titles took %s seconds to complete." (length titles) (format "%.5f" (car result)))))
    
(defun orgrr-show-related-notes ()
  "Show all related notes in `org-directory' to the current org-file. Related means here notes linking to this note and the notes that link to them as well as notes linked by the current note and the links from these notes. It is assumed that the more times a note in environment is mentioned, the more important it is. Notes of higher importance are listed at the top. Parents and grandparents as well as children and grandchildren."
  (interactive)
  (if (not (string-match-p "related notes for *" (buffer-name (current-buffer))))
      (progn
	(setq related-notes 0)
	(setq counter 0)
	(setq orgrr-filename-mentions (make-hash-table :test 'equal))
	(orgrr-get-meta)
	(orgrr-backlinks-first-and-second-order)
	(setq relatednotes-buffer (concat "related notes for *" title "*"))
	(orgrr-forwardlinks-first-and-second-order)
	(with-current-buffer (get-buffer-create relatednotes-buffer)
	  (erase-buffer)
	  (if (equal orgrr-window-management "multi-window")
	      (progn
		(display-buffer-in-side-window
		 (current-buffer)
		 '((side . right)
		   (slot . -1)
		   (window-width . 60)))))
	  (if (equal orgrr-window-management "single-window")
	      (switch-to-buffer relatednotes-buffer))
	  (org-mode)
	  (insert (concat "* " (number-to-string related-notes) " connections for *" title "*\n\n"))
	  (setq sorted-values '())
	  (maphash (lambda (key value)
		     (push (cons value key) sorted-values))
		   orgrr-filename-mentions)
	  (setq sorted-values (sort sorted-values (lambda (a b) (> (car a) (car b)))))
	  (dolist (entry sorted-values)
	    (insert (concat "** " "\[\[file:" (substring (cdr entry) 1) "\]\[" (gethash (cdr entry) orgrr-filename-title) "\]\]: " (number-to-string (car entry)) "\n")))
	(let ((win (get-buffer-window relatednotes-buffer)))
	  (select-window win)
	  (beginning-of-buffer)
	  (org-next-visible-heading 1)
	  (deactivate-mark)))
	(clrhash orgrr-title-filename)
	(clrhash orgrr-filename-title)
	(clrhash orgrr-filename-tags)
	(clrhash orgrr-short_filename-filename)
	(clrhash orgrr-filename-mentions))
    (if (equal orgrr-window-management "multi-window")
	(delete-window))
    (if (equal orgrr-window-management "single-window")
	(previous-buffer))))
  

(defun orgrr-backlinks-first-and-second-order ()
  "Gets backlinks first and second order."
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
		    (buffer-file-name))))
    (pcase (org-collect-keywords '("TITLE"))
      (`(("TITLE" . ,val))
       (setq title (car val))))
    (setq original-filename filename)
    ;; get all backlinks first order
    (with-temp-buffer
       (if (on-macos-p)
	   (insert (shell-command-to-string (concat "rg -l -e '" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "' " org-directory " -n -g \"*.org\"")))
	 (insert (shell-command-to-string (concat "rg -l -e '" (file-name-nondirectory filename) "' " org-directory " -n -g \"*.org\""))))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (if (string-match "\\.org$" line)
	      (progn
		(if (not (equal original-filename line))
		    (progn
		      (if (not (member (concat "\\" line) (hash-table-keys orgrr-filename-mentions)))
			  (progn
			    (puthash (concat "\\" line) 1 orgrr-filename-mentions)
			    (setq related-notes (+ related-notes 1)))
			(progn
			  (setq counter (gethash (concat "\\" line) orgrr-filename-mentions))
			  (setq counter (+ counter 1))
			  (setq related-notes (+ related-notes 1))
			  (puthash (concat "\\" line) counter orgrr-filename-mentions)))))))))))
  ;; get all backlinks second order
  (setq entry "")
  (dolist (entry (hash-table-keys orgrr-filename-mentions))
    (setq filename (substring entry 1))
    (with-temp-buffer
       (if (on-macos-p)
	   (insert (shell-command-to-string (concat "rg -l -e '" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "' " org-directory " -n -g \"*.org\"")))
	 (insert (shell-command-to-string (concat "rg -l -e '" (file-name-nondirectory filename) "' " org-directory " -n -g \"*.org\""))))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (if (string-match "\\.org$" line)
	      (progn
		(if (not (equal original-filename line))
		    (progn
		      (if (not (member (concat "\\" line) (hash-table-keys orgrr-filename-mentions)))
			  (progn
			    (puthash (concat "\\" line) 1 orgrr-filename-mentions)
			    (setq related-notes (+ related-notes 1)))
			(progn
			  (setq counter (gethash (concat "\\" line) orgrr-filename-mentions))
			  (setq counter (+ counter 1))
			  (setq related-notes (+ related-notes 1))
			  (puthash (concat "\\" line) counter orgrr-filename-mentions))))))))))))

(defun orgrr-forwardlinks-first-and-second-order ()
  "Gets backlinks first and second order."
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
		    (buffer-file-name))))
    (pcase (org-collect-keywords '("TITLE"))
      (`(("TITLE" . ,val))
       (setq title (car val))))
    (setq original-filename filename))
    (setq lines '())
    (setq contents (with-current-buffer (buffer-name)
                     (buffer-substring-no-properties (point-min) (point-max))))
;; find all foward links first order
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
	 (let* ((filename (file-name-nondirectory (match-string 1)))
		(new-filename
		  (if (on-macos-p)
		      (string-trim (shell-command-to-string (concat "rg -g \"" (ucs-normalize-HFS-NFD-string filename) "\" --files " org-directory)))
		(string-trim (shell-command-to-string (concat "rg -g \"" filename "\" --files " org-directory))))))
	   (if (not (equal original-filename new-filename))
		 (progn
		   (if (not (member (concat "\\" new-filename) (hash-table-keys orgrr-filename-mentions)))
		     (progn
		       (puthash (concat "\\" new-filename) 1 orgrr-filename-mentions)
		       (setq related-notes (+ related-notes 1)))
		   (progn
		     (setq counter (gethash (concat "\\" new-filename) orgrr-filename-mentions))
		     (setq counter (+ counter 1))
		     (setq related-notes (+ related-notes 1))
		     (puthash (concat "\\" new-filename) counter orgrr-filename-mentions)))))
;; add links second order
	       (with-temp-buffer
		 (insert-file-contents new-filename)
		 (goto-char (point-min))
		 (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
		   (let* ((2nd-filename (file-name-nondirectory (match-string 1)))
			  (2nd-new-filename
			    (if (on-macos-p)
				(string-trim (shell-command-to-string (concat "rg -g \"" (ucs-normalize-HFS-NFD-string 2nd-filename) "\" --files " org-directory)))
			      (string-trim (shell-command-to-string (concat "rg -g \"" 2nd-filename "\" --files " org-directory))))))
		      (if (not (equal original-filename 2nd-new-filename))
			  (if (not (member (concat "\\" 2nd-new-filename) (hash-table-keys orgrr-filename-mentions)))
			      (progn
				(puthash (concat "\\" 2nd-new-filename) 1 orgrr-filename-mentions)
				(setq related-notes (+ related-notes 1)))
			    (progn
			      (setq counter (gethash (concat "\\" 2nd-new-filename) orgrr-filename-mentions))
			      (setq counter (+ counter 1))
			      (setq related-notes (+ related-notes 1))
			      (puthash (concat "\\" 2nd-new-filename) counter orgrr-filename-mentions)))))))))))

(defun orgrr-change-container (&optional container)
  "Switch between a list of containers stored in ~/.orgrr-container-list. orgrr-change-container can be called with a specific container."
  (interactive)
  (orgrr-check-for-container-file)
  (ogrr-get-list-of-containers)
  (let* ((containers (nreverse (hash-table-keys orgrr-name-container))))
    (if container
	(setq selection container)
      (setq selection (completing-read "" containers)))
    (if (member selection containers)
	(setq org-directory (gethash selection orgrr-name-container))
      (message "Container does not exist.")))
  (clrhash orgrr-name-container))

(defun orgrr-check-for-container-file ()
 "Creates a container file in ~/.orgrr-container-list in case one does not yet exist."
 (if (not (file-exists-p "~/.orgrr-container-list"))
      (progn
	(puthash "main" org-directory orgrr-name-container)
	(with-temp-buffer
	  (let ((json-data (json-encode orgrr-name-container)))
	    (insert json-data)
	    (write-file "~/.orgrr-container-list"))))))

(defun ogrr-get-list-of-containers ()
 "Return orgrr-name-container, a hashtable that includes a list of names and locations of all containers."
 (setq orgrr-name-container (make-hash-table :test 'equal))
 (with-temp-buffer
   (insert-file-contents "~/.orgrr-container-list")
   (if (fboundp 'json-parse-buffer)
       (setq orgrr-name-container (json-parse-buffer)))))

(defun orgrr-create-container ()
  "Create or add a directory as a container and switch to that container."
  (interactive)
  (orgrr-check-for-container-file)
  (ogrr-get-list-of-containers)
  (let* ((new-container (read-directory-name "Enter a directory name: ")))
    (if (yes-or-no-p (format "Are you sure you want to create the directory %s as a container? " new-container))
	(progn
	  (unless (file-exists-p new-container)
	    (make-directory new-container t))
	  (let* ((name (read-from-minibuffer "Please provide a name for the new container: ")))
	    (puthash name new-container orgrr-name-container)
	    (with-temp-buffer
	     (let* ((json-data (json-encode orgrr-name-container)))
	       (insert json-data)
	       (write-file "~/.orgrr-container-list")))))
    (message "%s was not created!" new-container))
  (setq org-directory new-container)
  (clrhash orgrr-name-container)))


(defun orgrr-remove-container ()
  "Allow to remove a container for the list of containers."
  (interactive)
  (setq orgrr-name-container (make-hash-table :test 'equal))
  (if (not (file-exists-p "~/.orgrr-container-list"))
      (progn
	(puthash "main" org-directory orgrr-name-container)
	(with-temp-buffer
	  (setq json-data (json-encode orgrr-name-container))
	  (insert json-data)
	  (write-file "~/.orgrr-container-list"))))
  (with-temp-buffer
    (insert-file-contents "~/.orgrr-container-list")
    (if (fboundp 'json-parse-buffer)
	(setq orgrr-name-container (json-parse-buffer))))
  (setq containers (hash-table-keys orgrr-name-container))
  (setq selection (completing-read "Which container should be removed? " containers))
  (if (not (member selection containers))
      (message "Container does not exist.")
    (if (string-equal selection "main")
	(message "The container \"main\" cannot be removed!")
      (if (yes-or-no-p (format "Are you sure you want to remove %s as a container? " (gethash selection orgrr-name-container)))
	  (progn
	    (remhash selection orgrr-name-container)
	    (with-temp-buffer
	      (setq json-data (json-encode orgrr-name-container))
	      (insert json-data)
	      (write-file "~/.orgrr-container-list"))
	    (setq org-directory (gethash "main" orgrr-name-container))))))
  (clrhash orgrr-name-container))

(defun orgrr-change-container (&optional container)
  "Switch between a list of containers stored in ~/.orgrr-container-list. orgrr-change-container can be called with a specific container."
  (interactive)
  (orgrr-check-for-container-file)
  (ogrr-get-list-of-containers)
  (let* ((containers (nreverse (hash-table-keys orgrr-name-container))))
    (if container
	(setq selection container)
      (setq selection (completing-read "" containers)))
    (if (member selection containers)
	(setq org-directory (gethash selection orgrr-name-container))
      (message "Container does not exist.")))
  (clrhash orgrr-name-container))

(defun orgrr-check-for-container-file ()
 "Creates a container file in ~/.orgrr-container-list in case one does not yet exist."
 (if (not (file-exists-p "~/.orgrr-container-list"))
      (progn
	(puthash "main" org-directory orgrr-name-container)
	(with-temp-buffer
	  (let ((json-data (json-encode orgrr-name-container)))
	    (insert json-data)
	    (write-file "~/.orgrr-container-list"))))))

(defun ogrr-get-list-of-containers ()
 "Return orgrr-name-container, a hashtable that includes a list of names and locations of all containers."
 (setq orgrr-name-container (make-hash-table :test 'equal))
 (with-temp-buffer
   (insert-file-contents "~/.orgrr-container-list")
   (if (fboundp 'json-parse-buffer)
       (setq orgrr-name-container (json-parse-buffer)))))

(defun orgrr-create-container ()
  "Create or add a directory as a container and switch to that container."
  (interactive)
  (orgrr-check-for-container-file)
  (ogrr-get-list-of-containers)
  (let* ((new-container (read-directory-name "Enter a directory name: ")))
    (if (yes-or-no-p (format "Are you sure you want to create the directory %s as a container? " new-container))
	(progn
	  (unless (file-exists-p new-container)
	    (make-directory new-container t))
	  (let* ((name (read-from-minibuffer "Please provide a name for the new container: ")))
	    (puthash name new-container orgrr-name-container)
	    (with-temp-buffer
	     (let* ((json-data (json-encode orgrr-name-container)))
	       (insert json-data)
	       (write-file "~/.orgrr-container-list")))))
    (message "%s was not created!" new-container))
  (setq org-directory new-container)
  (clrhash orgrr-name-container)))


(defun orgrr-remove-container ()
  "Allow to remove a container for the list of containers."
  (interactive)
  (setq orgrr-name-container (make-hash-table :test 'equal))
  (if (not (file-exists-p "~/.orgrr-container-list"))
      (progn
	(puthash "main" org-directory orgrr-name-container)
	(with-temp-buffer
	  (setq json-data (json-encode orgrr-name-container))
	  (insert json-data)
	  (write-file "~/.orgrr-container-list"))))
  (with-temp-buffer
    (insert-file-contents "~/.orgrr-container-list")
    (if (fboundp 'json-parse-buffer)
	(setq orgrr-name-container (json-parse-buffer))))
  (setq containers (hash-table-keys orgrr-name-container))
  (setq selection (completing-read "Which container should be removed? " containers))
  (if (not (member selection containers))
      (message "Container does not exist.")
    (if (string-equal selection "main")
	(message "The container \"main\" cannot be removed!")
      (if (yes-or-no-p (format "Are you sure you want to remove %s as a container? " (gethash selection orgrr-name-container)))
	  (progn
	    (remhash selection orgrr-name-container)
	    (with-temp-buffer
	      (setq json-data (json-encode orgrr-name-container))
	      (insert json-data)
	      (write-file "~/.orgrr-container-list"))
	    (setq org-directory (gethash "main" orgrr-name-container))))))
  (clrhash orgrr-name-container))

(defun orgrr-fix-all-links-buffer ()
  "This runs the function orgrr-adjust-links on the current buffer."
 (interactive)
 (orgrr-get-all-filenames)
 (let ((contents (with-current-buffer (buffer-name)
                  (buffer-substring-no-properties (point-min) (point-max)))))
   (erase-buffer)
   (insert (orgrr-adjust-links contents)))
 (beginning-of-buffer))

(defun orgrr-adjust-backlinks-in-current-container (filename)
  "This is a helper function for orgrr-move-note and will adjust all links in notes in the previous/old container referring to the moving note to its new location.

This one of the very few functions where orgrr is directly changing your data (to fix the links). Be aware of this, but don't be scared."
  (save-some-buffers t)  ;; necessary, as we are working directly with the files 
  (let* ((orgrr-backlinks '()) 
	 (original-filename filename))
    ;; Add all files that mention filename to the list orgrr-backlinks.
    (with-temp-buffer
       (if (on-macos-p)
	   (insert (shell-command-to-string (concat "rg -l -e '" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "' " org-directory " -n -g \"*.org\"")))
	 (insert (shell-command-to-string (concat "rg -l -e '" (file-name-nondirectory filename) "' " org-directory " -n -g \"*.org\""))))
       (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (if (string-match "\\.org$" line)
	      (if (not (equal original-filename line))
		  (push line orgrr-backlinks))))))
;; This corrects the links. Please be aware that this is an intrusive action and might affect your data. 
  (dolist (filename orgrr-backlinks)
    (with-current-buffer (find-file-noselect filename)
      (orgrr-fix-all-links-buffer)))))

(defun orgrr-fix-all-links-container ()
   "This function can be used to fix all links in a container. This is useful, if you move a whole container/directory to a new location. Make sure to be in the correct container, when running this function."
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to fix all links in this container? "))
      (progn
	(dolist (filename (directory-files org-directory t "\\.org$"))
	  (with-current-buffer (find-file-noselect filename)
	    (message "Fixing links in '%s'." filename)
	    (orgrr-fix-all-links-buffer)
	    (message "Fixing backlinks for '%s'." filename)
	    (orgrr-adjust-backlinks-in-current-container filename)))
	(message "All links in this container have been adjusted!"))))

       

(provide 'orgrr)

;;; orgrr.el ends here
