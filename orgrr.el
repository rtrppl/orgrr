;;; orgrr.el --- A fast and feature-complete Zettelkasten -*- lexical-binding: t; -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL: https://github.com/rtrppl/orgrr
;; Version: 0.9.18
;; Package-Requires: ((emacs "27.2"))
;; Keywords: comm wp outlines 

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
;; intended purpose is the creation and management of a Zettelkasten-like 
;; system, e.g. many small notes that can easily be linked together.
;;
;;
;;
;;; News
;;
;; 0.9.18
;; - Added `orgrr-add-to-other-window'
;;
;; 0.9.17
;; - Added option to use active region for `orgrr-add-to-project'
;;
;; 0.9.16 
;; - Added convience: typing "q" in any results buffer now closes that buffer
;; (via a minor-mode)
;;
;; 0.9.15
;; - Added `orgrr-show-multiverse'; modified `orgrr-show-sequence' to 
;; also show parent zettel
;;
;; 0.9.14
;; - Added functions `orgrr-quick-add', `orgrr-global-quick-add', 
;; `orgrr-rename-title-and-file', `orgrr-rename-and-move'; fixed bug in 
;; `orgrr-rename' that could lead to duplicates when renaming
;;
;; 0.9.13
;; - `orgrr-open-project', `orgrr-add-to-project' and `orgrr-info' are now global and
;; work across containers
;;
;; For more changes see the changelog.
;;
;;; Code:

(require 'ucs-normalize)
(require 'org)
(require 'json)

(defvar orgrr-window-management "single-window")
(defvar orgrr-quick-add-token "quicknote")
(defvar orgrr-compile-open-link-other-window t) ;; set this to nil if orgrr-compile-draft should respect orgrr-window-management settings


;; The following list of hashtables create the data structure in which orgrr stores metadata on notes.
(defvar orgrr-title-filename (make-hash-table :test 'equal) "Hashtable with the key title and the value filename.")
(defvar orgrr-filename-title (make-hash-table :test 'equal) "Hashtable with the the key filename and the value title.")
(defvar orgrr-filename-tags (make-hash-table :test 'equal) "Hashtable with the key filename and the value tags.")
(defvar orgrr-short_filename-filename (make-hash-table :test 'equal) "Hashtable with the key filename without path and the value filename+path.")
(defvar orgrr-zettel-filename (make-hash-table :test 'equal) "Hashtable with the key zettel and the value filename.") 
(defvar orgrr-filename-zettel (make-hash-table :test 'equal) "Hashtable with the key filename and the value zettel.")  
(defvar orgrr-zettelrank-zettel (make-hash-table :test 'equal) "Hashtable with the key rank of a zettel and the value zettel.") ;; rank means how a zettel value of a note relates to other notes
(defvar orgrr-zettel-zettelrank (make-hash-table :test 'equal) "Hashtable with the key zettel and the value rank of a zettel.")
(defvar orgrr-short_filename-filename (make-hash-table :test 'equal) "Hashtable containing all org-files accross all containers.")
(defvar orgrr-short_filename-title (make-hash-table :test 'equal) "Hashtable containing filenames-titles for all org-files accross all containers.")
(defvar orgrr-title-short_filename (make-hash-table :test 'equal) "Hashtable containing titles-filenames for all org-files accross all containers.")
(defvar orgrr-filename-mentions (make-hash-table :test 'equal) "Hashtable necessary for orgrr-show-related-notes.") 

(define-minor-mode orgrr-results-buffer-mode
  "A minor mode for orgrr results buffers."
  :lighter " orgrr-results-buffer"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'orgrr-close-buffer)
            map))
  

(defun orgrr-open-file (filename)
  "A wrapper to open FILENAME either with find-file or find-file-other-window."
  (if (equal orgrr-window-management "multi-window")
      (find-file-other-window filename)
    (if (equal orgrr-window-management "single-window")
        (find-file filename)
      (find-file-other-window filename))))

(defun orgrr-open-buffer (buffer)
 "A wrapper to open BUFFER according to orgrr-window-management settings."
 (when (equal orgrr-window-management "multi-window")
		  (display-buffer-in-side-window
		   (current-buffer)
		   '((side . right)
		     (slot . -1)
		     (window-width . 60))))
 (when (equal orgrr-window-management "single-window")
   (switch-to-buffer buffer)))

(defun orgrr-prepare-findings-buffer (buffer)
 "Preparing the orgrr findings buffer."
 (with-current-buffer buffer
   (org-mode))
 (let ((window (get-buffer-window buffer)))
   (select-window window)
   (goto-char (point-min))
   (org-next-visible-heading 2)
   (orgrr-results-buffer-mode t)
   (deactivate-mark)))

(defun orgrr-close-buffer ()
   "A wrapper to close BUFFER according to orgrr-window-management settings."
   (interactive)
   (when (equal orgrr-window-management "multi-window")
     (kill-buffer))
   (when (equal orgrr-window-management "single-window")
     (kill-buffer)))

(defun orgrr-toggle-window-mode ()
  "Switch between single-window-mode and multi-window mode (which uses 
side-buffers)."
  (interactive)
  (if (equal orgrr-window-management "multi-window")
      (progn
	(setq orgrr-window-management "single-window")
	(setq org-link-frame-setup '((file . find-file))))
    (progn
      (setq orgrr-window-management "multi-window")
      (setq org-link-frame-setup '((file . find-file-other-window))))))

(defun orgrr-on-macos-p ()
  "Check if Emacs is running on macOS. This became necessary due to some 
normalization issues with filenames that contain non-ascii characters and 
require NCD-formating."
  (eq system-type 'darwin))

(defun orgrr-show-backlinks (arg)
  "Show all backlinks in `org-directory' to the current org-file."
  (interactive "P")
  (let ((call-with-arg nil))
    (when (equal arg '(4))
      (setq call-with-arg 1))
  (if (not (string-match-p "backlinks for *" (buffer-name (current-buffer))))
      (progn
	(orgrr-get-all-meta)
	(let* ((filename (if (equal major-mode 'dired-mode)
                         default-directory
			 (buffer-file-name)))
	       (title (pcase (org-collect-keywords '("TITLE"))
		    (`(("TITLE" . ,val)) (car val))))
	       (backlink-buffer (concat "backlinks for *" title "*"))
	       (backlinks 0)
	       (orgrr-counter-quote (make-hash-table :test 'equal))
	       (orgrr-counter-filename (make-hash-table :test 'equal))
	       (orgrr-name-container (orgrr-get-list-of-containers))
	       (containers (nreverse (hash-table-values orgrr-name-container))))
    ;; get all backlinks first order
	  (with-temp-buffer
	     (when (not call-with-arg)
	       (setq containers ())
	       (setq containers (cons org-directory containers)))
	     (dolist (container containers)
	       (erase-buffer)
	       (if (orgrr-on-macos-p)
		(insert (shell-command-to-string (concat "rg -e \"" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "\" \"" (expand-file-name container) "\" -n --sort accessed -g \"*.org\"")))
		(insert (shell-command-to-string (concat "rg -e \"" (file-name-nondirectory filename) "\" \"" (expand-file-name container) "\" -n --sort accessed -g \"*.org\""))))
	    (let ((lines (split-string (buffer-string) "\n" t)))
	      (dolist (line lines)
		(when (string-match "^\\(.*?\\):\\(.*\\)$" line)
 		  (setq backlinks (+ backlinks 1))
		  (puthash backlinks (match-string 1 line) orgrr-counter-filename)
		  (puthash backlinks (match-string 2 line) orgrr-counter-quote))))))
	  ;; match-string 2 includes the line number!
      (with-current-buffer (get-buffer-create backlink-buffer)
              (erase-buffer)
	      (orgrr-open-buffer backlink-buffer)
              (insert (concat "\*\[\[file:" filename "\]\[" title "\]\]\*\n\n"))
              (if (= backlinks 1)
		  (insert "* 1 backlink\n\n")
		(insert (concat "* " (number-to-string backlinks) " backlinks\n\n")))
	      ;; Going through the backlinks
              (dolist (counter (hash-table-keys orgrr-counter-filename))
		(let ((entry (gethash counter orgrr-counter-filename)))
		  (when (and (stringp entry)
                             (not (string= entry filename)))
		    (let ((key entry)
			  (value (gethash counter orgrr-counter-quote)))
                      (when (stringp value)
			(let* ((short_filename (file-name-nondirectory key))
			       (full-filename key)
			       (result (gethash (concat "\\" short_filename) orgrr-short_filename-title)))
			  (string-match "^\\(.*?\\):\\(.*\\)$" value)
			  (let* ((line-number (match-string 1 value))
				 (snippet (match-string 2 value))
				 (snippet (orgrr-adjust-links snippet))
				 (snippet (string-trim-left (string-trim-left snippet "*"))))
			    (insert (concat "\*\* \[\[file:" full-filename "::" line-number "\]" "\[" result "\]\]:\n\n"  snippet "\n\n")))))))))
	      (orgrr-prepare-findings-buffer backlink-buffer))))
    (orgrr-close-buffer))))

(defun orgrr-get-meta ()
  "Gets the value for #+title, #+roam_alias, #+roam_tags and #+zettel for all 
org-files and adds them to hashtables.

Updates the following hashtables: orgrr-title-filename, orgrr-filename-title, 
orgrr-zettel-filename, orgrr-filename-zettel, orgrr-filename-tags."
  (clrhash orgrr-filename-title)
  (clrhash orgrr-title-filename)
  (clrhash orgrr-zettel-filename)
  (clrhash orgrr-filename-zettel)
  (clrhash orgrr-filename-tags)
  (with-temp-buffer
    (insert (shell-command-to-string (concat "rg -i --sort modified \"^\\#\\+(title:.*)|(roam_alias.*)|(roam_tags.*)|(zettel:.*)\" \"" (expand-file-name org-directory) "\" -g \"*.org\"")))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-entry (buffer-substring (line-beginning-position) (line-end-position))))
      ;; The following checks if this is a #+title line and is so, adds the title + filename to orgrr-title-filename and filename + title to orgrr-filename-title.
	(when (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
	      (let* ((line (split-string current-entry "\\(:#\\+title:\\|:#+TITLE:\\)\\s-*\\(.+\\)" t))
		   (filename (car line))
		   (line (split-string current-entry "^.+\\(#\\+title:\\|:#+TITLE:\\)\\s-*" t))
		   (title (car line)))
		(puthash title filename orgrr-title-filename)
		(puthash (concat "\\" filename) title orgrr-filename-title)))
	;; The following checks if this is a #+roam_alias line and if so, adds all alias to orgrr-title-filename.
	(when (string-match "\\(#\\+roam_alias:\\|#+ROAM_ALIAS:\\)\\s-*\\(.+\\)" current-entry)
	  (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		 (filename (car line)))
	    (with-temp-buffer
	      (insert current-entry)
	      (goto-char (point-min))
	      (while (re-search-forward "\"\\(.*?\\)\\\"" nil t)
		(puthash (match-string 1) filename orgrr-title-filename)))))
;; The following checks if this is a #+zettel line and if so, adds the zettel-no to orgrr-zettel-filename.
	(when (string-match "\\(#\\+zettel:\\|#+ZETTEL:\\)\\s-*\\(.+\\)" current-entry)
	  (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		 (filename (car line))
		 (zettel (car (cdr (cdr line)))))   
	    (puthash zettel filename orgrr-zettel-filename)
	    (puthash (concat "\\" filename) zettel orgrr-filename-zettel)))
;; The following checks if the line contains tags and if so copies the tags to orgrr-tags-filename.
	(when (string-match "\\(#\\+roam_tags:\\|#+ROAM_TAGS:\\)\\s-*\\(.+\\)" current-entry)
	  (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		 (filename (car line))
		 (tags (car (cdr (cdr line)))))
	    (puthash (concat "\\" filename) tags orgrr-filename-tags)))
(forward-line)))))

;; orgrr-presorted-completion-table is based on 
;; https://emacs.stackexchange.com/questions/8115/make-completing-read
;; -respect-sorting-order-of-a-collection, thanks @sachac@emacs.ch for the hint!
;; lambda functions and let for whatever reason don't like each other,
;; hence this construction.

(defun orgrr-presorted-completion-table (orgrr-selection-list)
  (let ((orgrr-selection-list-completion ()))
    (setq orgrr-selection-list-completion 
	  (lambda (string pred action)
	    (if (eq action 'metadata)
		`(metadata (display-sort-function . ,#'identity))
	    (complete-with-action action orgrr-selection-list string pred))))
orgrr-selection-list-completion))

(defun orgrr-selection ()
  "Prepare the symbol orgrr-selection for completing-read and send the result 
in selection to orgrr-find and orgrr-insert. Prepends tags and zettel in front 
of title and alias."
  (orgrr-get-meta)
  (let* ((orgrr-selection-list ())
	 (orgrr-selection-list-completion)
         (final-title)
	 (selection)
	 (titles (hash-table-keys orgrr-title-filename))
	 (filenames-for-tags (hash-table-keys orgrr-filename-tags))
	 (filenames-for-zettel (hash-table-keys orgrr-filename-zettel)))
    (dolist (title titles)
      (let* ((filename (gethash title orgrr-title-filename)))
	(if (member (concat "\\" filename) filenames-for-tags)
	    (if (member (concat "\\" filename) filenames-for-zettel)
		(setq final-title (concat "[" (gethash (concat "\\" filename) orgrr-filename-zettel) "]" " (" (gethash (concat "\\" filename) orgrr-filename-tags) ") " title))
	      (setq final-title (concat "(" (gethash (concat "\\" filename) orgrr-filename-tags) ") "title)))
	  (if (member (concat "\\" filename) filenames-for-zettel)
	      (setq final-title (concat "[" (gethash (concat "\\" filename) orgrr-filename-zettel) "] " title))
	  (setq final-title title)))
	(setq orgrr-selection-list (cons final-title orgrr-selection-list))))
    (setq orgrr-selection-list (reverse orgrr-selection-list))
    (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
    (if (region-active-p)
	(setq selection (completing-read "Select: " orgrr-selection-list-completion nil nil  (buffer-substring-no-properties (region-beginning)(region-end))))
      (setq selection (completing-read "Select: " orgrr-selection-list-completion)))
    (if (string-match "^\\[" selection)
	(setq selection (replace-regexp-in-string "\\[.*?\\]\\s-*" "" selection)))  
    (if (string-match "^\(" selection)
	(setq selection (replace-regexp-in-string "\(.*?\)\\s-*" "" selection)))
  selection)) ;; this line ensures that the value of selection is returned when this function is called

(defun orgrr-global-selection ()
  "Use data from all containers to prepare the symbol orgrr-selection for 
completing-read and send the result in selection to orgrr-find and 
orgrr-insert. 

Does not prepend tags and zettel in front of title and alias."
  (orgrr-get-all-meta)
  (let* ((orgrr-selection-list-completion)
	 (selection)
	 (orgrr-selection-list (hash-table-keys orgrr-title-short_filename)))
    (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
    (if (region-active-p)
	(setq selection (completing-read "Select: " orgrr-selection-list-completion nil nil  (buffer-substring-no-properties (region-beginning)(region-end))))
      (setq selection (completing-read "Select: " orgrr-selection-list-completion)))
  selection)) ;; this line ensures that the value of selection is returned when this function is called

(defun orgrr-selection-zettel (&optional current-zettel)
  "Prepare the symbol orgrr-selection for completing-read and send the result 
in selection to `orgrr-find-zettel' and `orgrr-insert-zettel'. Only includes files 
that have a value for zettel. Prepends zettel value in front of title and 
alias."
  (let* ((orgrr-selection-list (orgrr-prepare-zettel-selection-list))
	(orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
	(selection))
    (if current-zettel
      (setq selection (completing-read "Select: " orgrr-selection-list-completion nil nil current-zettel))
      (setq selection (completing-read "Select: " orgrr-selection-list-completion)))
    (if (string-match "^\\[\\(.*?\\)\\]" selection)
      	(setq selection (replace-regexp-in-string "\\[.*?\\]\\s-*" "" selection)))
    selection))

(defun orgrr-prepare-zettel-selection-list ()
  "A function preparing a list of all zettel for selection and other functions."
  (interactive)
  (orgrr-get-meta)
  (let* ((orgrr-selection-list ())
	 (titles (hash-table-keys orgrr-title-filename))
	 (filenames-for-zettel (hash-table-keys orgrr-filename-zettel))
	 (final-title))
    (dolist (title titles)
      (let* ((filename (gethash title orgrr-title-filename)))
	(if (member (concat "\\" filename) filenames-for-zettel)
	    (progn 
	      (setq final-title (concat "[" (gethash (concat "\\" filename) orgrr-filename-zettel) "]\s" title))
	      (setq orgrr-selection-list (cons final-title orgrr-selection-list))))))
    (setq orgrr-selection-list (reverse orgrr-selection-list))))

(defun orgrr-find-zettel ()
  "Like `orgrr-find', but only considers notes that have a value for zettel. If 
the selected file name does not exist, a new one is created. Starts with the 
current zettel ID, which allows you to search within a context."
  (interactive)
  (let ((selection (orgrr-selection-zettel)))
    (when (member selection (hash-table-keys orgrr-title-filename))
      (let ((filename (gethash selection orgrr-title-filename)))
	(orgrr-open-file filename)))
    (when (not (member selection (hash-table-keys orgrr-title-filename)))
      (let* ((time (format-time-string "%Y%m%d%H%M%S"))
	     (filename (concat (file-name-as-directory org-directory) time "-" (replace-regexp-in-string "[\"'?,:;\\\s\/]" "_" selection))))
	(orgrr-open-file (concat filename ".org"))
	(insert (concat "#+title: " selection "\n"))))))

(defun orgrr-no-selection-zettel ()
  "Prepare the symbol orgrr-selection for completing-read and send the result 
in selection to orgrr-find and orgrr-insert. Excludes zettel. "
  (interactive)
  (orgrr-get-meta)
  (let*  ((orgrr-selection-list ())
	  (orgrr-selection-list-completion)
	  (final-title)
	  (selection)
	  (titles (hash-table-keys orgrr-title-filename))
	  (filenames-for-tags (hash-table-keys orgrr-filename-tags))
	  (filenames-for-zettel (hash-table-keys orgrr-filename-zettel)))
    (dolist (title titles)
      (let* ((filename (gethash title orgrr-title-filename)))
	(if (member (concat "\\" filename) filenames-for-tags)
	    (if (not (member (concat "\\" filename) filenames-for-zettel))
		(progn 
		  (setq final-title (concat "(" (gethash (concat "\\" filename) orgrr-filename-tags) ") " title))
		  (setq orgrr-selection-list (cons final-title orgrr-selection-list)))))
	(if (not (member (concat "\\" filename) filenames-for-tags))
	    (if (not (member (concat "\\" filename) filenames-for-zettel))
		(progn 
		  (setq final-title title)
		  (setq orgrr-selection-list (cons final-title orgrr-selection-list)))))))
    (setq orgrr-selection-list (reverse orgrr-selection-list))
    (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
    (if (region-active-p)
	(setq selection (completing-read "Select: " orgrr-selection-list-completion nil nil  (buffer-substring-no-properties (region-beginning)(region-end))))
      (setq selection (completing-read "Select: " orgrr-selection-list-completion))) 
    (if (string-match "^\(" selection)
	(setq selection (replace-regexp-in-string "\(.*?\)\\s-*" "" selection)))
    selection))

(defun orgrr-no-find-zettel ()
  "Like orgrr-find-zettel, but only considers notes that have no value for 
zettel."
  (interactive)
  (let ((selection (orgrr-no-selection-zettel))
	(filename))
    (when (member selection (hash-table-keys orgrr-title-filename))
      (setq filename (gethash selection orgrr-title-filename))
      (orgrr-open-file filename))))

(defun orgrr-add-zettel ()
  "Drill down a list of notes with values for zettel to allow the user to find 
the correct spot for a new zettel and then insert a line with #+zettel: 
zettel-value after the last line starting with #+ (from the beginning of the 
file)."
  (interactive)
  (orgrr-get-meta)
  (let ((current-zettel (orgrr-read-current-zettel)))
    (if (not current-zettel)
	(progn
	  (let ((orgrr-selection-list ())
		(orgrr-selection-list-completion)
		(selection-zettel)
		(inserted-already)
		(final-title)
		(titles (hash-table-keys orgrr-title-filename))
		(filenames-for-zettel (hash-table-keys orgrr-filename-zettel))
		(saved-point (point-marker)))
	    (dolist (title titles)
	  (let* ((filename (gethash title orgrr-title-filename)))
	    (when (member (concat "\\" filename) filenames-for-zettel)
	      (setq final-title (concat "[" (gethash (concat "\\" filename) orgrr-filename-zettel) "]\s" title))
	      (setq orgrr-selection-list (cons final-title orgrr-selection-list)))))
	    (setq orgrr-selection-list (reverse orgrr-selection-list))
	    (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
	    (setq selection-zettel (completing-read "Hit enter to narrow down: " orgrr-selection-list-completion))
	    (if (string-match "^\\[\\(.*?\\)\\]" selection-zettel)
		(setq selection-zettel (match-string 1 selection-zettel)))
    (while (member selection-zettel (hash-table-values orgrr-filename-zettel))
      (setq selection-zettel (completing-read "Hit enter to narrow down: " orgrr-selection-list-completion nil nil selection-zettel))
      (when (string-match "^\\[\\(.*?\\)\\]" selection-zettel)
	(setq selection-zettel (match-string 1 selection-zettel))))
    (goto-char (point-min))
    (setq inserted-already nil)
    (while (not inserted-already)
      (let ((current-entry (buffer-substring (line-beginning-position) (line-end-position))))
	(if (string-prefix-p "#+" current-entry)
	    (forward-line)
	  (progn
	    (setq inserted-already t)
	    (insert (concat "#+zettel: " selection-zettel "\n"))
	    (goto-char saved-point)
	    (save-buffer)))))))
  (message "This note already has a value for zettel!"))))
	 
(defun orgrr-read-current-zettel ()
    "Returns #+zettel for current note."
    (let ((current-zettel nil))
      (when (eq major-mode 'org-mode)
	(let ((current-entry "")
	      (buffer (buffer-substring-no-properties (point-min) (point-max))))
	  (with-temp-buffer
	    (insert buffer)
	    (goto-char (point-min))
	    (while (not (eobp))
              (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
              (if (string-match "\\(#\\+zettel:\\|#+ZETTEL:\\)\\s-*\\(.+\\)" current-entry)
		  (progn
		    (let* ((line (split-string current-entry "\\: " t))
			   (zettel (car (cdr line)))
			   (zettel (string-trim-left zettel)))
                      (setq current-zettel zettel))))
            (forward-line)))))
      current-zettel))
    
(defun orgrr-show-sequence (&optional zettel-title)
  "Shows a sequence of notes for any given zettel value. If run while visiting 
a buffer that has a value for zettel, this is taken as the starting value for 
zettel. Results are presented in a different buffer in accordance with the
variable orgrr-window-management."
  (interactive)
  (when (not (string-match-p "sequence for *" (buffer-name (current-buffer))))
    (orgrr-prepare-zettelrank)
    (let* ((current-zettel (orgrr-read-current-zettel))
	   (zettel-title (or zettel-title (orgrr-selection-zettel current-zettel)))
	   (zettel-filename (gethash zettel-title orgrr-title-filename))
	   (selection-zettel (gethash (concat "\\" zettel-filename) orgrr-filename-zettel))
	   (parent-zettel (orgrr-read-zettel-parent selection-zettel))
	   (orgrr-zettel-list (hash-table-values orgrr-filename-zettel))
	   (orgrr-zettel-list (sort orgrr-zettel-list 'orgrr-dictionary-lessp))
	   (sequence-buffer (concat "sequence for *[" selection-zettel "]*")))
      (with-current-buffer (get-buffer-create sequence-buffer)
	(let ((inhibit-read-only t))
          (erase-buffer)
	  (insert (concat (orgrr-return-fullzettel-linked-head selection-zettel) "\n\n"))
	  (insert "* sequence:\n\n")
	  (if (not (string-equal parent-zettel ""))
	      (insert (concat "** parent zettel: \t" (orgrr-return-fullzettel-linked parent-zettel) "\n\n"))
	    (insert "** This is a root zettel with no parent.\n\n"))
	  (dolist (element orgrr-zettel-list) 
	    (let* ((last-char (substring selection-zettel -1))
		    (is-last-char-num (string-match-p "[0-9]" last-char))
		    (regex (if is-last-char-num
			       (concat "^" selection-zettel "[a-zA-Z]")
			     (concat "^" selection-zettel))))
	    (when (string-match regex element)
	      (if (not (equal element selection-zettel))
		  (insert (concat "** " (orgrr-return-fullzettel-linked element) "\n")))))))
;;Starting here it is only window-management
	    (orgrr-open-buffer sequence-buffer) 
	    (orgrr-prepare-findings-buffer sequence-buffer))))
  (when (string-match-p "sequence for *" (buffer-name (current-buffer)))
    (orgrr-close-buffer)))


;; The following three functions have been taken from 
;; https://stackoverflow.com/questions/1942045/natural-order-sort-for-emacs-lisp
;; They work really well for dictionary compare.

(defun orgrr-dictionary-lessp (str1 str2)
  "Return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them)"
  (let ((str1-components (orgrr-dict-split str1))
        (str2-components (orgrr-dict-split str2)))
    (orgrr-dict-lessp str1-components str2-components)))

(defun orgrr-dict-lessp (slist1 slist2)
  "Compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         nil)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (orgrr-dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (orgrr-dict-lessp (cdr slist1) (cdr slist2)))))))

(defun orgrr-dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data 
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun orgrr-open-next-zettel ()
  "Opens the next zettel."
  (interactive)
  (let ((current-zettel (orgrr-read-current-zettel)))
    (when current-zettel
      (orgrr-prepare-zettelrank)
      (let* ((current-zettel-rank (gethash current-zettel orgrr-zettel-zettelrank))
	     (next-rank (+ (string-to-number current-zettel-rank) 1))
             (matched-zettel (gethash (number-to-string next-rank) orgrr-zettelrank-zettel))
	     (matched-zettel-filename (gethash matched-zettel orgrr-zettel-filename)))
      	(orgrr-open-file matched-zettel-filename)))
    (when (not current-zettel)
   (message "This note has no value for zettel, so there is no next zettel!"))))

(defun orgrr-prepare-zettelrank ()
  "Prepares a hashtable that contains the rank of all zettel."
  (orgrr-get-meta)
  (let* ((zettelrank 0)	 
	 (orgrr-zettel-list (hash-table-values orgrr-filename-zettel))
	 (orgrr-zettel-list (sort orgrr-zettel-list 'orgrr-dictionary-lessp)))
    (clrhash orgrr-zettelrank-zettel)
    (clrhash orgrr-zettel-zettelrank)
    (dolist (zettel orgrr-zettel-list)
      (setq zettelrank (+ zettelrank 1))   
      (puthash (number-to-string zettelrank) zettel orgrr-zettelrank-zettel)
      (puthash zettel (number-to-string zettelrank) orgrr-zettel-zettelrank))))

(defun orgrr-return-fullzettel (zettel)
  "Returns the full name of a zettel (as in orgrr-zettel-list)."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename))
	 (matched-zettel-title (gethash (concat "\\" matched-zettel-filename) orgrr-filename-title)))
    (setq zettel (concat "\[" zettel "\]\t" matched-zettel-title))))

(defun orgrr-return-fullzettel-linked (zettel)
  "Returns the full name of a zettel (as in orgrr-zettel-list) and links the 
title to the note."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename))
	 (matched-zettel-title (gethash (concat "\\" matched-zettel-filename) orgrr-filename-title)))
    (setq zettel (concat "\[" zettel "\]\t\[\[file:" matched-zettel-filename "\]\["  matched-zettel-title "\]\]"))))

(defun orgrr-return-fullzettel-linked-starred (zettel)
  "Returns the full name of a zettel (as in orgrr-zettel-list) and links the 
title to the note. Adds stars for org-bolding."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename))
	 (matched-zettel-title (gethash (concat "\\" matched-zettel-filename) orgrr-filename-title)))
    (setq zettel (concat "*" zettel "* \[\[file:" matched-zettel-filename "\]\["  matched-zettel-title "\]\]"))))

(defun orgrr-return-fullnote-linked-starred (title)
  "Returns the full name of a zettel (as in orgrr-zettel-list) and links the 
title to the note. Adds stars for org-bolding."
  (let* ((filename (gethash title orgrr-title-filename)))
    (setq title (concat "*\[\[file:" filename "\]\[" title "\]\]*"))))

(defun orgrr-return-zettel-linked (zettel)
  "Returns the zettel as an org-link."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename)))
    (setq zettel (concat "\[\[file:" matched-zettel-filename "\]\["  zettel "\]\]"))))


(defun orgrr-return-fullzettel-linked-head (zettel)
  "A version of orgrr-return-fullzettel-linked in a special format."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename))
	 (matched-zettel-title (gethash (concat "\\" matched-zettel-filename) orgrr-filename-title)))
    (setq zettel (concat "*\[" zettel "\]*\t\[\[file:" matched-zettel-filename "\]\["  matched-zettel-title "\]\]"))))

(defun orgrr-return-fullzettel-content (zettel)
  "Returns the full content of a zettel without meta-data."
  (let* ((matched-zettel-filename (gethash zettel orgrr-zettel-filename)))
    (with-temp-buffer 
      (insert-file-contents matched-zettel-filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((current-entry (thing-at-point 'line t)))
          (if (or (string-prefix-p "#+title" current-entry t)
		  (string-prefix-p "#+roam_alias" current-entry t)
		  (string-prefix-p "#+roam_key" current-entry t)
		  (string-prefix-p "#+roam_tags" current-entry t)
		  (string-prefix-p "#+zettel" current-entry t))	       
              (delete-region (line-beginning-position) (line-end-position))
            (forward-line 1))))
      (string-trim (buffer-string)))))     

(defun orgrr-return-zettel-from-title (title)
  "Returns the zettel from a title."
  (let* ((matched-title-filename (gethash title orgrr-title-filename))
	 (matched-zettel (gethash (concat "\\" matched-title-filename) orgrr-filename-zettel))
	 (zettel))
    (setq zettel matched-zettel)))

(defun orgrr-open-previous-zettel ()
  "Opens the previous zettel."
  (interactive)
  (let ((current-zettel (orgrr-read-current-zettel)))
    (when current-zettel
      (orgrr-prepare-zettelrank)
      (let* ((current-zettel-rank (gethash current-zettel orgrr-zettel-zettelrank))
	     (previous-rank (- (string-to-number current-zettel-rank) 1))
             (matched-zettel (gethash (number-to-string previous-rank) orgrr-zettelrank-zettel))
	     (matched-zettel-filename (gethash matched-zettel orgrr-zettel-filename)))
      	(orgrr-open-file matched-zettel-filename)))
    (when (not current-zettel)
      (message "This note has no value for zettel, so there is no next zettel!"))))

(defun orgrr-find (arg)
  "Find org-file in `org-directory' via mini-buffer completion. If the 
selected file name does not exist, a new one is created."
  (interactive "P")
  (let ((selection)
	(filename)
	(time))
    (when (equal arg '(4))
       (setq selection (orgrr-global-selection))
       (when (member selection (hash-table-keys orgrr-title-short_filename))
	 (setq filename (gethash selection orgrr-title-short_filename))
	 (setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename))
	 (orgrr-open-file filename)))
    (when (not (equal arg '(4)))  
      (setq selection (orgrr-selection))
      (when (member selection (hash-table-keys orgrr-title-filename))
	(setq filename (gethash selection orgrr-title-filename))
	(orgrr-open-file filename))
      (when (not (member selection (hash-table-keys orgrr-title-filename)))
	(setq time (format-time-string "%Y%m%d%H%M%S"))
	(setq filename (concat (file-name-as-directory org-directory) time "-" (replace-regexp-in-string "[\"'?,:;\\\s\/]" "_" selection)))
	(when (orgrr-on-macos-p)
	  (setq filename (ucs-normalize-HFS-NFD-string filename)))
	(orgrr-open-file (concat filename ".org"))
	(insert (concat "#+title: " selection "\n"))))))

(defun orgrr-global-find ()
  "A simple wrapper for a global orgrr-find."
  (interactive)
  (orgrr-find '(4)))

(defun orgrr-quick-add (&optional container)
  "Create org-file in container or org-directory."
  (interactive)
  (let* ((save-org-directory org-directory)
	 (filename)
	 (orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-keys orgrr-name-container)))
	 (time))
    (if (member container containers)
	(setq container (gethash container orgrr-name-container))
       (setq container org-directory))
    (setq time (format-time-string "%Y%m%d%H%M%S"))
    (setq filename (concat (file-name-as-directory container) time "-" orgrr-quick-add-token))
    (when (orgrr-on-macos-p)
      (setq filename (ucs-normalize-HFS-NFD-string filename)))
    (orgrr-open-file (concat filename ".org"))
    (insert (concat "#+title: " time "-" orgrr-quick-add-token "\n\n"))))

(defun orgrr-global-quick-add ()
  "Create org-file in a specific selected container."
  (interactive)
  (let*  ((orgrr-name-container (orgrr-get-list-of-containers))
	  (containers (nreverse (hash-table-keys orgrr-name-container)))
	  (container))
    (while (not (member container containers))
      (setq container (completing-read "Select container: " containers)))
    (orgrr-quick-add container)))

(defun orgrr-insert (arg)
  "Insert links to an org-file in `org-directory' via mini-buffer completion. 
If the selected title does not exist, a new note is created."
  (interactive "P")
  (let ((path-of-current-note
	 (if (buffer-file-name)
             (file-name-directory (buffer-file-name))
           default-directory))
	(selection)
	(filename))
    (when (equal arg '(4))
	(setq selection (orgrr-global-selection))
	(setq filename (gethash selection orgrr-title-short_filename))
	(setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename)) 
	(setq filename (file-relative-name filename path-of-current-note))
	(if (region-active-p)
	    (kill-region (region-beginning) (region-end)))
	(insert (concat "\[\[file:" filename "\]\[" selection "\]\]")))
    (when (not (equal arg '(4)))  
      (setq selection (orgrr-selection))
      (if (member selection (hash-table-keys orgrr-title-filename))
	  (progn
	    (setq filename (gethash selection orgrr-title-filename))
	    (setq filename (file-relative-name filename path-of-current-note))
	    (if (region-active-p)
		(kill-region (region-beginning) (region-end)))
	    (insert (concat "\[\[file:" filename "\]\[" selection "\]\]")))
	(progn
	  (let* ((time (format-time-string "%Y%m%d%H%M%S"))
		 (filename (concat (file-name-as-directory org-directory) time "-" (replace-regexp-in-string "[\"'?,:;\\\s\/]" "_" selection))))
	    (when (orgrr-on-macos-p)
	      (setq filename (ucs-normalize-HFS-NFD-string filename)))
	    (if (region-active-p)
		(kill-region (region-beginning) (region-end)))
	    (insert (concat "\[\[file:" (file-relative-name filename path-of-current-note) ".org" "\]\[" selection "\]\]"))
	    (orgrr-open-file (concat filename ".org"))
	    (insert (concat "#+title: " selection "\n"))))))))

(defun orgrr-global-insert ()
  "A simple wrapper for a global orgrr-insert."
  (interactive)
  (orgrr-insert '(4)))

(defun orgrr-random-note ()
  "Opens random org-file in `org-directory'."
  (interactive)
  (orgrr-get-meta)
  (let* ((titles (hash-table-keys orgrr-title-filename))
	 (random-title (elt titles (random (length titles))))
         (filename (gethash random-title orgrr-title-filename)))
    (orgrr-open-file filename)))

(defun orgrr-rename (&optional old-filename new-filename)
  "Rename current file and adjust all mentions of said file in other org-files in all containers. Does work across directories."
  (interactive)
  (let* ((old-filename-mentions '())
	 (orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-values orgrr-name-container)))
	 (lines))
    (when (not old-filename)
      (setq old-filename (if (equal major-mode 'dired-mode)
                        default-directory
			(buffer-file-name))))
    (when (not new-filename)
      (setq new-filename (read-from-minibuffer "Filename to change: " old-filename)))
    (rename-file old-filename new-filename)
    (set-visited-file-name new-filename nil t)
    (save-some-buffers t)  ;; necessary, as we are working directly with the files 
;; Add all files that mention filename to the list old-filename-mentions.
    (dolist (container containers)   
      (with-temp-buffer
	(if (orgrr-on-macos-p)
	    (insert (shell-command-to-string (concat "rg -l -e \"" (ucs-normalize-HFS-NFD-string (file-name-nondirectory old-filename)) "\" \"" (expand-file-name container) "\" -n -g \"*.org\"")))
	  (insert (shell-command-to-string (concat "rg -l -e \"" (file-name-nondirectory old-filename) "\" \"" (expand-file-name container) "\" -n -g \"*.org\""))))
	(setq lines (split-string (buffer-string) "\n" t)))
	  (dolist (line lines)
	    (if (string-match "\\.org$" line)
		(push line old-filename-mentions))))
;; This corrects the links. Please be aware that this is an intrusive action and might affect your data. 
      (dolist (filename old-filename-mentions)
	(with-current-buffer (find-file-noselect filename)
	  (goto-char (point-min))
	  (while (re-search-forward (file-name-nondirectory old-filename) nil t)
	    (replace-match (file-name-nondirectory new-filename)))))
  (save-some-buffers t)))

(defun orgrr-rename-title-and-file ()
  "Changes the title and filename of the current note. The first part of the 
filename (with the creation date) will not be modified."
  (interactive)
  (let ((old-filename (if (equal major-mode 'dired-mode)
                        default-directory
			(buffer-file-name)))
	(old-creation-time)
	(new-filename))
    (save-buffer)
    (setq new-title (read-from-minibuffer "New title: "))
    (orgrr-change-title new-title)
    (setq new-filename (replace-regexp-in-string "[\"'?,:;\\\s\/]" "_" new-title))
    (string-match "\[0-9\]+-" old-filename)
    (setq old-creation-time (match-string 0 old-filename))
    (setq new-filename (concat old-creation-time new-filename ".org"))
    (orgrr-rename nil new-filename)))

(defun orgrr-rename-and-move ()
 "Rename title and file, then move current note."
 (interactive)
 (orgrr-rename-title-and-file)
 (orgrr-move-note))
 
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
  (let ((orgrr-name-container (make-hash-table :test 'equal))
	(filename)
	(containers)
	(new-container)
	(selection))
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
  (clrhash orgrr-name-container)))

(defun orgrr-open-project ()
  "Find existing project or create a new one."
  (interactive)
  (let ((selection (orgrr-pick-project))
	(titles (hash-table-keys orgrr-title-short_filename))
	(filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-short_filename))
      (setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S")))
         (setq filename (concat (file-name-as-directory org-directory) time "-" (replace-regexp-in-string "[\"'\\\s\/]" "_" selection) ".org")))
	 (with-current-buffer (orgrr-open-file filename)
	 (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))))

(defun orgrr-insert-project ()
  "Insert link to an existing project."
  (interactive)
  (let ((selection (orgrr-pick-project))
	(path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))
	(titles (hash-table-keys orgrr-title-short_filename))
	(filename))
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-short_filename))
      (setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename))
      (setq filename (file-relative-name filename path-of-current-note))
      (insert (concat "\[\[file:" filename "\]\[" selection "\]\]"))))))

(defun orgrr-collect-project-snippet ()
  "Prepare snippet for `orgrr-add-to-project'."
  (let ((snippet))
    (when (string-match-p "backlinks for *" (buffer-name (current-buffer)))
      (let ((start (save-excursion
                     (org-back-to-heading)
                     (point)))
            (end (save-excursion
		   (org-end-of-subtree)
		   (point))))
	(setq snippet (buffer-substring-no-properties start end))))
    (when (not (string-match-p "backlinks for *" (buffer-name (current-buffer))))
      (save-excursion
	(let* ((line-number (line-number-at-pos))
	       (filename (buffer-file-name))
	       (title (pcase (org-collect-keywords '("TITLE"))
			(`(("TITLE" . ,val)) (car val)))))
	  (when (region-active-p)
	    (setq line-number (line-number-at-pos (region-beginning)))
	    (setq snippet (buffer-substring-no-properties (region-beginning) (region-end))))
	  (when (not (region-active-p))
	    (beginning-of-line)
	    (set-mark-command nil)
	    (end-of-line)
	    (setq snippet (buffer-substring-no-properties (region-beginning) (region-end))))
	  (setq snippet (concat "\*\* \[\[file:" filename "::" (number-to-string line-number)  "\]" "\[" title "\]\]:\n" snippet))
	  (deactivate-mark))))
    snippet))

(defun orgrr-add-to-project ()
  "Add the current line at point (including when in orgrr-backlinks buffer) 
or the active region of a note to an existing project."
  (interactive)
  (let* ((snippet (orgrr-collect-project-snippet))
	 (selection (orgrr-pick-project))
	 (filename)
         (titles (hash-table-keys orgrr-title-short_filename)))
  (when (member selection titles)
      (setq filename (gethash selection orgrr-title-short_filename))
      (setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename))
      (find-file-noselect filename))
  (when (not (member selection titles))
     (let ((time (format-time-string "%Y%m%d%H%M%S")))
       (setq filename (concat (file-name-as-directory org-directory) time "-" (replace-regexp-in-string "[\"'\\\s\/]" "_" selection) ".org"))
       (find-file-noselect filename)
       (insert (concat "#+title: " selection "\n#+roam_tags: orgrr-project\n"))))
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-max))
      (insert (orgrr-format-project-snippet snippet))
      (save-buffer))))

(defun orgrr-add-to-other-window ()
  "Add the current line at point (including when in orgrr-backlinks buffer) 
or the active region of a note to an other active window."
  (interactive)
  (let* ((snippet (orgrr-collect-project-snippet))
	 (selection (orgrr-pick-window))
	 (filename)
         (titles (hash-table-keys orgrr-title-short_filename)))
  (when (member selection titles)
      (setq filename (gethash selection orgrr-title-short_filename))
      (setq filename (gethash (concat "\\" filename) orgrr-short_filename-filename))
      (find-file-noselect filename))
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-max))
      (insert (orgrr-format-project-snippet snippet))
      (save-buffer))))

(defun orgrr-pick-project ()
  "Provides a list of all projects to add the new snippet, with the option to 
create a new one. Returns a project."
  (orgrr-get-all-meta)
  (let* ((orgrr-selection-list ())
	 (orgrr-selection-list-completion)
	 (orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-values orgrr-name-container)))
	 (selection))
    (dolist (container containers) 
      (with-temp-buffer
       (insert (shell-command-to-string (concat "rg -i --sort modified -l -e  \"^\\#\\+roam_tags:.+orgrr-project\" \"" (expand-file-name container) "\"")))
       (let ((lines (split-string (buffer-string) "\n" t)))
	 (dolist (line lines)
	   (when line
	     (let ((title (gethash (concat "\\" (file-name-nondirectory line)) orgrr-short_filename-title)))
	       (setq orgrr-selection-list (cons title orgrr-selection-list))))))))
     (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
     (setq selection (completing-read "Select: " orgrr-selection-list))
     (if (string-match "^\(" selection)
	 (setq selection (replace-regexp-in-string "\(.*?\) " "" selection)))
     selection))

(defun orgrr-pick-window ()
  "Provides a list of all open windows to add the new snippet -  with the 
exception of the source window."
  (orgrr-get-all-meta)
  (let* ((orgrr-selection-list ())
	 (list-of-windows '())
	 (orgrr-selection-list-completion)
	 (current-buffer-file (buffer-file-name (current-buffer)))
	 (selection))
    (with-temp-buffer
      (dolist (win (window-list))
      (insert (buffer-file-name (window-buffer win)))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (when (and line
		     (not (string-equal line current-buffer-file)))
	    (let ((title (gethash (concat "\\" (file-name-nondirectory line)) orgrr-short_filename-title)))
	       (setq orgrr-selection-list (cons title orgrr-selection-list))))))))
     (setq orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
     (when (>= (length orgrr-selection-list) 2)
       (setq selection (completing-read "Select: " orgrr-selection-list)))
     (when (= (length orgrr-selection-list) 1)
       (setq selection (car orgrr-selection-list)))
     (if (string-match "^\(" selection)
	 (setq selection (replace-regexp-in-string "\(.*?\) " "" selection)))
     selection))

(defun orgrr-format-project-snippet (snippet)
  "Formats an orgrr-project SNIPPET."
  (let ((footnote-link)
	(footnote-line)
	(footnote-description)
	(footnote)
	(project-snippet))
  (with-temp-buffer
    (insert snippet)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((current-entry (buffer-substring (line-beginning-position) (line-end-position))))
      (if (string-match "^\\*\\* \\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" current-entry)
	  (progn
            (setq footnote-link (match-string 1 current-entry))
            (setq footnote-description (match-string 2 current-entry))
	    (kill-whole-line 1))
	(forward-line)))
    (setq project-snippet (buffer-string)))
  (setq footnote (car (split-string (replace-regexp-in-string "^file:" "" footnote-link) "::")))
  (setq footnote (file-relative-name footnote default-directory))
  (setq footnote-line (string-to-number (car (cdr (split-string (replace-regexp-in-string "^file:" "" footnote-link) "::")))))
  (setq snippet (concat "\n\"" (string-trim (orgrr-adjust-links project-snippet)) "\"" "\t" "(Source: \[\[file:" (concat footnote "::" (number-to-string footnote-line)) "\]\[" footnote-description "\]\]" ")")))))

(defun orgrr-get-all-meta ()
  "Collects the filenames, titles and alias all of org-files across all
containers and adds them to the hashtables orgrr-short_filename-filename,
orgrr-short_filename-title, and orgrr-title-short_filename. This is needed
to correct the links of a snippet created in one container for use in another
via orgrr-add-to-project. 

An intended use case for orgrr-add-to-project is to add snippets to a writing 
project, which is located in a different container than the main database.

This also allows orgrr-show-related-notes to refer linked documents even 
if they are not in the same container."
  (clrhash orgrr-short_filename-filename)
  (clrhash orgrr-short_filename-title)
  (clrhash orgrr-title-short_filename)
  (let* ((orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-values orgrr-name-container))))
    (dolist (container containers) 
      (with-temp-buffer
	(insert (shell-command-to-string (concat "rg -i --sort accessed \"^\\#\\+(title:.*)|(roam_alias.*)\" \"" (expand-file-name container) "\" -g \"*.org\"")))
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((current-entry (buffer-substring (line-beginning-position) (line-end-position))))
	    (when (string-match "\\(.*\\):#\\+\\(title\\|TITLE\\):\\s-*\\(.+\\)" current-entry)
	      (let* ((filename (match-string 1 current-entry))
		     (title (match-string 3 current-entry)))
		(puthash (concat "\\" (file-name-nondirectory filename)) filename orgrr-short_filename-filename)
		(puthash (concat "\\" (file-name-nondirectory filename)) title orgrr-short_filename-title)
		(puthash title (file-name-nondirectory filename) orgrr-title-short_filename)))
	    (when (string-match "\\(#\\+roam_alias:\\|#+ROAM_ALIAS:\\)\\s-*\\(.+\\)" current-entry)
	      (let* ((line (split-string current-entry "\\(: \\|:\\)" t))
		     (filename (car line)))
		(with-temp-buffer
		  (insert current-entry)
		  (goto-char (point-min))
		  (while (re-search-forward "\"\\(.*?\\)\\\"" nil t)
		    (puthash (match-string 1) (file-name-nondirectory filename) orgrr-title-short_filename)))))
	  (forward-line)))))))
	 
(defun orgrr-adjust-links (string)
  "Adjusts/corrects all links of STRING relative to the position of the note. 
Does not work when filenames themselves are changed."
  (let* ((path-of-current-note
	  (if (buffer-file-name)
              (file-name-directory (buffer-file-name))
            default-directory)))
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
    (buffer-string))))
    
(defun orgrr-info ()
 "Show the amount of titles considered by orgrr and how long it takes to 
collect this information."
 (interactive)
 (let* ((result (benchmark-run-compiled 1
                  (progn
                    (orgrr-get-all-meta))))
       (titles (hash-table-keys orgrr-title-short_filename))
       (orgrr-name-container (orgrr-get-list-of-containers))
       (containers (nreverse (hash-table-values orgrr-name-container)))
       (container-number-of-files 0)
       (global-number-of-files 0))
 (dolist (container containers) 
   (setq container-number-of-files (string-to-number (string-trim (shell-command-to-string (concat "find \"" (expand-file-name container) "\"  -type f -name \"*.org\" | wc -l")))))
   (setq global-number-of-files (+ global-number-of-files container-number-of-files)))
   (message "Orgrr considers %d titles and alias in %d org-files in %d containers. Collecting the titles took %s seconds to complete." (length titles) global-number-of-files (length containers) (format "%.5f" (car result)))))
    
(defun orgrr-show-related-notes (arg)
  "Show all related notes in `org-directory' to the current org-file. Related 
means here notes linking to this note and the notes that link to them as well 
as notes linked by the current note and the links from these notes. It is 
assumed that the more times a note in environment is mentioned, the more 
important it is. Notes of higher importance are listed at the top. Parents and 
grandparents as well as children and grandchildren.

If called with C-u all containers will be searched for direct backlinks and
backlinks of backlinks (second order backlinks). This may take a while, be
patient."
   (interactive "P")
   (let ((call-with-arg nil))
     (when (equal arg '(4))
       (setq call-with-arg 1))
     (when (not (string-match-p "related notes for *" (buffer-name (current-buffer))))
       (clrhash orgrr-filename-mentions)
       (orgrr-get-all-meta)
       (let* ((filename (if (equal major-mode 'dired-mode)
			    default-directory
			  (buffer-file-name)))
	      (title (gethash (concat "\\" (file-name-nondirectory filename)) orgrr-short_filename-title))
              (relatednotes-buffer (concat "related notes for *" title "*"))
	      (related-notes (orgrr-backlinks-first-and-second-order call-with-arg))
	      (related-notes (+ related-notes (orgrr-forwardlinks-first-and-second-order)))
	      (sorted-values '()))
	 (with-current-buffer (get-buffer-create relatednotes-buffer)
	   (erase-buffer)
	   (orgrr-open-buffer relatednotes-buffer)
	   (insert (concat (orgrr-return-fullnote-linked-starred title) "\n\n"))
	   (insert (concat "* " (number-to-string related-notes) " relations\n\n"))
	   (maphash (lambda (key value)
		      (push (cons value key) sorted-values))
		    orgrr-filename-mentions)
	   (setq sorted-values (sort sorted-values (lambda (a b) (> (car a) (car b)))))
	   (dolist (entry sorted-values)
	     (let* ((connections (number-to-string (car entry)))
		    (list-filename (gethash (concat "\\" (substring (cdr entry) 1)) orgrr-short_filename-filename))
		    (list-title (gethash (concat "\\" (substring (cdr entry) 1)) orgrr-short_filename-title)))
	       (insert (concat "** " "\[\[file:" list-filename "\]\[" list-title "\]\]: " connections "\n"))))
	  (orgrr-prepare-findings-buffer relatednotes-buffer))))
    (when (string-match-p "related notes for *" (buffer-name (current-buffer)))
      (orgrr-close-buffer))))

(defun orgrr-show-multiverse (arg)
  "Combines `orgrr-show-relarted-notes' and `orgrr-show-sequence' into one
buffer. See these functions for more details.

If called with C-u all containers will be searched for direct backlinks and
backlinks of backlinks (second order backlinks). This may take a while, be
patient."
   (interactive "P")
   (let ((call-with-arg nil))
     (when (equal arg '(4))
       (setq call-with-arg 1))
     (when (not (string-match-p "multiverse for *" (buffer-name (current-buffer))))
       (clrhash orgrr-filename-mentions)
       (orgrr-get-all-meta)
       (orgrr-prepare-zettelrank)
       (let* ((filename (if (equal major-mode 'dired-mode)
			    default-directory
			  (buffer-file-name)))
	      (title (gethash (concat "\\" (file-name-nondirectory filename)) orgrr-short_filename-title))
              (multiverse-buffer (concat "multiverse for *" title "*"))
	      (related-notes (orgrr-backlinks-first-and-second-order call-with-arg))
	      (related-notes (+ related-notes (orgrr-forwardlinks-first-and-second-order)))
	      (sorted-values '())
	      (current-zettel (orgrr-read-current-zettel))
	      (parent-zettel (orgrr-read-zettel-parent current-zettel))
	      (zettel-filename (gethash current-zettel orgrr-zettel-filename))
	      (selection-zettel (gethash (concat "\\" zettel-filename) orgrr-filename-zettel))
	      (orgrr-zettel-list (hash-table-values orgrr-filename-zettel))
	      (orgrr-zettel-list (sort orgrr-zettel-list 'orgrr-dictionary-lessp)))
	 (when current-zettel
	   (with-current-buffer (get-buffer-create multiverse-buffer)
	     (erase-buffer)
	     (orgrr-open-buffer multiverse-buffer)
	     (insert (concat (orgrr-return-fullzettel-linked-head selection-zettel) "\n\n"))
	     (insert "* sequence:\n\n")
	     (if (not (string-equal parent-zettel ""))
		 (insert (concat "** parent: \t" (orgrr-return-fullzettel-linked parent-zettel) "\n\n"))
	     (insert "** This is a root zettel with no parent.\n\n"))
	     (dolist (element orgrr-zettel-list) 
	       (let* ((last-char (substring selection-zettel -1))
		    (is-last-char-num (string-match-p "[0-9]" last-char))
		    (regex (if is-last-char-num
			       (concat "^" selection-zettel "[a-zA-Z]")
			     (concat "^" selection-zettel))))
		 (when (string-match regex element)
		   (if (not (equal element selection-zettel))
		       (insert (concat "** " (orgrr-return-fullzettel-linked element) "\n"))))))
	     (insert (concat "\n* " (number-to-string related-notes) " relations:\n\n"))
	     (maphash (lambda (key value)
			(push (cons value key) sorted-values))
		      orgrr-filename-mentions)
	     (setq sorted-values (sort sorted-values (lambda (a b) (> (car a) (car b)))))
	     (dolist (entry sorted-values)
	       (let* ((connections (number-to-string (car entry)))
		      (list-filename (gethash (concat "\\" (substring (cdr entry) 1)) orgrr-short_filename-filename))
		      (list-title (gethash (concat "\\" (substring (cdr entry) 1)) orgrr-short_filename-title)))
		 (insert (concat "** " "\[\[file:" list-filename "\]\[" list-title "\]\]: " connections "\n"))))
	     (orgrr-prepare-findings-buffer multiverse-buffer)))
       (when (not current-zettel)
	 (message "This note does not have a value for zettel!"))))
     (when (string-match-p "multiverse for *" (buffer-name (current-buffer)))
       (orgrr-close-buffer))))

(defun orgrr-read-zettel-parent (zettel)
  "Returns parent zettel of current-zettel."
 (if (and zettel (string-match "\\(.*?\\)\\([0-9]+\\|[a-zA-Z]+\\)$" zettel))
      (match-string 1 zettel)
    zettel))

(defun orgrr-backlinks-first-and-second-order (call-with-arg)
  "Gets backlinks first and second order."
  (let* ((filename (if (equal major-mode 'dired-mode)
                      default-directory
		     (buffer-file-name)))
	 (original-filename (file-name-nondirectory filename))
	 (related-notes 0)
	 (counter 0)
	 (orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-values orgrr-name-container))))
    ;; get all backlinks first order
    (when (not call-with-arg)
      (setq containers ())
      (setq containers (cons org-directory containers)))
    (dolist (container containers)
      (with-temp-buffer
       (if (orgrr-on-macos-p)
	   (insert (shell-command-to-string (concat "rg -l -e \"" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "\" \"" (expand-file-name container) "\" -n -g \"*.org\"")))
	 (insert (shell-command-to-string (concat "rg -l -e \"" (file-name-nondirectory filename) "\" \"" (expand-file-name container) "\" -n -g \"*.org\""))))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (let* ((short-filename (file-name-nondirectory line)))
	  (if (string-match "\\.org$" line)
	      (progn
		(if (not (equal original-filename short-filename))
		    (progn
		      (if (not (member (concat "\\" short-filename) (hash-table-keys orgrr-filename-mentions)))
			  (progn
			    (puthash (concat "\\" short-filename) 1 orgrr-filename-mentions)
			    (setq related-notes (+ related-notes 1)))
			(progn
			  (setq counter (gethash (concat "\\" short-filename) orgrr-filename-mentions))
			  (setq counter (+ counter 1))
			  (setq related-notes (+ related-notes 1))
			  (puthash (concat "\\" short-filename) counter orgrr-filename-mentions)))))))))))
  ;; get all backlinks second order
  (dolist (entry (hash-table-keys orgrr-filename-mentions))
    (setq filename (substring entry 1))
    (with-temp-buffer
      (if (orgrr-on-macos-p)
	  (insert (shell-command-to-string (concat "rg -l -e \"" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "\" \"" (expand-file-name container) "\" -n -g \"*.org\"")))
	(insert (shell-command-to-string (concat "rg -l -e \"" (file-name-nondirectory filename) "\" \"" (expand-file-name container) "\" -n -g \"*.org\""))))
      (let ((lines (split-string (buffer-string) "\n" t)))
	(dolist (line lines)
	  (let* ((short-filename (file-name-nondirectory line)))
	    (if (string-match "\\.org$" line)
	      (progn
		(if (not (equal original-filename short-filename))
		    (progn
		      (if (not (member (concat "\\" short-filename) (hash-table-keys orgrr-filename-mentions)))
			  (progn
			    (puthash (concat "\\" short-filename) 1 orgrr-filename-mentions)
			    (setq related-notes (+ related-notes 1)))
			(progn
			  (setq counter (gethash (concat "\\" short-filename) orgrr-filename-mentions))
			  (setq counter (+ counter 1))
			  (setq related-notes (+ related-notes 1))
			  (puthash (concat "\\" short-filename) counter orgrr-filename-mentions)))))))))))))
  related-notes))

(defun orgrr-forwardlinks-first-and-second-order ()
  "Gets forward links first and second order in the same directory."
  (let* ((original-filename (if (equal major-mode 'dired-mode)
                      default-directory
		    (buffer-file-name)))
	 (original-filename (file-name-nondirectory original-filename))
	 (related-notes 0)
	 (counter 0)
	 (contents (with-current-buffer (buffer-name)
		     (buffer-substring-no-properties (point-min) (point-max)))))
;; find all forward links first order
    (with-temp-buffer
      (insert contents)
      (goto-char (point-min))
      (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
	 (let* ((filename (match-string 1))
		(new-directory (file-name-directory filename))
		(new-filename (file-name-nondirectory filename)))
	   (if (and (not (equal original-filename new-filename))(member (concat "\\" new-filename) (hash-table-keys orgrr-short_filename-filename)))
	       (progn
		 (if (not (member (concat "\\" new-filename) (hash-table-keys orgrr-filename-mentions)))
		     (progn
		       (puthash (concat "\\" new-filename) 1 orgrr-filename-mentions)
		       (setq related-notes (+ related-notes 1)))
		   (progn
		     (setq counter (gethash (concat "\\" new-filename) orgrr-filename-mentions))
		     (setq counter (+ counter 1))
		     (setq related-notes (+ related-notes 1))
		     (puthash (concat "\\" new-filename) counter orgrr-filename-mentions)))
;; add links second order
	       (with-temp-buffer
		 (insert-file-contents filename)
		 (goto-char (point-min))
		 (while (re-search-forward "file:\\(.*?\\.org\\)" nil t)
		   (let* ((2nd-filename (match-string 1))
			  (2nd-new-directory (file-name-directory 2nd-filename))
			  (2nd-new-filename (file-name-nondirectory 2nd-filename)))
		     (if (and (not (equal original-filename 2nd-new-filename))(member (concat "\\" 2nd-new-filename) (hash-table-keys orgrr-short_filename-filename)))
			 (progn
			   (if (not (member (concat "\\" 2nd-new-filename) (hash-table-keys orgrr-filename-mentions)))
			     (progn
			       (puthash (concat "\\" 2nd-new-filename) 1 orgrr-filename-mentions)
				(setq related-notes (+ related-notes 1)))
			    (progn
			      (setq counter (gethash (concat "\\" 2nd-new-filename) orgrr-filename-mentions))
			      (setq counter (+ counter 1))
			      (setq related-notes (+ related-notes 1))
			      (puthash (concat "\\" 2nd-new-filename) counter orgrr-filename-mentions)))))))))))))
    related-notes))

(defun orgrr-change-container (&optional container)
  "Switch between a list of containers stored in ~/.orgrr-container-list. 
orgrr-change-container can be called with a specific container."
  (interactive)
  (orgrr-check-for-container-file)
  (let* ((orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (nreverse (hash-table-keys orgrr-name-container)))
	 (selection))
    (if container
	(setq selection container)
      (setq selection (completing-read "Select: " containers)))
    (if (member selection containers)
	(setq org-directory (gethash selection orgrr-name-container))
      (message "Container does not exist."))))

(defun orgrr-check-for-container-file ()
  "Creates a container file in ~/.orgrr-container-list in case one does 
  not yet exist."
  (interactive)
  (when (not (file-exists-p "~/.orgrr-container-list"))
    (let ((orgrr-name-container (make-hash-table :test 'equal)))
       (when org-directory
         (puthash "main" org-directory orgrr-name-container)
       (with-temp-buffer
         (let ((json-data (json-encode orgrr-name-container)))
           (insert json-data)
           (write-file "~/.orgrr-container-list"))))))
  (when (file-exists-p "~/.orgrr-container-list")
    (let ((orgrr-name-container (make-hash-table :test 'equal))
	  (containers)
	  (containers-folders))
       (with-temp-buffer
	 (insert-file-contents "~/.orgrr-container-list")
	 (if (fboundp 'json-parse-buffer)
	     (setq orgrr-name-container (json-parse-buffer))))
       (setq containers (nreverse (hash-table-keys orgrr-name-container)))
       (setq containers-folders (hash-table-values orgrr-name-container))
       (when (not (member org-directory containers-folders))
	 (if (member "main" containers)
             (setq org-directory (gethash "main" orgrr-name-container))
           (setq org-directory (gethash (car containers) orgrr-name-container)))))))

(defun orgrr-get-list-of-containers ()
 "Return orgrr-name-container, a hashtable that includes a list of names and 
locations of all containers."
 (orgrr-check-for-container-file)
 (let ((orgrr-name-container (make-hash-table :test 'equal)))
   (with-temp-buffer
     (insert-file-contents "~/.orgrr-container-list")
     (if (fboundp 'json-parse-buffer)
	 (setq orgrr-name-container (json-parse-buffer))))
orgrr-name-container))

(defun orgrr-create-container ()
  "Create or add a directory as a container and switch to that container."
  (interactive)
  (let ((orgrr-name-container (orgrr-get-list-of-containers))
	(new-container (read-directory-name "Enter a directory name: ")))
    (if (yes-or-no-p (format "Are you sure you want to create the directory %s as a container? " new-container))
	(progn
	  (unless (file-exists-p new-container)
	    (make-directory new-container t))
	  (let* ((name (read-from-minibuffer "Please provide a name for the new container: ")))
	    (puthash name new-container orgrr-name-container)
	    (with-temp-buffer
	     (let* ((json-data (json-encode orgrr-name-container)))
	       (insert json-data)
	       (write-file "~/.orgrr-container-list"))))
	  (setq org-directory new-container))
    (message "%s was not created!" new-container))))

(defun orgrr-remove-container ()
  "Allow to remove a container for the list of containers."
  (interactive)
  (let* ((orgrr-name-container (orgrr-get-list-of-containers))
	 (containers (hash-table-keys orgrr-name-container))
	 (json-data)
	 (selection (completing-read "Which container should be removed? " containers)))
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
	    (setq org-directory (gethash "main" orgrr-name-container))))))))

(defun orgrr-fix-all-links-buffer ()
  "This runs the function orgrr-adjust-links on the current buffer."
 (interactive)
 (orgrr-get-all-meta)
 (let ((contents (with-current-buffer (buffer-name)
                  (buffer-substring-no-properties (point-min) (point-max)))))
   (erase-buffer)
   (insert (orgrr-adjust-links contents)))
 (goto-char (point-min)))

(defun orgrr-adjust-backlinks-in-current-container (filename)
  "This is a helper function for orgrr-move-note and will adjust all links in 
notes in the previous/old container referring to the moving note to its new 
location. It does not account for changes of the filename itself!

This one of the very few functions where orgrr is directly changing your data 
(to fix the links). Be aware of this, but don't be scared."
  (save-some-buffers t)  ;; necessary, as we are working directly with the files 
  (let* ((orgrr-backlinks '()) 
	 (original-filename filename))
    ;; Add all files that mention filename to the list orgrr-backlinks.
    (with-temp-buffer
       (if (orgrr-on-macos-p)
	   (insert (shell-command-to-string (concat "rg -l -e \"" (ucs-normalize-HFS-NFD-string (file-name-nondirectory filename)) "\" \"" (expand-file-name org-directory) "\" -n -g \"*.org\"")))
	 (insert (shell-command-to-string (concat "rg -l -e \"" (file-name-nondirectory filename) "\" \"" (expand-file-name org-directory) "\" -n -g \"*.org\""))))
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
   "This function can be used to fix all links in a container. This is useful 
if you move a whole container/directory to a new location. When running this 
function, make sure to be in the correct container."
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

(defun orgrr-read-roam-key ()
  "Reads out #+roam_key."
  (let* ((current-entry nil)
	 (roam-key nil)
	 (buffer (buffer-substring-no-properties (point-min) (point-max)))
	 (line)
	 (key))
    (with-temp-buffer
      (insert buffer)
      (goto-char (point-min))
      (while (not (eobp))
        (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
        (when (string-match "\\(#\\+roam_key:\\|#+ROAM_KEY:\\)\\s-*\\(.+\\)" current-entry)
          (let* ((line (split-string current-entry "\\: " t))
                 (key (car (cdr line)))
                 (key (string-trim-left key)))
            (setq roam-key key)))
        (forward-line)))
    roam-key))

(defun orgrr-change-title (&optional new-title)
  "Changes #+title."
  (interactive)
  (let* ((has-changed-p))
    (when (not new-title)
      (setq new-title (read-from-minibuffer "New title: ")))
    (save-excursion  
      (goto-char (point-min))
      (while (not (and (eobp)
		       has-changed-p))
	(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
	  (when (string-match "^#\\+title:\\s-*\\(.*\\)" line)
	    (kill-region (line-beginning-position) (line-end-position))
	    (insert (concat "#+title: " new-title))
	    (setq has-changed-p t))
	(forward-line))))))

  (defun orgrr-open-ref-url ()
    "Opens the URL in the current note's ROAM_KEY property, if one exists."
    (interactive)
    (let ((roam-key (orgrr-read-roam-key)))
      (browse-url roam-key)))

(defun orgrr-compile-sequence (arg)
  "Creates a temporary buffer with all notes between two selected zettels
(e.g. between two notes with values for zettel). Sequencing works similar
as in orgrr-show-sequence. 

This feature has two main use-cases. First, it does allow to look at a 
stack of notes at once. It is, therefore, not unlike orgrr-show-sequence
but includes the content of each zettel. Second, this also allows for 
drafting chapters based on notes. 

Clicking on the headlines or any other link will open the linked note in 
the mode other-window (can be turned off).

If called with C-u the buffer is created without headlines."
  (interactive "P")
  (let ((call-with-arg nil))
    (when (equal arg '(4))
      (setq call-with-arg 1))
    (when (not (string-match-p "*compiled sequence*" (buffer-name (current-buffer))))
      (let* ((current-zettel (orgrr-read-current-zettel))
	     (orgrr-selection-list (orgrr-prepare-zettel-selection-list))
	     (orgrr-selection-list-completion (orgrr-presorted-completion-table orgrr-selection-list))
	     (orgrr-zettel-list (hash-table-values orgrr-filename-zettel))
	     (orgrr-zettel-list (sort orgrr-zettel-list 'orgrr-dictionary-lessp))
	     (starting-point)
	     (end-point)
	     (end-flag)
	     (draft-buffer "*compiled sequence*"))
	(if current-zettel
	    (setq starting-point (completing-read "Select starting point: " orgrr-selection-list-completion nil nil current-zettel))
	  (setq starting-point (completing-read "Select starting point: " orgrr-selection-list-completion)))
	(if (string-match "^\\[\\(.*?\\)\\]" starting-point)
      	    (setq starting-point (replace-regexp-in-string "\\[.*?\\]\\s-*" "" starting-point)))
	(let*  ((zettel-filename (gethash starting-point orgrr-title-filename))
		(selection-zettel (gethash (concat "\\" zettel-filename) orgrr-filename-zettel)))
	  (dolist (element orgrr-zettel-list)
	    (let* ((last-char (substring selection-zettel -1))
		   (is-last-char-num (string-match-p "[0-9]" last-char))
		   (regex (if is-last-char-num
			      (concat "^" selection-zettel "[a-zA-Z]")
			    (concat "^" selection-zettel))))
	    (when (string-match regex element)
	      (if (not (equal element selection-zettel))
		    (setq end-point element)))))
	  (setq end-point (completing-read "Select end point: " orgrr-selection-list-completion nil nil end-point))
	  (if (string-match "^\\[\\(.*?\\)\\]" end-point)
	      (progn
      		(setq end-point (replace-regexp-in-string "\\[.*?\\]\\s-*" "" end-point))
		(setq end-point (orgrr-return-zettel-from-title end-point))))
	  (with-current-buffer (get-buffer-create draft-buffer)
	    (let ((inhibit-read-only t))
              (erase-buffer)
	      (insert (concat (orgrr-return-fullzettel-linked-starred selection-zettel) " - " (orgrr-return-fullzettel-linked-starred end-point) "\n\n"))
	      (insert "* compiled sequence:\n\n")
	      (dolist (element orgrr-zettel-list)
		(let* ((last-char (substring selection-zettel -1))
		       (is-last-char-num (string-match-p "[0-9]" last-char))
		       (regex (if (and is-last-char-num
				       (not (string-equal element selection-zettel)))
				  (concat "^" selection-zettel "[a-zA-Z]")
				(concat "^" selection-zettel))))
		  (when (and (string-match regex element)
			     (not end-flag))
		    (when (not call-with-arg) 
		      (insert (concat "* " (orgrr-return-fullzettel-linked-starred element) "\n\n")))
		    (insert (concat (orgrr-return-fullzettel-content element) "\n\n")))
		  (when (equal element end-point)
		      (setq end-flag t)))))
;;Starting here it is only window-management
	      (orgrr-open-buffer draft-buffer)
	      (orgrr-prepare-findings-buffer draft-buffer)))))
    (when (string-match-p "*compiled sequence*" (buffer-name (current-buffer)))
      (orgrr-close-buffer))))

(defun orgrr-search (arg &optional search-string)
 "Search function for orgrr based on ripgrep. When called with C-u all 
containers will be searched. Regex don't need to be escaped."
 (interactive "P")
  (let ((call-with-arg nil))
    (when (equal arg '(4))
      (setq call-with-arg 1))
    (when (not (string-match-p "search for *" (buffer-name (current-buffer))))
      (orgrr-get-all-meta)
      (let* ((search (if (not search-string) 
			 (read-from-minibuffer "Search for: ")
		       search-string)) 
	     (search-buffer (concat "search for *" search "*"))
	     (hits 0)
	     (orgrr-counter-quote (make-hash-table :test 'equal))
	     (orgrr-counter-filename (make-hash-table :test 'equal))
	     (orgrr-name-container (orgrr-get-list-of-containers))
	     (containers (nreverse (hash-table-values orgrr-name-container))))
    ;; collect all hits
	  (with-temp-buffer
	     (when (not call-with-arg)
	       (setq containers ())
	       (setq containers (cons org-directory containers)))
	     (dolist (container containers)
	       (erase-buffer)
	       (if (orgrr-on-macos-p)
		(insert (shell-command-to-string (concat "rg -i -e \"" search "\" \"" (expand-file-name container) "\" -n --sort accessed -g \"*.org\"")))
		(insert (shell-command-to-string (concat "rg -i -e \"" search "\" \"" (expand-file-name container) "\" -n --sort accessed -g \"*.org\""))))
	    (let ((lines (split-string (buffer-string) "\n" t)))
	      (dolist (line lines)
		(when (string-match "^\\(.*?\\):\\(.*\\)$" line)
 		  (setq hits (+ hits 1))
		  (puthash hits (match-string 1 line) orgrr-counter-filename)
		  (puthash hits (match-string 2 line) orgrr-counter-quote))))))
	  ;; match-string 2 includes the line number!
      (with-current-buffer (get-buffer-create search-buffer)
              (erase-buffer)
	      (orgrr-open-buffer search-buffer)
	      (org-mode)
	      (if call-with-arg
		  (insert (concat "Global search for: *" search "*\n\n"))
		(insert (concat "Local search for: *" search "*\n\n")))
              (if (= hits 1)
		  (insert "* 1 result\n\n")
		(insert (concat "* " (number-to-string hits) " results\n\n")))
	      ;; Going through the search results
              (dolist (counter (hash-table-keys orgrr-counter-filename))
		(let ((entry (gethash counter orgrr-counter-filename)))
		  (when (stringp entry)
		    (let ((key entry)
			  (value (gethash counter orgrr-counter-quote)))
                      (when (stringp value)
			(let* ((short_filename (file-name-nondirectory key))
			       (full-filename key)
			       (result (gethash (concat "\\" short_filename) orgrr-short_filename-title)))
			  (string-match "^\\(.*?\\):\\(.*\\)$" value)
			  (let* ((line-number (match-string 1 value))
				 (snippet (match-string 2 value))
				 (snippet (orgrr-adjust-links snippet))
				 (snippet (string-trim-left (string-trim-left snippet "*"))))
			    (insert (concat "\*\* \[\[file:" full-filename "::" line-number "\]" "\[" result "\]\]:\n\n"  snippet "\n\n")))))))))
	      (orgrr-prepare-findings-buffer search-buffer))))
      (when (string-match-p "search for *" (buffer-name (current-buffer)))
	(orgrr-close-buffer))))

(defun orgrr-global-search ()
  "A simple wrapper for a global orgrr-search."
  (interactive)
  (orgrr-search '(4)))

(defun orgrr-initialize ()
  "Sets org-link-frame-setup for single-window-mode and multi-window mode 
(which uses side-buffers). Also checks for org-directory and container
file."
  (when (equal orgrr-window-management "single-window")
    (setq org-link-frame-setup '((file . find-file))))
   (when (equal orgrr-window-management "multi-window")
      (setq org-link-frame-setup '((file . find-file-other-window))))
   (orgrr-check-for-container-file))

(orgrr-initialize)

(provide 'orgrr)

;;; orgrr.el ends here
