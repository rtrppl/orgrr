(defun orgrr-get-title (filename)
  "Get value for #+TITLE:/#+title for any file."
(setq line (shell-command-to-string (concat "rg -i \"\\#\\+title: \" " filename)))
(let ((case-fold-search t))
    (when (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)"
                        line)
      (match-string 2 line))))

(defun orgrr-show-backlinks ()
  "Show all backlinks to current file."
;; TODO: add unlinked references below backlinks!
;; TODO: Rewrite using hashtables.
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
     (pcase (org-collect-keywords '("TITLE"))
       (`(("TITLE" . ,val))
          (setq title (car val))))
 (setq backlinks 0)
     (with-temp-buffer
       (insert (shell-command-to-string (concat "rg -e '" (file-name-nondirectory filename) "' " org-directory " -g \"*.org\"")))
       (let ((result '())
            (current-entry "")
            (lines (split-string (buffer-string) "\n" t)))
         (dolist (line lines)
           (if (string-match "^\\(.*?\\):\\(.*\\)$" line)
               (progn
 		(setq backlinks (+ backlinks 1))
                 (setq current-entry (match-string 1 line))
                 (setq result (plist-put result current-entry (match-string 2 line))))
             (setq current-entry (concat current-entry "\n" line)))
          (setq result-list result))))
      (with-current-buffer (get-buffer-create "*Orgrr Backlinks*")
        (let ((inhibit-read-only t))
          (erase-buffer)
           (insert (concat "\*\[\[file:" filename "\]\[" title "\]\]\*\n\n"))
           (if (= backlinks 1)
               (insert "* 1 Backlink\n\n")      
           (insert (concat "* " (number-to-string backlinks) " Backlinks\n\n")))          
           (dolist (entry result-list)
             (when (and (stringp entry)
                        (not (string= entry filename)))
               (let ((key entry)
                     (value (plist-get result-list entry)))
                 (when (stringp value)
                   (let ((result (orgrr-get-title key)))
                     (insert (concat "\*\* \[\[file:" key "\]" "\[" result "\]\]:\n\n" value "\n\n"))))))))
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
    (next-line 4))))

(defun orgrr-get-meta ()
  "Get value for #+TITLE:/#+title for all org-files."
  (setq current-entry "")
  (setq orgrr-title-filename (make-hash-table :test 'equal))
  (setq orgrr-filename-title (make-hash-table :test 'equal))
  (setq orgrr-alias-filename (make-hash-table :test 'equal))
  (setq orgrr-filename-tags (make-hash-table :test 'equal))
  (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -i --sort modified \"^\\#\\+(title:.*)|(roam_alias.*)|(roam_tags.*)\" " org-directory " -g \"*.org\"")))
      (goto-char (point-min))
          (while (not (eobp))
	    (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
;; The following checks if this is a #+title line and is so, adds the title  + filename to orgrr-title-filename.
	    (if (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	     (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	     (setq title (car (cdr (cdr line))))
	     (puthash title filename orgrr-title-filename)
	     (puthash (concat "\\" filename) title orgrr-filename-title)))
;; The following checks if this is a #+roam_alias line and if so, adds all alias  + filename to orgrr-alias-filename.
	    (if (string-match "\\(#\\+roam_alias:\\|#+ROAM_ALIAS:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	       (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	       (with-temp-buffer
		 (setq alias "")
		 (insert current-entry)
		 (goto-char (point-min))
		 (while (re-search-forward "\"\\(.*?\\)\\\"" nil t)
		   (puthash (match-string 1) filename orgrr-alias-filename)))))
;; The following checks if the line is about tags and if so copies the tags to  orgrr-tags-filename.
	     (if (string-match "\\(#\\+roam_tags:\\|#+ROAM_TAGS:\\)\\s-*\\(.+\\)" current-entry)
	     (progn
	     (setq line (split-string current-entry "\\(: \\|:\\)" t))
	     (setq filename (car line))
	     (setq tags (car (cdr (cdr line))))
	     (puthash (concat "\\" filename) tags orgrr-filename-tags)))
(forward-line))))

(defun orgrr-selection ()
 "This function prepares the variable orgrr-selection for completing-read and sends the result in selection to orgrr-find and orgrr-insert. It prepends tags before title and alias."
 (setq orgrr-selection-list ())
 (orgrr-get-meta)
 (setq titles (hash-table-keys orgrr-title-filename))
 (setq filenames-for-titles (hash-table-values orgrr-title-filename))
 (setq aliases (hash-table-keys orgrr-alias-filename))
 (setq filenames-for-alias (hash-table-values orgrr-alias-filename))
 (setq filenames-for-tags (hash-table-keys orgrr-filename-tags))
 (dolist (title titles)
  (setq filename (gethash title orgrr-title-filename)) 
  (if (member (concat "\\" filename) filenames-for-tags) 
      (setq orgrr-selection-list (cons (concat "(" (gethash (concat "\\" filename) orgrr-filename-tags) ")" " " title) orgrr-selection-list))
   (setq orgrr-selection-list (cons title orgrr-selection-list))))
(dolist (alias aliases)
  (setq filename (gethash alias orgrr-alias-filename)) 
  (if (member (concat "\\" filename) filenames-for-tags) 
      (setq orgrr-selection-list (cons (concat "(" (gethash (concat "\\" filename) orgrr-filename-tags) ")" " " alias) orgrr-selection-list))
   (setq orgrr-selection-list (cons alias orgrr-selection-list))))
(setq orgrr-selection-list (reverse orgrr-selection-list))
(setq selection (completing-read "" orgrr-selection-list))
(if (string-match "^\(" selection)
    (setq selection (replace-regexp-in-string "\(.*?\) " "" selection))))

(defun orgrr-find ()
  "Find org-file in org-directory via mini-buffer completion. If file does not exist, create a new one."
;; TODO: roam_tags
  (interactive)
  (orgrr-selection)
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (org-open-file filename))
  (if (member selection aliases)
    (progn
      (setq filename (gethash selection orgrr-alias-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" selection))))
	 (find-file (concat filename ".org"))
	 (insert (concat "#+title: " selection "\n\n"))))))

(defun orgrr-insert ()
  "Find org-file in org-directory via mini-buffer completion. If file does not exist, create a new one."
;; TODO: roam_tags
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
  (if (member selection aliases)
    (progn
      (setq filename (gethash selection orgrr-alias-filename))
      (setq filename (file-relative-name filename path-of-current-note))
      (insert (concat "\[\[file:" filename "\]\[" selection "\]\]")))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" selection))))
	 (find-file (concat filename ".org"))
	 (insert (concat "#+title: " selection "\n\n"))))))

