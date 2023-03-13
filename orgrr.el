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
;;
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

(defun orgrr-update ()
  "This creates a list with the location of the file and the title of the note. It takes about 30 seconds to complete. Not practical."
  (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -l --sortr modified  \"\\#\\+title:\" " org-directory)))
      (let ((result '())
            (current-entry "")
            (lines (split-string (buffer-string) "\n" t)))
        (dolist (line lines)
                (setq current-entry line)
                (setq result (plist-put result (orgrr-get-title current-entry) current-entry)))
;	  (setq result (cons (orgrr-get-title current-entry) result)))
          (setq orgrr-files-titles result)))
(message "orgrr updated!"))

(defun orgrr-update-test ()
  "A different attempt to do orgrr-update."
  (setq current-entry "")
  (setq orgrr-files "")
  (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -l --sort accessed  \"\\#\\+title:\" " org-directory)))
      (goto-char (point-min))
          (while (not (eobp))
	    (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
            (setq orgrr-files (cons (orgrr-get-title current-entry) orgrr-files))
            (forward-line)))
  (message "orgrr updated!"))

(defun orgrr-get-all-titles ()
  "Get value for #+TITLE:/#+title for all org-files."
  (setq current-entry "")
  (setq orgrr-titles "")
  (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -i --sort modified \"^\\#\\+title:\" " org-directory)))
      (goto-char (point-min))
          (while (not (eobp))
	    (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
	     (when (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
		    (setq orgrr-titles (cons (match-string 2 current-entry) orgrr-titles)))  
            (forward-line))))

(defun orgrr-insert ()
  "Insert a link to another org-file in org-directory via mini-buffer completion"
;; TODO: integrate roam_alias
  (interactive)
  (orgrr-get-all-titles)
  (setq selection (completing-read "" orgrr-titles))
  (setq line (shell-command-to-string (concat "rg -l -i -e \"^\\#\\+title:." (replace-regexp-in-string "[\"]" "." selection) "$\" " org-directory " -g \"*.org\"")))
  (setq line (string-trim-right line "\n"))
  (setq path-of-current-note
      (if (buffer-file-name)
          (file-name-directory (buffer-file-name))
        default-directory))  
  (setq line (file-relative-name line path-of-current-note))
  (insert (concat "\[\[file:" line "\]\[" selection "\]\]")))

(defun orgrr-find ()
  "Find org-file in org-directory via mini-buffer completion. Create a new one, if not existent."
;; TODO: integrate roam_alias
  (interactive)
  (orgrr-get-all-titles)
  (setq selection (completing-read "" orgrr-titles))
  (if (member selection (flatten-tree orgrr-titles))
    (progn
      (setq line (shell-command-to-string (concat "rg -l -i -e \"^\\#\\+title:." (replace-regexp-in-string "[\"]" "." selection) "$\" " org-directory " -g \"*.org\"")))
      (setq line (string-trim-right line "\n"))
      (org-open-file line))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" selection))))
	 (find-file (concat filename ".org"))
	 (insert (concat "#+title: " selection "\n\n")))))


