(defun orgrr-get-title (filename)
  "Get value for #+TITLE:/#+title for any file."
(setq line (shell-command-to-string (concat "rg \"\\#\\+title: \" " filename)))
(let ((case-fold-search t))
    (when (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)"
                        line)
      (match-string 2 line))))



(defun orgrr-show-backlinks ()
  "Show all backlinks to current file."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
    (pcase (org-collect-keywords '("TITLE"))
      (`(("TITLE" . ,val))
         (setq title (car val))))
(setq backlinks 0)
    (with-temp-buffer
      (insert (shell-command-to-string (concat "rg -e '" (file-name-nondirectory filename) "' " org-directory)))
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
          (if (= (- (length result-list) 1) 1)
              (insert "* 1 Backlink\n\n")
          (insert (concat "* " (number-to-string backlinks) " Backlinks\n\n")))          
          (dolist (entry result-list)
            (when (and (stringp entry)
                       (not (string= entry filename)))
              (let ((key entry)
                    (value (plist-get result-list entry)))
                (when (stringp value)
                  (let ((result (orgrr-get-title key))
			(line-number (orgrr-get-line-number key filename)
                    (insert (concat "\*\* \[\[file:" key "\]" "\[" result "\]\]:\n\n" value "\n\n"))))))))
        (display-buffer-in-side-window
         (current-buffer)
         '((side . right)
           (slot . -1)
           (window-width . 60)))
(with-current-buffer "*Orgrr Backlinks*"
      (org-mode)
      (beginning-of-buffer)))))

(defun orgrr-update ()
  "Insert a link to another org-file in org-directory via mini-buffer completion"
  (interactive)
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
  "Insert a link to another org-file in org-directory via mini-buffer completion"
  (interactive)
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
      (insert (shell-command-to-string (concat "rg -i --sort modified \"\\#\\+title:\" " org-directory)))
      (goto-char (point-min))
          (while (not (eobp))
	    (setq current-entry (buffer-substring (line-beginning-position) (line-end-position)))
	     (when (string-match "\\(#\\+title:\\|#+TITLE:\\)\\s-*\\(.+\\)" current-entry)
		    (setq orgrr-titles (cons (match-string 2 current-entry) orgrr-titles)))  
            (forward-line)))
 (message "orgrr updated!"))

(defun orgrr-insert ()
  "Insert a link to another org-file in org-directory via mini-buffer completion"
  (interactive)
  (orgrr-get-all-titles)
  (setq selection (completing-read "" orgrr-titles))
  (setq line (shell-command-to-string (concat "rg -l -i -e '\\#\\+title: " selection "$' " org-directory)))
  (print line)
  (setq line (string-trim-right line "\n"))
  (insert (concat "\[\[file:" line "\]\[" selection "\]\]")))



