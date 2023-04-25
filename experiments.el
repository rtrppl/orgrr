(defun orgrr-find ()
  "Finds org-file in org-directory via mini-buffer completion. If the selected file name does not exist, a new one is created."
  (interactive)file:../../../../../2020-07-01-2020_agrarian:  (orgrr-selection)
  (if (member selection titles)
    (progn
      (setq filename (gethash selection orgrr-title-filename))
      (org-open-file filename))
    (let* ((time (format-time-string "%Y%m%d%H%M%S"))
         (filename (concat org-directory time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection) ".org")))
      (let ((org-capture-templates '(("q" "orgrr-capture" plain
				    (file filename)))))
	   (org-capture nil "q")
	   (insert (concat "#+title: " selection "\n\n")))))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-filename-tags))

(let ((filename (concat "~/Documents/test" ".org"))
      (org-capture-templates
      '(("q" "orgrr-capture" plain 
	 (file filename)))))
(org-capture nil "q")  
(insert (concat "#+title: " "test" "\n\n")))


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
         (filename (concat time "-" (replace-regexp-in-string "[\"'\\\s]" "_" selection) ".org")))
         
;	 (let ((org-capture-templates '(("q" "orgrr-capture" plain
;	 (file (concat filename ".org")))))
;	   (org-capture nil "q")  
;	   (insert (concat "#+title: " selection "\n\n"))))
(let ((org-capture-templates '(("q" "orgrr-capture" plain
				    (file filename))))
	   (result (org-capture nil "q")))
	   (insert (concat "#+title: " selection "\n\n"))
	   (when (eq result 'org-capture-done)
	     (insert (concat "\[\[file:" filename "\]\[" selection "\]\]"))))))
(clrhash orgrr-title-filename)
(clrhash orgrr-filename-title)
(clrhash orgrr-filename-tags))

(let ((org-capture-templates '(("q" "orgrr-capture" plain
                                (file filename))))
      (result (org-capture nil "q")))
  (insert (concat "#+title: " selection "\n\n"))
  (when (eq result 'org-capture-done)
    (insert (concat "[[file:" filename "][" selection "]]"))))


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
(clrhash orgrr-counter-quote)
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
   (insert (concat "\n\"" (string-trim (orgrr-adjust-links project-snippet)) "\"" "\t" "(Source \[\[file:" footnote-link "\]\[" footnote-description "\]\]" ")"))
   (save-buffer))
(clrhash orgrr-counter-quote)
(clrhash orgrr-counter-filename)
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
(clrhash orgrr-counter-quote)
(clrhash orgrr-counter-filename)
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
 (let ((time (benchmark-run-compiled
 (orgrr-get-meta)
 (setq titles (hash-table-keys orgrr-title-filename)))))
 (message "Orgrr considers %d titles (this includes titles and alias). This function took %d seconds to complete." (length titles) time)))


(let ((time (benchmark-run-compiled
 (orgrr-get-meta)
 (setq titles (hash-table-keys orgrr-title-filename)))))
  (print time))

(defun orgrr-info ()
 "This function shows the amount of titles considered by orgrr."
 (interactive)
 (let ((result (benchmark-run-compiled 1
                 (progn
                   (orgrr-get-meta)
                   (setq titles (hash-table-keys orgrr-title-filename))))))
   (message "Orgrr considers %d titles (this includes titles and alias). This function took %s seconds to complete." (length titles) (format "%.2f" (car result)))))
