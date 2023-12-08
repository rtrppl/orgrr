;; orgrr-extensions.el includes nice-to-have additions to orgrr,
;; that depend on external packages or binaries. The core functions
;; of orgrr do not require these functions. 
;;
;;
;;

;; change the following to your liking
(defvar orgrr-save-website-tags "website orgrr-project")

(cl-defun orgrr-save-website ()
  "Adds a website as an org-file to the current org-directory. Works on links, org-links or via entry of an URL."
  (interactive) 
  (let ((url (or 
              (thing-at-point-url-at-point)
              (org-element-property :raw-link (org-element-context))
              (read-string "Please enter a URL: "))))
    (-let* ((dom (plz 'get url :as #'org-web-tools--sanitized-dom))
            ((title . readable) (org-web-tools--eww-readable dom))
            (title (org-web-tools--cleanup-title (or title "")))
            (converted (org-web-tools--html-to-org-with-pandoc readable))
	    (time (format-time-string "%Y%m%d%H%M%S"))
            (filename (concat org-directory time "-" (replace-regexp-in-string "[\"':;\s\\\/]" "_" title))))
        (orgrr-open-file (concat filename ".org"))
          (insert (concat "#+title: " time "-" (replace-regexp-in-string "[\(\)]" "-" title) "\n"))
          (insert (concat "#+roam_tags: " orgrr-save-website-tags "\n"))
          (insert (concat "#+roam_key: " url "\n\n"))
          (insert converted))
      (goto-char (point-min))))     

(defun orgrr-show-findlike ()
  "Shows all related notes in org-directory using findlike."
  (interactive)
  (if (findlike-installed-p)
      (progn
	(if (not (equal (buffer-name (current-buffer)) "*Orgrr findlike*"))
	    (progn
	      (orgrr-get-meta)
	      (let ((filename (if (equal major-mode 'dired-mode)
				  default-directory
				(buffer-file-name))))
		(pcase (org-collect-keywords '("TITLE"))
		  (`(("TITLE" . ,val))
		   (setq title (car val))))
		(setq findlikelinks 0)
		(setq orgrr-counter-filename (make-hash-table :test 'equal))
		(with-temp-buffer
		  (insert (shell-command-to-string (concat "findlike -R -d " org-directory " " filename)))
		  (let ((lines (split-string (buffer-string) "\n" t)))
		    (dolist (line lines)
		      (progn
 			(setq findlikelinks (+ findlikelinks 1))
			(puthash findlikelinks line orgrr-counter-filename)))))
		(with-current-buffer (get-buffer-create "*Orgrr findlike*")
		  (let ((inhibit-read-only t))
		    (erase-buffer)
		    (insert (concat "\*\[\[file:" filename "\]\[" title "\]\]\*\n\n"))
		    ;; Going through the findlike findings
		    (dolist (counter (reverse (hash-table-keys orgrr-counter-filename)))
		      (setq entry (gethash counter orgrr-counter-filename))
                      (when (stringp entry)
			(let ((result (gethash (concat "\\" entry) orgrr-filename-title)))
			  (insert (concat "\*\* \[\[file:" entry "\]" "\[" result "\]\]\n\n"))))))
		  (display-buffer-in-side-window
		   (current-buffer)
		   '((side . right)
		     (slot . -1)
		     (window-width . 60)))
		  (with-current-buffer "*Orgrr findlike*"
		    (org-mode))))
	      (let ((window (get-buffer-window "*Orgrr findlike*")))
		(when window
		  (select-window window)
		  (setq default-directory org-directory)
		  (beginning-of-buffer)
		  (next-line 2)))
	      (clrhash orgrr-counter-filename)
	      (clrhash orgrr-filename-title))
	  (delete-window)))
    (message "findlike is not installed. See https://github.com/brunoarine/findlike for instructions.")))

(defun findlike-installed-p ()
  "Check whether exa is installed in the system path."
  (let ((findlike-exe (executable-find "findlike")))
    (and findlike-exe (file-executable-p findlike-exe))))
