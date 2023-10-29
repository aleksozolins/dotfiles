(defun my-view-and-update-clocktables ()
  "Open time_tracking.org in a split buffer and update all clock tables."
  (interactive)
  (let ((buffer (find-file-noselect "~/docs/denote/20230530T132757--time-tracking__org_zapier.org")))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "^#\\+BEGIN: clocktable" nil t)
	  (org-ctrl-c-ctrl-c)
	  (forward-line)))
      (save-buffer))
    (display-buffer buffer)))

(defun my-kill-all-agenda-files ()
  "Close all buffers associated with files in `org-agenda-files'."
  (interactive)
  (let ((agenda-files (mapcar 'expand-file-name (org-agenda-files))))
    (dolist (buffer (buffer-list))
      (let ((buffer-file-name (buffer-file-name buffer)))
	(when (and buffer-file-name (member buffer-file-name agenda-files))
	  (kill-buffer buffer)))))
  (org-agenda-quit))

(provide 'org-mode-functions)
