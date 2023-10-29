(defun my-zapier-ticketbar-check-in ()
  "Run the Check In AppleScript when the task has a specific heading."
  (let ((heading (nth 4 (org-heading-components))))
    (when (or (string-equal heading "Zapier Tickets")
              (string-equal heading "Zapier Chat"))
      (shell-command "osascript ~/Dropbox/apps/applescript/ticketbar-check-in.scpt"))))

(defun my-zapier-ticketbar-check-out ()
  "Run the Check Out AppleScript when the task has a specific heading."
  (let ((heading (nth 4 (org-heading-components))))
    (when (or (string-equal heading "Zapier Tickets")
              (string-equal heading "Zapier Chat"))
      (shell-command "osascript ~/Dropbox/apps/applescript/ticketbar-check-out.scpt"))))

(provide 'org-mode-non-interactive-functions)
