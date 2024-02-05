(defun my-elfeed-download-youtube-video (arg)
  "Download the YouTube video of the current entry in elfeed using youtube-dlp.
With a prefix argument, download the audio only in the best available format."
  (interactive "P")
  (when (eq major-mode 'elfeed-show-mode)  ; Ensure the function is called in elfeed-show-mode
    (elfeed-show-yank)  ; Copy the URL to the clipboard
    (let* ((url (current-kill 0))  ; Get the URL from the clipboard
           (download-dir (pcase system-type
                           ('darwin "~/Downloads/")
                           ('gnu/linux "~/dls/")))  ; Set download directory based on system
           (command (if arg
                        (format "yt-dlp -f 'bestaudio' -P '%s' '%s'" download-dir url)
                      (format "yt-dlp -f 'bestvideo+bestaudio' --merge-output-format mkv -P '%s' '%s'" download-dir url))))
      (async-shell-command command))))

(defun my-elfeed-show-visit-reader ()
  "Visit the current entry in Firefox using reader view."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (setq link (concat "about:reader?url=" link))
      (start-process "firefox" nil my-firefox-executable link))))

(provide 'elfeed-functions)
