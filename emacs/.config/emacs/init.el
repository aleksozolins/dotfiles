;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)      ; Disable startup message
(menu-bar-mode -1)                    ; Disable the menu bar
(scroll-bar-mode -1)                  ; Disable the scroll bar
(tool-bar-mode -1)                    ; Disable the toolbar
(tooltip-mode -1)                     ; Disable tooltips
(set-fringe-mode 10)                  ; Give some breathing room
(setq visible-bell t)                 ; Set up the visible bell
(minibuffer-electric-default-mode t)  ; Make default disappear in the minibuffer when typing
(setq suggest-key-bindings 3)         ; Make keybinding suggestions stick around longer

;; Change the startup message in the minibuffer to a nice greeting
(defun display-startup-echo-area-message ()
  (message "Welcome back Aleks!"))

;; Make sure all Emacs frames start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode -1) ; Right now they are disabled

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                mu4e-headers-mode-hook
                mu4e-main-mode-hook
                mu4e-view-mode-hook
                org-agenda-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq vc-follow-symlinks t) ; Stop Emacs from asking about following symlinks when opening files
(recentf-mode 1) ; Have Emacs remember recently opened files when using find file

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1) ; Remember and restore the last cursor location of opened files

(global-auto-revert-mode 1) ; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

(windmove-default-keybindings 'super) ; Navigate between windows with s-<arrow keys>

;; Enable visual-line-mode for txt and md files
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

;; When Emacs runs 2 async commands at once, it will just rename the async buffers instead of ask.
;; This is useful in elfeed when I'm downloading YT videos.
(setq async-shell-command-buffer 'rename-buffer)

;; Enable delete selection mode
(delete-selection-mode 1)

(pcase system-type
  ('gnu/linux
   (setq default-frame-alist '((font . "Iosevka-14"))))
  ('darwin
   (set-face-attribute 'default nil :font "Iosevka" :height 190)))

(setq modus-themes-mode-line '(accented)
    modus-themes-bold-constructs t
    modus-themes-fringes 'subtle
    modus-themes-tabs-accented t
    modus-themes-paren-match '(bold-intense)
    modus-themes-prompts '(bold-intense)
    modus-themes-org-blocks 'tinted-background
    modus-themes-region '(bg-only)
    modus-themes-headings
    '((0 . (1.6))
      (1 . (rainbow overline background 1.2))
      (2 . (rainbow background 1.2))
      (3 . (rainbow bold 1))
      (t . (semilight 1))))

;; Load a Theme
(load-theme 'modus-operandi t)

;; Set a hot-key for switching between light and dark theme
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;; Backup options
(setq backup-directory-alist '(("." . "~/.config/emacs/backup/"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; auto-save
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq kill-buffer-delete-auto-save-files t)

;; Key re-bindings
(global-set-key (kbd "M-o") 'other-window)    ; Move to the other window C-x o but also now M-o
(global-set-key (kbd "M-i") 'imenu)           ; Invoke imenu. This replaces tab-to-tab-stop but what is that even?

;; Unbind C-z (Can still use C-x C-z to suspend the frame
(global-unset-key (kbd "C-z"))

;; Quick launch
(global-set-key (kbd "C-z m") 'mu4e)
(global-set-key (kbd "C-z e") 'elfeed)
(global-set-key (kbd "C-z v") 'vterm)
(global-set-key (kbd "C-z V") 'vterm-other-window)

(setq auth-sources '("~/.local/share/emacs/authinfo.gpg"))

(setq bookmark-default-file
      (pcase system-type
	('gnu/linux "~/Dropbox/apps/emacs/bookmarks")
	('darwin "~/Library/CloudStorage/Dropbox/apps/emacs/bookmarks")))

(global-set-key (kbd "<f8>") 'bookmark-bmenu-list)

;; Settings for tab-bar-mode
(tab-bar-mode t)                                                 ; Enable tab-bar-mode
(setq tab-bar-new-tab-choice "*scratch*")                        ; Automatically switch to the scratch buffer for new tabs
(setq tab-bar-new-tab-to 'rightmost)                             ; Make new tabs all the way to the right automatically
(setq tab-bar-new-button-show nil)                               ; Hide the new tab button - use the keyboard
(setq tab-bar-close-button-show nil)                             ; Hide the close tab button - use the keyboard
(setq tab-bar-tab-hints nil)                                     ; Hide the tab numbers
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))   ; Get rid of the history buttons in the tab bar

;; Keybindings
(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;; tab-bar-history-mode lets you step back or forwad through the window config history of the current tab
(tab-bar-history-mode t)
(global-set-key (kbd "s-[") 'tab-bar-history-back)
(global-set-key (kbd "s-]") 'tab-bar-history-forward)

(add-hook 'js-mode-hook
         (lambda ()
           (setq js-indent-level 2)))

(use-package typescript-mode
  :ensure t
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq completion-ignore-case t))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Vertico
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package corfu
  :ensure t)

(global-corfu-mode 1)

(corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

(define-key corfu-map (kbd "<tab>") #'corfu-complete)
(setq tab-always-indent 'complete) ;; This we needed for tab to work. Not from Prot's config.

;; Adapted from Corfu's manual.
(defun contrib/corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active.
  Useful for prompts such as `eval-expression' and `shell-command'."
  (unless (bound-and-true-p vertico--input)
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1)

;; Use GNU ls as insert-directory-program in case of macOS
(when (eq system-type 'darwin)
  (setq insert-directory-program "gls"))

;; Use human readable sizes and group directories first
;; Note that the "A" switch, as opposed to "a" leaves out . and ..
(setq dired-listing-switches "-Alh --group-directories-first")

(setq dired-dwim-target t)            ;; When copying/moving, suggest other dired buffer as target
(setq dired-recursive-copies 'always) ;; Always copy/delete recursively
(setq dired-recursive-deletes 'top)   ;; Ask once before performing a recursive delete

;; Hide details by default
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))

;; Do not disable using 'a' to visit a new directory without killing the buffer
(put 'dired-find-alternate-file 'disabled nil)

(use-package dired-hide-dotfiles
  :ensure t
  )

(defun my-dired-mode-hook ()
  "My `dired' mode hook."
  ;; To hide dot-files by default
  (dired-hide-dotfiles-mode))

;; To toggle hiding
(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(defun dired-open-file ()
  "Open the file at point in dired with the appropriate system application."
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (open-cmd (pcase system-type
                    ('darwin "open")
                    ('gnu/linux "xdg-open")
                    (_ "xdg-open"))))
    (message "Opening %s..." file)
    (call-process open-cmd nil 0 nil file)))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "V") 'dired-open-file))

(when (eq system-type 'gnu/linux)
  (define-key dired-mode-map (kbd "C-c d e") 'my-dropbox-exclude-list)
  (define-key dired-mode-map (kbd "C-c d x") 'my-dropbox-exclude-directory))

(use-package vterm
    :ensure t
    :config
    (setq vterm-kill-buffer-on-exit t)
    (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t)

(use-package pulsar
  :ensure t
  :init
  (setq pulsar-pulse t
	pulsar-delay 0.055
	pulsar-iterations 10
	pulsar-face 'pulsar-magenta
	pulsar-highlight-face 'pulsar-blue)
  :config
  (pulsar-global-mode 1)
  (let ((map global-map))
    (define-key map (kbd "C-x l") #'pulsar-pulse-line)
    (define-key map (kbd "C-x L") #'pulsar-highlight-dwim)))

(use-package ledger-mode
  :defer t
  :config
  (setq ledger-clear-whole-transactions 1)
  (setq ledger-default-date-format "%Y-%m-%d"))

;; Any file ending in _ledger.txt opens in ledger mode
(add-to-list 'auto-mode-alist '("-ledger\\.txt\\'" . ledger-mode))

(defun my-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/2024--my-ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/2024--my-ledger.txt")
  (goto-char (point-max)))

(defun my-recurring-ledger ()
  "Open the ledger file located at ~/docs/finances/ledger/2024--my-recurring-ledger.txt."
  (interactive)
  (find-file "~/docs/finances/ledger/2024--my-recurring-ledger.txt")
  (goto-char (point-max)))

(global-set-key (kbd "C-z l") 'my-ledger)
(global-set-key (kbd "C-z L") 'my-recurring-ledger)

(use-package rg
  :defer t
  :config
  (rg-enable-default-bindings))

;; Install the elfeed package
(use-package elfeed
  :ensure t
  :defer t
  :config
  (setq elfeed-db-directory "~/Dropbox/apps/elfeed")
  (pcase system-type
    ('darwin (setq elfeed-enclosure-default-dir "~/Downloads/"))
    ('gnu/linux (setq elfeed-enclosure-default-dir "~/dls/")))

  ;; Ensure elfeed-org is installed
  (use-package elfeed-org
    :ensure t)

  ;; Load elfeed-org to use an Org file as the source for feeds
  (with-eval-after-load 'elfeed-org
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Dropbox/docs/denote/20220814T132654--rss-feeds__elfeed_rss.org"))))

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

(add-hook 'elfeed-show-mode-hook
          (lambda ()
            (define-key elfeed-show-mode-map (kbd "D") 'my-elfeed-download-youtube-video)
            (define-key elfeed-show-mode-map (kbd "B") 'my-elfeed-show-visit-reader)))

(require 'org)

;; Org keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Define a function and then call a hook to enable some settings whenenver org-mode is loaded
(defun org-mode-setup ()
  ;;(org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook 'org-mode-setup)

;; Start org mode folded
(setq org-startup-folded nil)

;; Set org directory
(setq org-directory "~/docs/denote")

;; Use org-indent-mode by default
(setq org-startup-indented t)

;; Set denote-directory so we can set org-agenda files. Note that we do this again later.
(setq denote-directory (expand-file-name "~/docs/denote/"))

;; Set org-agenda files to list of files. Note they all have the agenda tag.
(setq org-agenda-files
      (list (concat denote-directory "agenda/20210804T113317--todos__agenda.org")
            (concat denote-directory "agenda/20240629T132837--journal__agenda.org")
            (concat denote-directory "agenda/20220720T114139--projects__agenda_project.org")
            (concat denote-directory "agenda/20220727T113610--calendar__agenda.org")
            (concat denote-directory "agenda/20220727T114811--recurring-financial-transactions__agenda_finances_recurring.org")
            (concat denote-directory "agenda/20230903T141829--email-inbox__agenda_inbox.txt")
            (concat denote-directory "agenda/20240509T094502--zapier-todos__agenda_zapier.org")
            (concat denote-directory "agenda/20240629T132516--zapier-journal__agenda_zapier.org")
            (concat denote-directory "agenda/20240629T132605--zapier-projects__agenda_project_zapier.org")))

;; org-agenda window settings
(setq org-agenda-window-setup 'only-window) ; open the agenda full screen
(setq org-agenda-restore-windows-after-quit t) ; restore the previous window arrangement after quitting
(setq org-agenda-hide-tags-regexp "agenda") ; hide the "agenda" tag when viewing the agenda

;; Include archived trees in the agenda view
;; Used to have this to nil. Now it's recommended to use "v" in the agenda view to include archived items.
(setq org-agenda-skip-archived-trees t)

;; Allow refiling to other files
(setq org-refile-targets `((nil :maxlevel . 1)
                           (,(list (concat denote-directory "agenda/20210804T113317--todos__agenda.org")) :maxlevel . 1)
                           (,(list (concat denote-directory "agenda/20220720T114139--projects__agenda_project.org")) :maxlevel . 2)
                           (,(list (concat denote-directory "agenda/20240509T094502--zapier-todos__agenda_zapier.org")) :maxlevel . 1)
                           (,(list (concat denote-directory "agenda/20240629T132605--zapier-projects__agenda_project_zapier.org")) :maxlevel . 2)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Logging
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-clock-into-drawer t) ; As opposed to 'CLOCKING'. t goes to 'LOGGING' by default. 
(setq org-log-note-clock-out nil)
(setq org-log-redeadline 'time)
(setq org-log-reschedule 'time)
(setq org-read-date-prefer-future 'time)

;; Set todo sequence
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "SKIP(k!)")))

(setq org-agenda-custom-commands
      '(("i" "Tasks with inbox tag"
         ((tags-todo "inbox"
                     ((org-agenda-overriding-header "Task Inbox")))))

        ("d" "Day Dashboard"
         ((agenda "" ((org-deadline-warning-days 7) (org-agenda-span 1)))
          (tags-todo "inbox"
                     ((org-agenda-overriding-header "Inbox")))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting Tasks")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("w" "Week Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting Tasks")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("n" "Tasks in NEXT state"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("u" "Unscheduled TODOs without Deadline"
         ((tags-todo "TODO=\"TODO\"-DEADLINE={.+}-SCHEDULED={.+}"
                     ((org-agenda-overriding-header "Unscheduled Tasks without Deadline")))))))

;; Configure org tags (C-c C-q)
(setq org-tag-alist
      '((:startgroup) ; list mutually exclusive tags below
        ("@home" . ?h)
        ("@computer" . ?c)
        ("@zapier" . ?z)
        ("@phone" . ?p)
        ("@out" . ?o)
        (:endgroup)
        ("finances" . ?f)
        ("parents" . ?P)
        ("buy" . ?b)
        ("connections" . ?C)
        ("someday" . ?s)
        ("edge" . ?e)
        ("emacs" . ?E)
        ("recurring" . ?r)))

;; Add some modules
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

(org-link-set-parameters
 "magit-status"
 :follow (lambda (path)
           (magit-status (expand-file-name path)))
 :export (lambda (path desc format)
           (cond
            ((eq format 'html)
             (format "<a href=\"magit-status:%s\">%s</a>" path desc))
            ((eq format 'latex)
             (format "\\href{magit-status:%s}{%s}" path desc))
            (t (format "magit-status:%s" path)))))

;; Org Contacts
(use-package org-contacts
  :ensure t
  :after org
  :custom (org-contacts-files '("~/docs/denote/20220727T132509--contacts__contact.org")))

;; Org capture
(use-package org-capture
  :ensure nil
  :after org)

(defvar my-org-contacts-template "* %(org-contacts-template-name)
      :PROPERTIES:
      :ADDRESS: %^{9 Birch Lane, Verona, NJ 07044}
      :EMAIL: %(org-contacts-template-email)
      :MOBILE: tel:%^{973.464.5242}
      :NOTE: %^{NOTE}
      :END:" "Template for org-contacts.")

(setq org-capture-templates
      `(("t" "Personal Task (Quick Capture)" entry (file+olp "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("T" "Personal Task (Detailed)")
        ("Th" "@Home" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "@Home")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("Tc" "@Computer" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "@Computer")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("Tp" "@Phone" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "@Phone")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("To" "@Out" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "@Out")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("TC" "Connections" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "Connections")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("Ts" "Someday" entry (file+headline "~/docs/denote/agenda/20210804T113317--todos__agenda.org" "Someday")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("d" "Daily Journal Template" entry
         (file "~/docs/denote/agenda/20240629T132837--journal__agenda.org")
         , (concat "* %<%Y-%m-%d %A>\n\n"
                   "** Tasks\n\n"
                   "** Notes\n\n"
                   "** Fitness\n\n"
                   "** Horn Playing\n\n"
                   "** Time Tracking\n\n"
                   "#+BEGIN: clocktable :scope agenda :filetitle t :fileskip0 t :maxlevel 3 :block today :match \"-@zapier\"\n"
                   "#+END:\n") :empty-lines 1)

        ("c" "Contact" entry (file+headline "~/docs/denote/20220727T132509--contacts__contact.org" "Misc")
         my-org-contacts-template :empty-lines 1 :kill-buffer t)

        ("m" "Metrics")
        ("mw" "Weight" table-line (file "~/docs/denote/20140713T132841--my-weight__health.org")
         "| %U | %^{Weight} | %^{Note} |" :kill-buffer t)

        ("M" "Mouthpiece")
        ("M1" "One-Piece Mouthpiece" entry (file+headline "~/docs/denote/20220725T132500--my-mouthpieces__mouthpiece.org" "Mouthpieces")
         "* %^{Make} %^{Model}\n:PROPERTIES:\n:Make: %\\1\n:Model: %\\2\n:Type: one-piece\n:Finish: %^{Finish|silver-plated|gold-plated|brass|nickel|stainless|bronze|plastic}\n:Notes: %^{Notes}\n:END:" :empty-lines 1 :kill-buffer t)

        ("M2" "Two-Piece Mouthpiece" entry (file+headline "~/docs/denote/20220725T132500--my-mouthpieces__mouthpiece.org" "Mouthpieces")
         "* %^{Make} %^{Model}\n:PROPERTIES:\n:Make: %\\1\n:Model: %\\2\n:Type: two-piece\n:Finish: %^{Finish|silver-plated|gold-plated|brass|nickel|stainless|bronze|plastic}\n:Threads: %^{Threads|standard|metric|Lawson}\n:Notes: %^{Notes}\n:END:" :empty-lines 1 :kill-buffer t)

        ("Mc" "Mouthpiece Cup" entry (file+headline "~/docs/denote/20220725T132500--my-mouthpieces__mouthpiece.org" "Mouthpieces")
         "* %^{Make} %^{Model} Cup\n:PROPERTIES:\n:Make: %\\1\n:Model: %\\2\n:Type: cup\n:Finish: %^{Finish|silver-plated|gold-plated|brass|nickel|stainless|bronze|plastic}\n:Threads: %^{Threads|standard|metric|Lawson}\n:Notes: %^{Notes}\n:END:" :empty-lines 1 :kill-buffer t)

        ("Mr" "Mouthpiece Rim" entry (file+headline "~/docs/denote/20220725T132500--my-mouthpieces__mouthpiece.org" "Mouthpieces")
         "* %^{Make} %^{Model} Rim\n:PROPERTIES:\n:Make: %\\1\n:Model: %\\2\n:Type: rim\n:Finish: %^{Finish|silver-plated|gold-plated|brass|nickel|stainless|bronze|plastic}\n:Threads: %^{Threads|standard|metric|Lawson}\n:Notes: %^{Notes}\n:END:" :empty-lines 1 :kill-buffer t)

        ("e" "Event" entry (file+headline "~/docs/denote/agenda/20220727T113610--calendar__agenda.org" "Events")
               "* %^{Event Name}\n:SCHEDULED: %^T\n:PROPERTIES:\n:Location: %^{Location}\n:Note: %^{Note}\n:END:\n%?\n" :empty-lines 1)

        ("z" "Zapier")
        ("zt" "@Zapier Task" entry (file+headline "~/docs/denote/agenda/20240509T094502--zapier-todos__agenda_zapier.org" "Inbox")
         "* %^{State|TODO|NEXT|WAIT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("zd" "Zapier Daily Journal Template" entry
         (file "~/docs/denote/agenda/20240629T132516--zapier-journal__agenda_zapier.org")
         , (concat "* %<%Y-%m-%d %A>\n\n"
                   "** Slack and Admin\n\n"
                   "** Email Support\n\n"
                   "** Chat Support\n\n"
                   "** Meetings\n\n"
                   "** Incident Handling\n\n"
                   "** Team Edge     :edge:\n\n"
                   "** Tasks\n\n"
                   "** Notes\n\n"
                   "** Time Tracking\n\n"
                   "#+BEGIN: clocktable :scope agenda :filetitle t :fileskip0 t :maxlevel 3 :block today :match \"+@zapier\"\n"
                   "#+END:\n") :empty-lines 1)

        ("zf" "Zapier Daily Journal Template (Expanded Clocktables)" entry
         (file "~/docs/denote/agenda/20240629T132516--zapier-journal__agenda_zapier.org")
         , (concat "* %<%Y-%m-%d %A>\n\n"
                   "** Slack\n\n"
                   "** Email Support\n\n"
                   "** Chat Support\n\n"
                   "** Meetings\n\n"
                   "** Incident Handling\n\n"
                   "** Team Edge     :edge:\n\n"
                   "** Tasks\n\n"
                   "** Notes\n\n"
                   "** Time Tracking\n\n"
                   "*** Today\n\n"
                   "#+BEGIN: clocktable :scope agenda :filetitle t :fileskip0 t :maxlevel 3 :block today :match \"+@zapier\"\n"
                   "#+END:\n\n"
                   "*** This Week\n\n"
                   "#+BEGIN: clocktable :scope agenda :filetitle t :fileskip0 t: :maxlevel 3 :block thisweek :match \"+@zapier\"\n"
                   "#+END:\n\n"
                   "*** This Week (Team Edge)\n\n"
                   "#+BEGIN: clocktable :scope agenda :filetitle t :fileskip0 t: :maxlevel 3 :block thisweek :match \"+@zapier+edge\"\n"
                   "#+END:\n\n") :empty-lines 1)))

;; Default org capture file
(setq org-default-notes-file (concat org-directory "~/docs/denote/agenda/20230903T141829--email-inbox__agenda_inbox.txt"))

;; Prevent org-capture from saving bookmarks
(setq org-bookmark-names-plist '())
(setq org-capture-bookmark nil)

;;Enable certain languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (shell . t)))

;; Skip confirming when evaluating source blocks
(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("pyo" . "src python :results output"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("jso" . "src js :results output"))
(add-to-list 'org-structure-template-alist '("jst" . "src js :tangle ~/temp.js"))
(add-to-list 'org-structure-template-alist '("html" . "src html"))
(add-to-list 'org-structure-template-alist '("css" . "src css"))

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

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C-c t") 'my-view-and-update-clocktables)
  (define-key org-agenda-mode-map (kbd "Q") 'my-kill-all-agenda-files))

(use-package denote
  :ensure t
  )
(require 'denote)

;; Remember to check the doc strings of those variables.
(setq denote-directory (expand-file-name "~/docs/denote/"))
(setq denote-known-keywords '("emacs" "journal" "meta" "zapier" "daily" "weekly"))
(setq denote-infer-keywords t)
(setq denote-sort-keywords t)
(setq denote-file-type nil) ; Org is the default, set others here
(setq denote-prompts '(file-type date title keywords))
(setq denote-excluded-directories-regexp nil)
(setq denote-excluded-keywords-regexp nil)

;; Pick dates, where relevant, with Org's advanced interface:
(setq denote-date-prompt-use-org-read-date t)

;; Read this manual for how to specify `denote-templates'.  We do not
;; include an example here to avoid potential confusion.

;; We do not allow multi-word keywords by default.  The author's
;; personal preference is for single-word keywords for a more rigid
;; workflow.
(setq denote-allow-multi-word-keywords nil)

(setq denote-date-format nil) ; read doc string

;; By default, we do not show the context of links.  We just display
;; file names.  This provides a more informative view.
(setq denote-backlinks-show-context t)

;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
;; advanced.

;; If you use Markdown or plain text files (Org renders links as buttons
;; right away)
(add-hook 'find-file-hook #'denote-link-buttonize-buffer)

;; We use different ways to specify a path for demo purposes.
(setq denote-dired-directories
      (list denote-directory
            (thread-last denote-directory (expand-file-name "data"))))

;; Generic (great if you rename files Denote-style in lots of places):
;; (add-hook 'dired-mode-hook #'denote-dired-mode)
;;
;; OR if only want it in `denote-dired-directories':
(add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

;; Here is a custom, user-level command from one of the examples we
;; showed in this manual.  We define it here and add it to a key binding
;; below.
(defun my-denote-daily ()
  "Create an entry tagged 'journal' with the date as its title.
If a journal for the current day exists, visit it.  If multiple
entries exist, prompt with completion for a choice between them.
Else create a new file."
  (interactive)
  (let* ((today (format-time-string "%A %e %B %Y"))
         (string (denote-sluggify 'title today))
         (files (denote-directory-files-matching-regexp string)))
    (cond
     ((> (length files) 1)
      (find-file (completing-read "Select file: " files nil :require-match)))
     (files
      (find-file (car files)))
     (t
      (denote
       today
       '("daily"))))))

;; Denote DOES NOT define any key bindings.  This is for the user to
;; decide.  For example:
(let ((map global-map))
  (define-key map (kbd "C-c d d") #'my-denote-daily) ; our custom command
  (define-key map (kbd "C-c d n") #'denote)
  (define-key map (kbd "C-c d N") #'denote-type)
  (define-key map (kbd "C-c d D") #'denote-date)
  (define-key map (kbd "C-c d z") #'denote-signature) ; "zettelkasten" mnemonic
  (define-key map (kbd "C-c d s") #'denote-subdirectory)
  (define-key map (kbd "C-c d t") #'denote-template)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  (define-key map (kbd "C-c d i") #'denote-link) ; "insert" mnemonic
  (define-key map (kbd "C-c d I") #'denote-add-links)
  (define-key map (kbd "C-c d b") #'denote-backlinks)
  (define-key map (kbd "C-c d l f") #'denote-find-link)
  (define-key map (kbd "C-c d l b") #'denote-find-backlink)
  ;; Note that `denote-rename-file' can work from any context, not just
  ;; Dired bufffers.  That is why we bind it here to the `global-map'.
  (define-key map (kbd "C-c d r") #'denote-rename-file)
  (define-key map (kbd "C-c d R") #'denote-rename-file-using-front-matter)
  ;; Added by Aleks
  (define-key map (kbd "C-c d k") #'denote-keywords-add)
  (define-key map (kbd "C-c d K") #'denote-keywords-remove)
  (define-key map (kbd "C-c d f") #'my-denote-find-file)
  (define-key map (kbd "C-c d F") #'my-denote-open-dired))

;; Key bindings specifically for Dired.
(let ((map dired-mode-map))
  (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
  (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-marked-files)
  (define-key map (kbd "C-c C-d C-R") #'denote-dired-rename-marked-files-using-front-matter)
  ;; Added by Aleks
  (define-key map (kbd "C-c C-d C-a") #'my-denote-aggregate-notes))

(with-eval-after-load 'org-capture
  (setq denote-org-capture-specifiers "%l\n%i\n%?")
  (add-to-list 'org-capture-templates
               '("n" "New note (with denote.el)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; If you want to have Denote commands available via a right click
;; context menu, use the following and then enable
;; `context-menu-mode'.
(add-hook 'context-menu-functions #'denote-context-menu)

(defun my-denote-find-file ()
  "Find a file in denote-directory recursively using completion, excluding files with 'archive' in the name."
  (interactive)
  (let* ((dir (directory-file-name denote-directory)) ; Ensure no trailing slash
         (cmd-output (shell-command-to-string
                      (format "find '%s' -type d \\( -name '.git' -o -name 'data' \\) -prune -o -type f ! -iname '*archive*' -print 2>&1" dir)))
         (all-files (split-string cmd-output "\n" t))
         (file-display-names (mapcar (lambda (f) (string-remove-prefix dir f)) all-files)))
    (if (string-match-p "No such file or directory" cmd-output)
        (message "Directory not found: %s" dir)
      (let* ((selected-display-name (completing-read "Choose file: " file-display-names nil t))
             (selected-file (concat dir selected-display-name)))
        (when selected-file
          (find-file selected-file))))))

(defun my-denote-open-dired ()
  "Open dired to denote-directory"
  (interactive)
  (dired denote-directory))

(defun my-denote-aggregate-notes ()
  "Aggregate contents of marked txt, md, and org files in Dired to an org buffer."
  (interactive)
  (if (not (eq major-mode 'dired-mode))
      (message "You're not in a Dired buffer!")
    (let ((files (dired-get-marked-files))
          (target-buffer (generate-new-buffer "*Denote Aggregated Notes*"))
          content)
      (with-current-buffer target-buffer
        (org-mode))
      (dolist (file files)
        (when (string-match-p "\\(txt\\|md\\|org\\)$" file)
          (with-temp-buffer
            (insert-file-contents file)
            (setq content (buffer-string)))
          (with-current-buffer target-buffer
            (goto-char (point-max))
            (insert (format "* %s\n" (file-name-nondirectory file)))
            (if (not (string-match-p "org$" file))
                (insert content)
              ;; If it's an org file, shift all headings down by one level.
              (insert (replace-regexp-in-string "^\\*" "**" content)))))
        )
      (switch-to-buffer target-buffer))))

;; Install the package
(pcase system-type
  ('gnu/linux (use-package mu4e
                :ensure nil))
  ('darwin (use-package mu4e
             :ensure nil
             :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"))) ;; macOS Only

;; Because we installed mu with homebrew (macOS Only)
(pcase system-type
  ('darwin (setq mu4e-mu-binary (executable-find "/opt/homebrew/bin/mu"))))

;; GPG binary (macOS Only)
(pcase system-type
  ('darwin (require 'epa-file)
           (setq epg-gpg-program "/opt/homebrew/bin/gpg")
           (epa-file-enable)))

;; set the default mail user agent
(setq mail-user-agent 'mu4e-user-agent)

;; This is set to 't' to avoid mail syncing issues when using mbsync
(setq mu4e-change-filenames-when-moving t)

;; Prevent space bar from moving to next message
(setq mu4e-view-scroll-to-next nil)

;; Display more messages in each mailbox if possible
(setq mu4e-headers-results-limit 5000)

;; Disable auto-save-mode when composing email to eliminate extra drafts
(add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))

;; Don't autocomplete email addresses using mu's built in autocompletion (we'll use org-contacts for this)
(setq mu4e-compose-complete-addresses nil)

;; Always show the plaintext version of emails over the HTML version
;; (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

;; Prefer the plain text version of emails
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

;; Inhibit images from loading
(setq gnus-inhibit-images t)

;; Turn off threading by default
(setq mu4e-headers-show-threads nil)

;; Turn off automatic mark as read (use ! instead)
;; (setq mu4e-view-auto-mark-as-read nil)

;; Set the download directory for attachments
(pcase system-type
  ('gnu/linux (setq mu4e-attachment-dir  "~/dls")) ;; Linux
  ('darwin (setq mu4e-attachment-dir  "~/Downloads"))) ;; macOS

;; Refresh mail using isync every 10 minutes
(setq mu4e-update-interval (* 1 60))
(pcase system-type
  ('gnu/linux (setq mu4e-get-mail-command "mbsync -a -c ~/.config/mbsyncrc")) ;; Linux
  ('darwin (setq mu4e-get-mail-command "/opt/homebrew/bin/mbsync -a -c ~/.config/mbsyncrc"))) ;; macOS
(setq mu4e-maildir "~/.local/share/mail")
(setq mu4e-context-policy 'pick-first)

;; Configure how to send mails
;; Note: .authinfo.gpg is used by default for authentication.
;; You can customize the variable auth-sources
(setq message-send-mail-function 'smtpmail-send-it)

;; Make sure plain text emails flow correctly for recipients
(setq mu4e-compose-format-flowed t)

;; Turn off use-hard-newlines - this helps the flow in certain clients aka Gmail
(add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

;; Compose a signature
(setq mu4e-compose-signature "Aleks Ozolins\ne: aleks@ozolins.xyz\nw: https://ozolins.xyz\nm: 973.464.5242")

;; Do not include related messages
(setq mu4e-headers-include-related nil)

;; Use org-contacts
(setq mu4e-org-contacts-file  "~/docs/denote/20220727T132509--contacts__contact.org")
;; BELOW DISABLED AS I THINK IT'S BETTER TO JUST USE ORG CAPTURE FOR REFILING
;;(add-to-list 'mu4e-headers-actions
;;  '("org-contact-add" . mu4e-action-add-org-contact) t)
;;(add-to-list 'mu4e-view-actions
;;  '("org-contact-add" . mu4e-action-add-org-contact) t)

(setq mu4e-maildir-shortcuts
      '(("/aleks@ozolins.xyz/Inbox"           . ?i)
	("/aleks@ozolins.xyz/Sent Items"      . ?s)
	("/aleks@ozolins.xyz/Drafts"          . ?d)
	("/aleks@ozolins.xyz/Archive"         . ?A)
	("/aleks@ozolins.xyz/Trash"           . ?t)
	("/aleks@ozolins.xyz/Admin"           . ?a)
	("/aleks@ozolins.xyz/Receipts"        . ?r)
	("/aleks@ozolins.xyz/Parents"         . ?p)
	("/aleks@ozolins.xyz/Sus"             . ?u)
	("/aleks@ozolins.xyz/Spam?"           . ?S)))

(setq mu4e-contexts
      (list
       ;; aleks@ozolins.xyz account
       (make-mu4e-context
	:name "1-aleks@ozolins.xyz"
	:match-func
	(lambda (msg)
	  (when msg
	    (string-prefix-p "/aleks@ozolins.xyz" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address     . "aleks@ozolins.xyz")
		(user-full-name        . "Aleks Ozolins")
		(smtpmail-smtp-server  . "smtp.mailfence.com")
		(smtpmail-smtp-service . 465)
		(smtpmail-stream-type  . ssl)
		(mu4e-drafts-folder    . "/aleks@ozolins.xyz/Drafts")
		(mu4e-sent-folder      . "/aleks@ozolins.xyz/Sent Items")
		(mu4e-refile-folder    . "/aleks@ozolins.xyz/Archive")
		(mu4e-trash-folder     . "/aleks@ozolins.xyz/Trash")))
       ;; aleks.admin@ozolins.xyz account
       (make-mu4e-context
	:name "2-aleks.admin@ozolins.xyz"
	:match-func
	(lambda (msg)
	  (when msg
	    (string-prefix-p "/aleks@ozolins.xyz" (mu4e-message-field msg :maildir))))
	:vars '((user-mail-address     . "aleks.admin@ozolins.xyz")
		(user-full-name        . "Aleks Ozolins")
		(smtpmail-smtp-server  . "smtp.mailfence.com")
		(smtpmail-smtp-service . 465)
		(smtpmail-stream-type  . ssl)
		(mu4e-drafts-folder    . "/aleks@ozolins.xyz/Drafts")
		(mu4e-sent-folder      . "/aleks@ozolins.xyz/Sent Items")
		(mu4e-refile-folder    . "/aleks@ozolins.xyz/Archive")
		(mu4e-trash-folder     . "/aleks@ozolins.xyz/Trash")))))

;; Set the compose context policy
(setq mu4e-compose-context-policy 'pick-first)

;; Run mu4e in the background to sync mail periodically - only in Linux
(when (eq system-type 'gnu/linux)
  (mu4e t))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
