;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
  		       ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(setq inhibit-startup-message t)      ; Disable startup message
(menu-bar-mode -1)                    ; Disable the menu bar
(scroll-bar-mode -1)                  ; Disable the scroll bar
(tool-bar-mode -1)                    ; Disable the toolbar
(tooltip-mode -1)                     ; Disable tooltips
(set-fringe-mode 10)                  ; Give some breathing room
(setq visible-bell t)                 ; Set up the visible bell
(minibuffer-electric-default-mode t)  ; Make default disappear in the minibuffer when typing
(setq suggest-key-bindings 3)         ; Make keybinding suggestions stick around longer
(add-to-list 'default-frame-alist '(undecorated . t))   ; Get rid of the title bar in macOS

;; Change the startup message in the minibuffer to a nice greeting
(defun display-startup-echo-area-message ()
  (message "Welcome back Aleks! Emacs started in %.2f seconds."
           (float-time (time-subtract after-init-time before-init-time))))

;; Setup initial scratch message
(setq initial-scratch-message ";; Welcome to Emacs! Feel free to write Lisp code or notes here.\n\n")

;; Make sure all Emacs frames start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable line numbers in general
(column-number-mode)
(global-display-line-numbers-mode)

;; Disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                mu4e-headers-mode-hook
                mu4e-main-mode-hook
                mu4e-view-mode-hook
                org-agenda-mode-hook
  	      vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Enable relative line numbers (like in Vim)
(setq display-line-numbers-type 'relative)

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

;; Set authinfo Source
(setq auth-sources '("~/.local/share/emacs/authinfo.gpg"))

(defun set-my-font ()
  (pcase system-type
    ('gnu/linux
     (setq default-frame-alist '((font . "Iosevka-14"))))
    ('darwin
     (set-face-attribute 'default nil :font "Iosevka" :height 190))))

;; Apply to existing frames and any future frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-my-font)))

;; Ensure the font is applied to the initial frame
(set-my-font)

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
(load-theme 'modus-vivendi t)

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

;; Define C-c o as a prefix key
(define-prefix-command 'my-custom-prefix)
(global-set-key (kbd "C-c o") 'my-custom-prefix)

(setq bookmark-default-file "~/Dropbox/apps/emacs/bookmarks")

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

;; Install Evil Mode
(use-package evil
  :ensure t
  :init
  ;; Enable Evil mode globally
  (setq evil-want-integration t) ;; This is optional since evil-collection handles it
  (setq evil-want-keybinding nil) ;; Disable default evil keybindings for better integration with evil-collection
  :config
  (evil-mode 1))

;; Install Evil Collection
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Esup
(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa
  :config
  (setq esup-depth 0)) ;; Without this we get a failure on macOS.

;; Which-Key
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
  (vertico-mode)
  :config
  ;; Use vim-like keybindings for Vertico (evil)
  (define-key vertico-map (kbd "C-j") 'vertico-next) ;; Move down
  (define-key vertico-map (kbd "C-k") 'vertico-previous) ;; Move up
  (define-key vertico-map (kbd "C-l") 'vertico-exit) ;; Select current entry
  (define-key vertico-map (kbd "C-h") 'vertico-directory-up)) ;; Go up a directory in dired mode

;; Orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq completion-ignore-case t))

;; Marginalia
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Embark
(use-package embark
  :ensure t
  :defer t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

; Corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)  ; shows documentation after `corfu-popupinfo-delay'
  (setq tab-always-indent 'complete)  ; This is needed for tab to work properly

  :config
  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  ;; Function to enable Corfu in the minibuffer when Vertico is not active,
  ;; useful for prompts such as `eval-expression' and `shell-command'.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  :hook
  (minibuffer-setup . contrib/corfu-enable-always-in-minibuffer))

;; Dired
(use-package dired
  :ensure nil ;; Dired is part of Emacs; no need to install it
  :bind (:map dired-mode-map
              ("V" . dired-open-file)) ;; Binding to a function defined in :config
  :config
  ;; Use GNU ls as insert-directory-program in case of macOS
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))

  ;; Set listing options
  (setq dired-listing-switches "-Alh --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)

  ;; Default to hiding details
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-hide-details-mode 1)))

  ;; Enable using 'a' to visit directories
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Function to open files using the system's default application
  (defun dired-open-file ()
    "Open the file at point in Dired with the appropriate system application."
    (interactive)
    (let ((file (dired-get-file-for-visit))
          (open-cmd (pcase system-type
                      ('darwin "open")
                      ('gnu/linux "xdg-open")
                      (_ "xdg-open"))))
      (message "Opening %s..." file)
      (call-process open-cmd nil 0 nil file))))

;; Dired Hide Dotfiles
(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . my-dired-mode-hook)
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode))
  :config
  (defun my-dired-mode-hook ()
    "My `dired' mode hook to hide dot-files by default."
    (dired-hide-dotfiles-mode)))

;; Vterm
(use-package vterm
  :ensure t
  :bind
  (("C-c o v" . vterm)
   ("C-c o V" . vterm-other-window))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (add-hook 'vterm-mode-hook 'goto-address-mode)) ;; Make links click-able!

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t)

;; Pulsar
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
  :bind (("C-x l" . pulsar-pulse-line)
         ("C-x L" . pulsar-highlight-dwim)))

;; Ledger Mode
(when (eq system-type 'darwin)
  (use-package ledger-mode
    :defer t
    :mode ("-ledger\\.txt\\'" . ledger-mode)  ;; Associate files ending in _ledger.txt with ledger-mode
    :config
    (setq ledger-clear-whole-transactions 1)
    (setq ledger-default-date-format "%Y-%m-%d")))

(when (eq system-type 'darwin)
  (use-package evil-ledger
    :ensure t
    :after ledger-mode
    :config
    (setq evil-ledger-sort-key "S")
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

;; Ripgrep
(use-package rg
  :defer t
  :config
  (rg-enable-default-bindings))

;; Elfeed
(use-package elfeed
  :ensure t
  :bind ("C-c o e" . elfeed) ;; My quick launcher
  :config
  (setq elfeed-db-directory "~/Dropbox/apps/elfeed")
  (pcase system-type
    ('darwin (setq elfeed-enclosure-default-dir "~/Downloads/"))
    ('gnu/linux (setq elfeed-enclosure-default-dir "~/dls/"))))

;; Elfeed-Org
(use-package elfeed-org
  :ensure t
  :after elfeed  ;; Ensure elfeed-org loads after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/docs/denote/20220814T132654--rss-feeds__elfeed_rss.org")))

(require 'org) ;; This may not be necessary. We can rely on org's built in lazy loading instead.

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
      (list (concat denote-directory "20210804T113317--todos__agenda.org")
            (concat denote-directory "20220720T114139--projects__agenda_project.org")
            (concat denote-directory "20220727T113610--calendar__agenda.org")
            (concat denote-directory "20220727T114811--recurring-financial-transactions__agenda_finances_recurring.org")
            (concat denote-directory "20230903T141829--task-inbox__agenda_inbox.txt")))

;; org-agenda window settings
(setq org-agenda-window-setup 'only-window) ; open the agenda full screen
(setq org-agenda-restore-windows-after-quit t) ; restore the previous window arrangement after quitting
(setq org-agenda-hide-tags-regexp "agenda") ; hide the "agenda" tag when viewing the agenda

;; Include archived trees in the agenda view
;; Used to have this to nil. Now it's recommended to use "v" in the agenda view to include archived items.
(setq org-agenda-skip-archived-trees t)

;; Allow refiling to other files
(setq org-refile-targets `((nil :maxlevel . 1)
                           (,(list (concat denote-directory "20210804T113317--todos__agenda.org")) :maxlevel . 1)
                           (,(list (concat denote-directory "20220720T114139--projects__agenda_project.org")) :maxlevel . 2)))

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
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "SKIP(k@/!)")))

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
;; For Habits
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

;; Custom Link Types
;; For magit status buffers
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
      `(("t" "Task (Quick Capture)" entry (file "~/docs/denote/20230903T141829--task-inbox__agenda_inbox.txt")
         "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

      ("T" "Task (Detailed Personal)" entry (file+headline "~/docs/denote/20210804T113317--todos__agenda.org" "Personal")
         "* %^{State|TODO|NEXT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

        ("Z" "Task (Detailed Zapier)" entry (file+headline "~/docs/denote/20210804T113317--todos__agenda.org" "Zapier")
         "* %^{State|TODO|NEXT} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

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

        ("e" "Event" entry (file+headline "~/docs/denote/20220727T113610--calendar__agenda.org" "Events")
               "* %^{Event Name}\n:SCHEDULED: %^T\n:PROPERTIES:\n:Location: %^{Location}\n:Note: %^{Note}\n:END:\n%?\n" :empty-lines 1)))

;; Default org capture file
(setq org-default-notes-file (concat org-directory "~/docs/denote/20230903T141829--task-inbox__agenda_inbox.txt"))

;; Prevent org-capture from saving bookmarks
(setq org-bookmark-names-plist '())
(setq org-capture-bookmark nil)

;; Org Babel
;; Enable certain languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (shell . t)))

;; Skip confirming when evaluating source blocks
(setq org-confirm-babel-evaluate nil)

;; Org Babel Structure Templates
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
  :after org
  :config
  (require 'denote)
  (setq denote-directory (expand-file-name "~/docs/denote/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "meta" "zapier"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(file-type date title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-date-format nil)
  (setq denote-backlinks-show-context t)
  (setq denote-save-files t)

  ;; If you use Markdown or plain text files (Org renders links as buttons right away)
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

  ;; I should probably add ~/docs to the list below too no?
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "data"))))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)

  ;; Denote DOES NOT define any key bindings. Se we define them here.
  (let ((map global-map))
    (define-key map (kbd "C-c d n") #'denote)
    (define-key map (kbd "C-c d N") #'denote-type)
    (define-key map (kbd "C-c d d") #'denote-date)
    (define-key map (kbd "C-c d z") #'denote-signature) ; "zettelkasten" mnemonic
    (define-key map (kbd "C-c d s") #'denote-subdirectory)
    (define-key map (kbd "C-c d t") #'denote-template)
    (define-key map (kbd "C-c d i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c d I") #'denote-add-links)
    (define-key map (kbd "C-c d b") #'denote-backlinks)
    (define-key map (kbd "C-c d f f") #'denote-find-link)
    (define-key map (kbd "C-c d f b") #'denote-find-backlink)
    (define-key map (kbd "C-c d r") #'denote-rename-file)
    (define-key map (kbd "C-c d R") #'denote-rename-file-using-front-matter)
    (define-key map (kbd "C-c d D") #'denote-journal-extras-new-or-existing-entry) ;; See journaling section below
    ;; Also check the commands `denote-link-after-creating',
    ;; `denote-link-or-create'.  You may want to bind them to keys as well.
    ;; Added by Aleks
    (define-key map (kbd "C-c d k") #'denote-rename-file-keywords)
    (define-key map (kbd "C-c d o") #'denote-open-or-create))

  ;; Key bindings specifically for Dired.
  (let ((map dired-mode-map))
    (define-key map (kbd "C-c C-d C-i") #'denote-link-dired-marked-notes)
    (define-key map (kbd "C-c C-d C-r") #'denote-dired-rename-files)
    (define-key map (kbd "C-c C-d C-k") #'denote-dired-rename-marked-files-with-keywords)
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

  ;; Journaling
  (require 'denote-journal-extras)
  (setq denote-journal-extras-keyword "journal")
  ;; (setq denote-journal-extras-directory "/Users/aleksozolins/docs/denote/journal") ;; this is set by default to a subdir of denote-directory called journal.
  (setq denote-journal-extras-title-format 'day-date-month-year)

  )

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

;; MU4E
(use-package mu4e
  :ensure nil  ;; mu4e is usually installed with mu; ensure should be nil
  :bind
  ("C-c o m" . mu4e)
  :hook
  (mu4e-compose-mode . (lambda () (auto-save-mode -1))) ;; Disable auto-save-mode when composing email to eliminate extra drafts
  ((mu4e-compose-mode . (lambda () (use-hard-newlines -1))))
  :init
  ;; Load path for mu4e installed via Homebrew on macOS
  (when (eq system-type 'darwin)
    (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/")
    (setq mu4e-mu-binary (executable-find "/opt/homebrew/bin/mu")))
  :config
  ; First we set the context-policy and contexts
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)
  (setq mu4e-contexts
      (list
       ;; aleks@ozolins.xyz
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
                  (mu4e-trash-folder     . "/aleks@ozolins.xyz/Trash")))))

  ;; Set up paths and specific configurations depending on the system
  (pcase system-type
    ('gnu/linux
     ;; Linux-specific settings
     (setq mu4e-attachment-dir  "~/dls")
     (setq mu4e-get-mail-command "mbsync -a"))
    ('darwin
     ;; macOS-specific settings
     (setq mu4e-attachment-dir  "~/Downloads")
     (setq mu4e-get-mail-command "/opt/homebrew/bin/mbsync -a")
     ;; Ensure GPG is configured correctly
     (require 'epa-file)
     (setq epg-gpg-program "/opt/homebrew/bin/gpg")
     (epa-file-enable)))

  ;; Settings that apply reglardless of system type...
  (setq mu4e-maildir "~/.local/share/mail")
  (setq mu4e-headers-include-related nil) ;; Do not include related messages (no threading!)
  (setq mu4e-org-contacts-file  "~/docs/denote/20220727T132509--contacts__contact.org") ;; Use org-contacts
  (setq mail-user-agent 'mu4e-user-agent) ;; set the default mail user agent
  (setq mu4e-change-filenames-when-moving t) ;; ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-view-scroll-to-next nil) ;; Prevent space bar from moving to next message
  (setq mu4e-headers-results-limit 5000) ;; Display more messages in each mailbox if possible
  ;; (setq mu4e-compose-complete-addresses nil) ;; Don't autocomplete emails using mu's built in autocompletion (we'll use org-contacts for this)
  (setq mu4e-compose-complete-addresses t) ;; Disabled org-contacts
  ;; (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum) ;; Always show the plaintext version of emails over HTML

  ;; Prefer the plain text version of emails
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))

  (setq mu4e-compose-format-flowed t) ;; Make sure plain text emails flow correctly for recipients

  (setq gnus-inhibit-images t) ;; Inhibit images from loading
  (setq mu4e-headers-show-threads nil) ;; Turn off threading by default
  ;; (setq mu4e-view-auto-mark-as-read nil) ;; Turn off automatic mark as read (use ! instead)
  (setq mu4e-update-interval (* 1 60)) ;; Refresh mail using isync every 10 minutes

  ;; Configure how to send mails
  ;; Note: .authinfo.gpg is used by default for authentication.
  ;; You can customize the variable auth-sources
  (setq message-send-mail-function 'smtpmail-send-it)

  (setq mu4e-compose-signature "Aleks Ozolins\ne: aleks@ozolins.xyz\nw: https://ozolins.xyz\nm: 973.464.5242")

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
  )

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
