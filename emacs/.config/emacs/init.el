;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

;; Magit won't install on the Mac on 28.1 so this is a temporary workaround
(pcase system-type 
  ('darwin (add-to-list 'package-archives
			(cons "gnu-devel" "https://elpa.gnu.org/devel/")
			t)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; macOS Only but might want in Linux
(pcase system-type
  ('darwin (use-package exec-path-from-shell
	     :ensure t
	     :if (memq window-system '(mac ns x))
	     :config
	     ;; (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
	     (exec-path-from-shell-initialize))))

(setq inhibit-startup-message t)      ; Disable startup message
(menu-bar-mode -1)                    ; Disable the menu bar
(scroll-bar-mode -1)                  ; Disable the scroll bar
(tool-bar-mode -1)                    ; Disable the toolbar
(tooltip-mode -1)                     ; Disable tooltips
(set-fringe-mode 10)                  ; Give some breathing room
(setq visible-bell t)                 ; Set up the visible bell
(minibuffer-electric-default-mode t)  ; Make default disappear in the minibuffer when typing
(setq suggest-key-bindings 3)         ; Make keybinding suggestions stick around longer

;; Stop Emacs from asking about following symlinks when opening files
(setq vc-follow-symlinks nil)

(set-face-attribute 'default nil :font "Liberation Mono" :height 130)

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

;; Configure the Modus Theme's appearance
(setq modus-themes-mode-line '(accented)
      modus-themes-bold-constructs t
      modus-themes-fringes 'subtle
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold-intense)
      modus-themes-prompts '(bold-intense)
      modus-themes-completions 'opinionated
      modus-themes-org-blocks 'tinted-background
      modus-themes-scale-headings nil
      modus-themes-region '(bg-only)
      modus-themes-headings
      '((1 . (rainbow overline background 1.4))
	(2 . (rainbow background 1.3))
	(3 . (rainbow bold 1.2))
	(t . (semilight 1.1))))

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

(setq completion-styles '(substring))  ;; define the completion style
(setq completion-ignore-case  t)  ;; ignore case

;; whick-key
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
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Sort directories first in dired
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header 
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
    (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

;; Allow command to visit directories and kill buffer in dired
(put 'dired-find-alternate-file 'disabled nil)

(defvar *afilename-cmd*
  '(("/home/aleksozolins/docs/org-roam/zapier_brags_and_contributions.org" . ". /home/aleksozolins/repos/aodotcom/zbp.sh")
    ("/Users/aleksozolins/docs/org-roam/zapier_brags_and_contributions.org" . ". /Users/aleksozolins/repos/aodotcom/zbp.sh")
    ("/home/aleksozolins/docs/org-roam/real_python_course_log.org" . ". /home/aleksozolins/repos/aodotcom/zbp.sh")
    ("/Users/aleksozolins/docs/org-roam/real_python_course_log.org" . ". /Users/aleksozolins/repos/aodotcom/zbp.sh")
    ("/home/aleksozolins/docs/org-roam/zapier_scc_competencies.org" . ". /home/aleksozolins/repos/aodotcom/zbp.sh")
    ("/Users/aleksozolins/docs/org-roam/zapier_scc_competencies.org" . ". /Users/aleksozolins/repos/aodotcom/zbp.sh"))
  "File association list with their respective command.")

(defun my/cmd-after-saved-file ()
  "Execute a command after saved a specific file."
  (let* ((match (assoc (buffer-file-name) *afilename-cmd*)))
    (when match
      (shell-command (cdr match)))))

(add-hook 'after-save-hook 'my/cmd-after-saved-file)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Magit
(use-package magit
  :ensure t)

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
(setq org-startup-folded t)

;; Set org directory
(setq org-directory "~/docs/org-roam")

;; Set org-agenda files
(setq org-agenda-files (expand-file-name "~/docs/agenda.txt" org-directory))

;; Include archived trees in the agenda view
;; Used to have this to nil. Now it's recommended to use "v" in the agenda view to include archived items.
(setq org-agenda-skip-archived-trees t)

;; Allow refiling to other agenda files 1 level deep
(setq org-refile-targets '((nil :maxlevel . 1)
			   (org-agenda-files :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Logging
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-note-clock-out nil)
(setq org-log-redeadline 'time)
(setq org-log-reschedule 'time)
(setq org-read-date-prefer-future 'time)

;; Set todo sequence
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" "SKIP(k!)")))

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Week Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))

	("D" "Day Dashboard"
	 ((agenda "" ((org-deadline-warning-days 7)(org-agenda-span 1)))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))))))

;; Configure org tags (C-c C-q)
(setq org-tag-alist
      '((:startgroup)
	; Put mutually exclusive tags here
	(:endgroup)
	("home" . ?h)
	("tech" . ?t)
	("financial" . ?f)
	("zapier" . ?z)
	("gigs" . ?g)
	("ozostudio" . ?o)
	("parents" . ?p)
	("check out" . ?c)
	("shopping" . ?s)
	("connections" . ?C)
	("someday" . ?S)
	("emacs" . ?e)
	("recurring" . ?r)))

;; Add some modules
(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t))

;; Org Contacts
(use-package org-contacts
  :ensure t
  :after org
  :custom (org-contacts-files '("~/docs/org-roam/contacts.org")))

;; Org capture
(use-package org-capture
  :ensure nil
  :after org)

(defvar my/org-contacts-template "* %(org-contacts-template-name)
    :PROPERTIES:
    :ADDRESS: %^{9 Birch Lane, Verona, NJ 07044}
    :EMAIL: %(org-contacts-template-email)
    :MOBILE: tel:%^{973.464.5242}
    :NOTE: %^{NOTE}
    :END:" "Template for org-contacts.")

(setq org-capture-templates
      `(("c" "Contact" entry (file+headline "~/docs/org-roam/contacts.org" "Misc"),
	 my/org-contacts-template :empty-lines 1)

	("t" "Tasks")
	("tt" "Task" entry (file+olp "~/docs/org-roam/todos.org" "Inbox")
	 "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

	("tn" "Next Task" entry (file+olp "~/docs/org-roam/todos.org" "Inbox")
	 "* NEXT %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

	("tc" "Check Out" entry (file+headline "~/docs/org-roam/todos.org" "Check Out")
	 "* TODO Check out %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n%i" :empty-lines 1)

	("m" "Metrics")
	("mw" "Weight" table-line (file "~/docs/org-roam/weight.org")
	 "| %U | %^{Weight} | %^{Note} |" :kill-buffer t)

	("M" "Meeting" entry (file "~/docs/org-roam/meetings.org")
	 "* %^U - %^{With} - %^{Event}     %^g\n\n%?" :empty-lines 1)

	("o" "Mouthpiece")
	("o1" "One-Piece" table-line (file "~/docs/org-roam/my_mouthpieces.org")
	 "| %^{Make} | one-piece | %^{Model} | %^{Finish||silver plated|gold plated|brass|nickel|stainless|bronze|plastic} | | %^{Notes} | |" :kill-buffer t)

	("o2" "Two-Piece" table-line (file "~/docs/org-roam/my_mouthpieces.org")
	 "| %^{Make} | two-piece | %^{Model} | %^{Finish||silver plated|gold plated|brass|nickel|stainless|bronze|plastic} | %^{Threads||standard|metric|other} | %^{Notes} | |" :kill-buffer t)

	("or" "Rim" table-line (file "~/docs/org-roam/my_mouthpieces.org")
	 "| %^{Make} | rim | %^{Model} | %^{Finish||silver plated|gold plated|brass|nickel|stainless|bronze|plastic} | %^{Threads||standard|metric|other} | %^{Notes} | |" :kill-buffer t)

	("oc" "Cup" table-line (file "~/docs/org-roam/my_mouthpieces.org")
	 "| %^{Make} | cup | %^{Model} | %^{Finish||silver plated|gold plated|brass|nickel|stainless|bronze|plastic} | %^{Threads||standard|metric|other} | %^{Notes} | |" :kill-buffer t)

	("z" "Zapier")
	("zb" "Brag" table-line (file "~/docs/org-roam/zapier_brags_and_contributions.org")
	 "| %^u | %^{Size||small|medium|large} | %^{Type||Loki issue|Loki FR|Rover note|brag|other} | [[%^{Link}][link]] | %^{Note} |")

	("r" "Real Python Course Completion" table-line (file "~/docs/org-roam/real_python_course_log.org")
	 "| %^u | %^{Course Name} | [[%^{Certificate Link}][link]] |")))

;; Default org capture file
(setq org-default-notes-file (concat org-directory "~/docs/inbox.txt"))

;;Enable certain languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; Skip confirming when evaluating source blocks
(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/docs/org-roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("p" "project" plain
      "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n#+date: %U\n#+category: ${title}\n#+filetage: project\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 ("C-M-i"    . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

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
(setq mu4e-compose-signature "Aleks Ozolins\nm:973.464.5242\naleks@aleksozolins.com\nhttps://www.aleksozolins.com")

;; Do not include related messages
(setq mu4e-headers-include-related nil)

;; Use org-contacts
(setq mu4e-org-contacts-file  "~/docs/org-roam/contacts.org")
;; BELOW DISABLED AS I THINK IT'S BETTER TO JUST USE ORG CAPTURE FOR REFILING
;;(add-to-list 'mu4e-headers-actions
;;  '("org-contact-add" . mu4e-action-add-org-contact) t)
;;(add-to-list 'mu4e-view-actions
;;  '("org-contact-add" . mu4e-action-add-org-contact) t)

(setq mu4e-maildir-shortcuts
      '(("/aleksozolins/INBOX"                . ?i)
        ("/aleksozolins/Sent Messages"        . ?s)
        ("/aleksozolins/Drafts"               . ?d)
        ("/aleksozolins/Archive"              . ?a)
        ("/aleksozolins/Trash"                . ?t)))

;; Run mu4e in the background to sync mail periodically
;;(mu4e t)

(setq mu4e-contexts
      (list
       ;; aleksozolins account
       (make-mu4e-context
        :name "aleksozolins"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/aleksozolins" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address     . "aleks@aleksozolins.com")
                (user-full-name        . "Aleks Ozolins")
                (smtpmail-smtp-server  . "smtp.powweb.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder    . "/aleksozolins/Drafts")
                (mu4e-sent-folder      . "/aleksozolins/Sent Messages")
                (mu4e-refile-folder    . "/aleksozolins/Archive")
                (mu4e-trash-folder     . "/aleksozolins/Trash")))
       ;; icloud account
       (make-mu4e-context
        :name "icloud"
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address     . "aleksozolins@me.com")
                (user-full-name        . "Aleks Ozolins")
                (smtpmail-smtp-server  . "smtp.mail.me.com")
                (smtpmail-smtp-service . 587)
                (smtpmail-stream-type  . starttls)
                (mu4e-drafts-folder    . "/icloud/Drafts")
                (mu4e-sent-folder      . "/icloud/Sent Messages")
                (mu4e-refile-folder    . "/icloud/Archive")
                (mu4e-trash-folder     . "/icloud/Deleted Messages")))))

;; Allow attaching files from within dired with C-c RET C-a
(require 'gnus-dired)

;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq python-shell-completion-native-disabled-interpreters '("python3"))

(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")
(setq org-babel-python-command "python3")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit embark marginalia vertico which-key rainbow-delimiters use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
