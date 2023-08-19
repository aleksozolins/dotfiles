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

(setq vc-follow-symlinks nil) ; Stop Emacs from asking about following symlinks when opening files
(recentf-mode 1) ; Have Emacs remember recently opened files when using fild file

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1) ; Remember and restore the last cursor location of opened files

(global-auto-revert-mode 1) ; Revert buffers when the underlying file has changed
(setq global-auto-revert-non-file-buffers t) ; Revert Dired and other buffers

(windmove-default-keybindings 'super) ; Navigate between windows with s-<arrow keys>

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

(pcase system-type
  ('gnu/linux
   (set-face-attribute 'default nil :font "Liberation Mono" :height 140))
  ('darwin
   (set-face-attribute 'default nil :font "Monaco" :height 170)))

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
