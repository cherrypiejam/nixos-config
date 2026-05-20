;; Set default font
(set-frame-font "Hack Nerd Font Mono-10" nil t)
(setq default-frame-alist '((font . "Hack Nerd Font Mono-10")))
;; (tooltip-mode -1)

;; Make more room
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 1)

;; Mode bar utils
(display-battery-mode)
(display-time-mode)

;; TODO
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; Auto-refresh buffers when files on disk change
(global-auto-revert-mode t)

;; Setup backups and autosave
(make-directory "~/.emacs.d/backups/" t)
(make-directory "~/.emacs.d/autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)

;; Show trailing spaces
(setq-default show-trailing-whitespace t)

;; No tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Maybe?
;; (setq sentence-end-double-space t)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Helper functions
(defun wrapper/format/systemd-run (command)
  "Format COMMAND with systemd-run"
  (format "systemd-run -q --user --slice=app.slice --scope -- %s" command))

(defun wrapper/scoped-run (command)
  "Run COMMAND with systemd-run"
  (start-process-shell-command command nil (wrapper/format/systemd-run command)))

;; Ensure unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Enable auto pair
(require 'electric)
(electric-pair-mode)

;; Only use packages from Nixpkgs
(setq package-archives nil)
;; Setup package.el to work with MELPA
;; (require 'package)
;; (add-to-list 'package-archives
;;           '("melpa" . "https://melpa.org/packages/"))
;; (package-initialize)
;; (package-refresh-contents)
;; (unless (package-installed-p 'use-package)
;;  (package-install 'use-package))

;; Use-package
(require 'use-package)

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init
   '(vterm
     company
     xref
     org
     eldoc
     (pdf pdf-view)
     dired
     notmuch
     calc
     pass
     (magit magit-repos magit-submodule)
     calendar
     image-dired)))

(defun dark-mode ()
  "Toggle between `doom-moonlight` (dark) and `doom-one-light` (light)."
  (interactive)

  (let ((is-dark (memq 'doom-moonlight custom-enabled-themes)))
    ;; Disable themes. Continue if errors
    (dolist (theme custom-enabled-themes)
      (ignore-errors (disable-theme theme)))

    ;; Load the new theme
    (if is-dark
        (progn
          (load-theme 'doom-one-light t)
          (let ((fg (face-foreground 'mode-line nil 'default))
                (bg (face-background 'mode-line nil 'default)))
            (when (and (stringp fg) (stringp bg))
              (set-face-attribute 'mode-line nil
                                  :foreground bg
                                  :background fg)))
          (when (gsettings-available?)
            (gsettings-set-from-gvariant-string
             "org.gnome.desktop.interface" "color-scheme" "prefer-light")))
      (progn
        (load-theme 'doom-moonlight t)
        (set-face-attribute 'mode-line nil
                            :foreground "black"
                            :background "DarkOliveGreen4")

        (when (gsettings-available?)
          (gsettings-set-from-gvariant-string
           "org.gnome.desktop.interface" "color-scheme" "prefer-dark"))))))

;; Theme
(use-package gsettings)
(use-package doom-themes
  :after gsettings
  :ensure t
  :config
  ;; Fix broken font after a theme change
  (defun wrapper/reapply-font-after-theme (&rest _)
    "Reapply the custom font after loading or enabling a theme."
    (set-frame-font "Hack Nerd Font Mono-10" nil t)
    (add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono-10"))
    (message "Font reapplied after a theme change."))
  (advice-add 'load-theme :after #'wrapper/reapply-font-after-theme)
  (advice-add 'enable-theme :after #'wrapper/reapply-font-after-theme)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config)

  ;; Toggle dark mode on startup
  (dark-mode)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'doom-palenight t)
  ;; (set-face-foreground 'mode-line "black")
  ;; (set-face-background 'mode-line "DarkOliveGreen4")
  ) ;; End of Theme

;; Window manager
(use-package exwm
  :config
  ;; Set the initial workspace number
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings
  (setq exwm-input-global-keys
        `(
          ;; 's-r': Reset (to line-mode)
          ([?\s-r] . exwm-reset)
          ;; 's-w': Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ;; 's-&': Launch application
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (wrapper/scoped-run command)))
          ;; 's-N': Switch to certain workspace
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ;; Audio
          (,(kbd "<XF86AudioMute>") .
           (lambda ()
             (interactive)
             (let* ((out (shell-command-to-string "amixer set Master toggle | tail -1"))
                (result (last (split-string out) 2)))
               (message "Sound: %s %s" (car result) (cadr result)))))
          (,(kbd "<XF86AudioLowerVolume>") .
           (lambda ()
             (interactive)
             (let* ((out (shell-command-to-string "amixer set Master 5%- | tail -1"))
                (result (last (split-string out) 2)))
               (message "Sound: %s %s" (car result) (cadr result)))))
          (,(kbd "<XF86AudioRaiseVolume>") .
           (lambda ()
             (interactive)
             (let* ((out (shell-command-to-string "amixer set Master 5%+ | tail -1"))
                (result (last (split-string out) 2)))
               (message "Sound: %s %s" (car result) (cadr result)))))
	      ;; Brightness
	      (,(kbd "<XF86MonBrightnessDown>") .
	       (lambda ()
             (interactive)
             (wrapper/scoped-run "brightnessctl --device 'amdgpu_bl1' set 5%-")))
	      (,(kbd "<XF86MonBrightnessUp>") .
	       (lambda ()
             (interactive)
             (wrapper/scoped-run "brightnessctl --device 'amdgpu_bl1' set 5%+")))
	      ;; Others
          ;; 's-l': Lock screen
          ([?\s-l] . (lambda ()
                       (interactive)
                       (wrapper/scoped-run
                        ;; "xautolock -locknow"
                        "systemctl suspend"
                        )))))
  ;; Line-editing shortcuts
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])))
  ;; Class-specific handling
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              (if (string= exwm-class-name "firefox") (evil-emacs-state))))
  ;; Workspace
  (setq exwm-workspace-warp-cursor t)
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)
  ;; Multi-displays
  (require 'exwm-randr)
  (add-hook 'exwm-randr-screen-change-hook
        (lambda ()
          (wrapper/scoped-run "autorandr --change --force")
          (message "Display config changed")))
  ;; (setq exwm-randr-workspace-monitor-plist '(2 "DP-3" 3 "DP-4"))
  (setq exwm-randr-workspace-monitor-plist '())
  (exwm-randr-mode 1)
  ;; Startup tasks
  (add-hook 'exwm-init-hook
        (lambda ()
          (wrapper/scoped-run "nm-applet")))
  ;; Enable the system tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 22) ;; default 22
  (exwm-systemtray-mode 1)
  ;; Debug
  ;; (setq exwm-debug t)
  ;; (xcb:debug)
  ;; Using xim input (FIXME: Enabling XIM freezes emacs)
  ;; (require 'exwm-xim)
  ;; (exwm-xim-mode 1)
  (push ?\C-\\ exwm-input-prefix-keys)
  ;; Enable EXWM
  (exwm-enable))

;; (use-package exwm-outer-gaps
;;   :config
;;   (exwm-outer-gaps-set-all 20 nil)
;;   ;; Disable by default
;;   (exwm-outer-gaps-mode -1))

(use-package exwm-firefox
  :demand t
  :config
  (advice-add 'exwm-firefox--setup-hook :override (lambda () ()))
  (exwm-firefox-mode))

;; VTerm
(use-package vterm
  :ensure t
  :custom
  (vterm-shell (wrapper/format/systemd-run shell-file-name))
  :hook
  (vterm-mode . (lambda ()
                  (setq show-trailing-whitespace nil)
                  (display-line-numbers-mode -1)))
  ;; :bind
  ;; (("s-v" . nterm))
  :config
  ;; Increase maximum scrollback value
  (setq vterm-max-scrollback 3000)
  ;; Additional bindings
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key '(normal insert emacs) 'vterm-mode-map
      (kbd "C-c C-q") 'vterm-send-next-key))
  (defun nterm ()
    "Instantiate a new vterm instance with a unique buffer name"
    (interactive)
    (let ((counter 0))
      (while (get-buffer (format "%s<%d>" vterm-buffer-name counter))
        (setq counter (1+ counter)))
      (vterm (format "%s<%d>" vterm-buffer-name counter))))
  (defun kterm (k)
    "Instantiate k vterm instance(s)"
    (interactive "nNumber of vterm instances: ")
    (dotimes (_ k) (nterm))))

;; Ivy mode
(use-package ivy
  :config (ivy-mode 1))

;; Cursor finder
(use-package beacon
  :config (beacon-mode 1))

;; Autocompletion
(use-package company
  :hook
  (after-init . global-company-mode))
  ;; ((nix-mode rust-mode) . company-mode))

;; PDF tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :hook
  (pdf-view-mode . (lambda ()
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))
                     (display-line-numbers-mode -1)))
  :config
  (pdf-tools-install :no-query))

;; LSP support
(use-package eglot
  :hook
  ;; FIXME: Removed rust-mode as rust-analyzer regularly crashes
  ;; ((c-mode scala-mode) . eglot-ensure)
  ((c-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  ;; Evil bindings
  (add-hook 'eglot-managed-mode-hook #'evil-normalize-keymaps)
  (evil-define-key 'normal 'eglot-mode-map
    "gd" 'xref-find-definitions
    "gD" 'xref-find-definitions-other-window
    "g5" 'xref-find-definitions-other-frame
    (kbd "C-t") 'xref-pop-marker-stack
    "gr" 'xref-find-references))

(use-package eldoc-box
  :config
  ;; Evil bindings
  (add-hook 'eglot-managed-mode-hook #'evil-normalize-keymaps)
  (evil-define-key 'normal 'eglot-mode-map
    "K" 'eldoc-box-help-at-point))

;; Syntax highlight
(use-package tree-sitter
  :demand t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode t))

;; Nix mode
(use-package nix-mode
  :mode "\\.nix\\'")

;; Rust mode
(use-package rust-mode
  :mode "\\.rs\\'")

;; Markdown mode
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Scala mode
(use-package scala-mode
  :mode "\\.scala\\'"
  :interpreter "scala")

;; Haskell mode
(use-package haskell-mode
  :mode "\\.hs$")

;; Typst mode
(use-package typst-ts-mode
  :mode "\\.typ$"
  :custom
  (typst-ts-mode-enable-raw-blocks-highlight t))

;; Agda mode
(use-package agda2-mode
  ;; :hook
  ;; (deactivate-input-method)
  :mode "\\.agda$")

;; Org mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook
  (org-mode . (lambda () (org-bullets-mode 1)))
  :config
  (require 'org-bullets)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts t)
  (setq org-agenda-files '("~/org")))

(use-package org-babel
  :no-require
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C      . t)
     (python . t)
     (emacs-lisp . t)
     (lilypond . t))))

;; (use-package evil-org
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys)
;;   (evil-set-initial-state 'org-agenda-mode 'motion))

;; Key finding
(use-package which-key
  :config
  (which-key-mode -1))

;; GPG pinentry
;; let's get encryption established
(use-package pinentry
  :config
  (setenv "GPG_AGENT_INFO" nil)  ;; use emacs pinentry
  (setq auth-source-debug t)
  (setq epg-gpg-program "gpg2")
  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start)
  ;; Enable encryption for org :crypt: tags
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))

(use-package pass
  :config
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-do-cache nil))

;; Email indexing and configuration
(use-package notmuch
  :demand t
  :load-path
  (lambda ()
    (shell-command-to-string
     "readlink -f \"$(which notmuch-emacs-mua)/../../share/emacs/site-lisp/\" | tr -d '\n'"))
  :config
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        mail-from-style nil
        user-full-name "Gongqi Huang"
        user-mail-address "gh0477@cs.princeton.edu"
        smtpmail-debug-info t
        smtpmail-debug-verb t)

  (defvar smtp-accounts
    '(;; Princeton CS
      (ssl                       ;; Protocol
       "gh0477@cs.princeton.edu" ;; Address matched in From field
       "smtp.cs.princeton.edu"   ;; SMTP server
       587                       ;; SMTP port
       "gh0477"                  ;; User
       (auth-source-pass-get 'secret "cs.princeton.edu/passcode")) ;; Password
      ;; Personal
      (ssl
       "gongqi@thuh.zone"
       "mail.gongqi.zone"
       587
       "gongqi@thuh.zone"
       (auth-source-pass-get 'secret "thuh.zone/gongqi"))
      ))

  (defun set-smtp (mech server port user password)
    "Set related SMTP variables for supplied parameters."
    (setq smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtpmail-stream-type nil
          smtpmail-auth-credentials (list (list server port user password))
          smtpmail-auth-supported (list mech)
          smtpmail-starttls-credentials nil)
    (message "Setting SMTP server to `%s:%s' for user `%s'."
             server port user))

  (defun set-smtp-ssl (server port user password &optional key cert)
    "Set related SMTP and SSL variables for supplied parameters."
    (setq starttls-use-gnutls t
          starttls-gnutls-program "gnutls-cli"
          starttls-extra-arguments nil
          smtpmail-smtp-server server
          smtpmail-smtp-service port
          smtpmail-smtp-user user
          smtpmail-stream-type 'starttls
          ;; No effect. Outdated?
          ;; smtpmail-auth-credentials (list (list server port user password))
          ;; smtpmail-starttls-credentials (list (list server port key cert))
          )
    (message
     "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
     server port user))

  (defun change-smtp ()
    "Change the SMTP server according to the current from line."
    (save-excursion
      (cl-loop with from = (save-restriction
                             (message-narrow-to-headers)
                             (message-fetch-field "from"))
               for (auth-mech address . auth-spec) in smtp-accounts
               when (string-match address from)
               do (cond
                   ((memq auth-mech '(cram-md5 plain login))
                    (cl-return (apply 'set-smtp (cons auth-mech auth-spec))))
                   ((eql auth-mech 'ssl)
                    (cl-return (apply 'set-smtp-ssl auth-spec)))
                   (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
               finally (error "Cannot infer SMTP information."))))

  (defadvice smtpmail-via-smtp
      (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
    "Call `change-smtp' before every `smtpmail-via-smtp'."
    (with-current-buffer smtpmail-text-buffer (change-smtp)))

  (ad-activate 'smtpmail-via-smtp))

;; Enable pinyin input method
(use-package pyim
  :config
  (require 'pyim-basedict)
  (pyim-basedict-enable)
  (setq default-input-method "pyim")
  (setq pyim-page-length 7))

;; LilyPond mode
(progn
  (autoload 'LilyPond-mode "lilypond-mode")
  (setq auto-mode-alist
        (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))
  (add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
     "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8"
     default))
 '(package-selected-packages '(doom-themes exwm vterm use-package evil))
 '(warning-suppress-log-types '(((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
