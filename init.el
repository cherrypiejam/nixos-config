;; Set default font
(set-frame-font "Hack Nerd Font-9" nil t)
(setq default-frame-alist '((font . "Hack Nerd Font-9")))
(tooltip-mode -1)

;; Make more room
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 1)

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
     (magit magit-repos magit-submodule))))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config)
  (set-face-foreground 'mode-line "black")
  (set-face-background 'mode-line "DarkOliveGreen4"))

(defun wrapper/format/systemd-run (command)
  "Format COMMAND with systemd-run"
  (format "systemd-run -q --user --slice=app.slice --scope -- %s" command))

(defun wrapper/scoped-run (command)
  "Run COMMAND with systemd-run"
  (start-process-shell-command command nil (wrapper/format/systemd-run command)))

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
             (wrapper/scoped-run "brightnessctl --device 'amdgpu_bl0' set 5%-")))
	      (,(kbd "<XF86MonBrightnessUp>") .
	       (lambda ()
             (interactive)
             (wrapper/scoped-run "brightnessctl --device 'amdgpu_bl0' set 5%+")))
	      ;; Others
          ;; 's-l': Lock screen
          ([?\s-l] . (lambda ()
                       (interactive)
                       (wrapper/scoped-run "xautolock -locknow")))))
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
  (setq exwm-randr-workspace-monitor-plist '(2 "DP-3" 3 "DP-4"))
  (exwm-randr-enable)
  ;; Startup tasks
  (add-hook 'exwm-init-hook
        (lambda ()
          (wrapper/scoped-run "nm-applet")
          (wrapper/scoped-run "blueman-applet")))
  ;; Enable the system tray
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 24) ;; default 22
  (exwm-systemtray-enable)
  ;; Using xim input
  ;; (require 'exwm-xim)
  ;; (exwm-xim-enable)
  ;; (push ?\C-\\ exwm-input-prefix-keys)
  ;; Enable EXWM
  (exwm-enable))

(use-package exwm-outer-gaps
  :config
  (exwm-outer-gaps-set-all 20 nil)
  ;; Disable by default
  (exwm-outer-gaps-mode -1))

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
  ;; Additional bindings
  (evil-define-key '(normal insert) 'vterm-mode-map
    (kbd "C-c C-q") 'vterm-send-next-key)
  (defun nterm ()
    "Open a new vterm instance with a unique buffer name"
    (interactive)
    (let ((counter 0))
      (while (get-buffer (format "%s<%d>" vterm-buffer-name counter))
        (setq counter (1+ counter)))
      (vterm (format "%s<%d>" vterm-buffer-name counter)))))

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
  ((nix-mode rust-mode c-mode scala-mode)
   . eglot-ensure)
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
  (which-key-mode))

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

;; Email indexing
(use-package notmuch
  :demand t
  :load-path
  (lambda ()
    (shell-command-to-string
     "readlink -f \"$(which notmuch-emacs-mua)/../../share/emacs/site-lisp/\" | tr -d '\n'")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages '(doom-themes exwm vterm use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
