;;; init.el --- The Emacs configuration

;;; Commentary:

;;; Code:
(require 'use-package)

(use-package use-package
  :no-require
  :custom
  (use-package-imenu-support t)
  (package-native-compile t)
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
  :config
  (require 'package)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(use-package emacs
  :config
  (set-language-environment "UTF-8")
  ;; Forbid tabs by default
  ;; Use C-q to insert TAB (C-q <tab>)
  (setq-default
   y-or-n-p-use-read-key t
   indent-tabs-mode nil
   tab-width 4)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq
   ;; Backups - set it up early to not be affected by any errors below
   backup-by-copying nil
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-make-backup-files nil

   font-use-system-font t
   inhibit-startup-message t
   initial-scratch-message nil
   scroll-conservatively 100000
   scroll-margin 0

   ;; Don't ring on any occasion
   ring-bell-function 'ignore
   ;; Increse GC threshold to 200Mb
   gc-cons-threshold 200000000
   ;; Real emacs knights don't use shift to mark things
   shift-select-mode nil
   ;; Move files to trash when deleting
   delete-by-moving-to-trash t
   read-process-output-max (* 1024 1024))

  ;; Start maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(undecorated . t))

  (auto-compression-mode t)
  (column-number-mode 1)
  (size-indication-mode t)
  (global-hl-line-mode)
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ;; Enable the functionality disabled by default
  (put 'narrow-to-region 'disabled nil)

  ;; Show time on the mode line, only in text terminal
  (display-time-mode (not (display-graphic-p)))

  (dotimes (n 10)
    (global-unset-key (kbd (format "C-%d" n)))
    (global-unset-key (kbd (format "M-%d" n)))))

(use-package server
  :config
  (setq server-socket-dir (getenv "XDG_RUNTIME_DIR")))

(use-package delight
  :ensure t)

(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

(use-package autorevert
  :delight auto-revert-mode)

(use-package abbrev
  :delight)

(use-package subword
  :delight
  :hook prog-mode)

(use-package no-littering
  :ensure t
  :config
  (add-to-list 'load-path no-littering-etc-directory)
  (setq custom-file
        (no-littering-expand-etc-file-name "custom.el")
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist
        (list (cons ".*" (no-littering-expand-var-file-name "backup/")))))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

(use-package flyspell
  :delight
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

;; Highlight matching paren
(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

(use-package whitespace-cleanup-mode
  :ensure t
  :delight
  :config
  (setq
   whitespace-style
   '(trailing lines space-before-tab indentation space-after-tab)
   whitespace-line-column 100)
  (global-whitespace-cleanup-mode))

(use-package beginend
  :ensure t
  :delight beginend-global-mode
  :hook
  ((dired-mode . beginend-dired-mode)
   (prog-mode . beginend-prog-mode)
   (message-mode . beginend-message-mode)
   (mu4e-view-mode . beginend-message-mode)
   (mu4e-compose-mode . beginend-message-mode)
   (mu4e-headers-mode . beginend-mu4e-headers-mode)
   (occur-mode . beginend-occur-mode)
   (ibuffer-mode-hook . beginend-ibuffer-mode)
   (magit-status-mode . beginend-magit-status-mode))
  :config
  (beginend-define-mode mu4e-headers-mode
    (progn
      (point-min))
    (progn
      (point-max)
      (forward-line -1)
      (end-of-line)))
  (dolist (mode beginend-modes) (delight (cdr mode) nil 'beginend)))

(use-package recentf
  :bind (("C-x f" . recentf-open-files))
  :config
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  (add-to-list 'recentf-exclude  no-littering-etc-directory)
  (add-to-list 'recentf-exclude  no-littering-var-directory)
  (setq
   recentf-max-saved-items 500
   recentf-max-menu-items 15
   recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package wgrep
  :ensure t)

(use-package tab-bar
  :bind
  (("M-1" . (lambda () (interactive) (tab-bar-select-tab-by-name "ws1")))
   ("M-2" . (lambda () (interactive) (tab-bar-select-tab-by-name "ws2")))
   ("M-3" . (lambda () (interactive) (tab-bar-select-tab-by-name "mail")))
   ("M-4" . (lambda () (interactive) (tab-bar-select-tab-by-name "org"))))
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  :config
  (add-hook 'after-make-frame-functions  #'(lambda (frame) (select-frame frame) (init-workspaces)))
  (defun init-workspaces ()
    (tab-bar-mode)
    (tab-bar-rename-tab "ws1")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "ws2")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "mail")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "org")
    (tab-bar-select-tab "ws1"))
  (init-workspaces))

(use-package swiper
  :ensure t
  :delight ivy-mode
  :demand
  :config
  (defvar ivy-use-virtual-buffers nil)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch-backward)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package counsel
  :ensure t
  :after swiper)

(use-package amx
  :ensure t
  :after ivy-mode)

(use-package lsp-mode
  :ensure t
  :delight
  :commands lsp-deferred
  :init
  (setq lsp-use-plists t)
  :custom
  (lsp-keymap-prefix "M-q"))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (defvar lsp-intelephense-storage-path)
  (defvar lsp-intelephense-stubs)
  (defvar lsp-intelephense-completion-fully-qualify-global-constants-and-functions)
  (setq
   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   lsp-file-watch-threshold 20000
   lsp-intelephense-completion-fully-qualify-global-constants-and-functions t
   lsp-intelephense-storage-path (no-littering-expand-var-file-name "intelephense")
   lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "redis"]))

(use-package multiple-cursors
  :ensure t
  :config
  (require 'mc-hide-unmatched-lines-mode)
  (setq
   hum/lines-to-expand 1)
  :bind (("C-S-p" . mc/mark-pop)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package ace-window
  :ensure t
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-dispatch-always t
           aw-background t
           aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :config
  (setq avy-background t))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package flycheck
  :ensure t
  :demand
  :delight
  :config
  (declare-function global-flycheck-mode "ext:flycheck")
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-checker-error-threshold nil)
  (global-flycheck-mode))

(use-package company
  :ensure t
  :demand
  :delight
  :bind (("C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  :hook
  (after-init . global-company-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; TODO Configure this
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-define-segment my-project-name
    "The current project name"
    (propertize (format "<%s>"
                        (or (and (boundp 'project-name) project-name)
                            (when-let ((project-current (project-current)))
                              (file-name-nondirectory (directory-file-name (project-root project-current))))
                            "-")) 'face 'bold))
  (setq powerline-default-separator 'wave)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (spaceline-toggle-my-project-name-on)
  (when (fboundp 'spaceline-emacs-theme)
    (spaceline-emacs-theme 'my-project-name)))

(use-package undo-tree
  :ensure t
  :demand
  :delight
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist (list (cons "." (no-littering-expand-var-file-name "undo"))))
:hook
  (after-init . global-undo-tree-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\.twig" . web-mode)
         ("\\.html" . web-mode)
         ("\\.vue" . (lambda () (web-mode) (lsp))))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-script-padding 0
        web-mode-style-padding 0
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

(use-package evil
  :ensure t
  :commands evil-mode)

(use-package ag
  :ensure t
  :commands ag)

(use-package magit
  :ensure t
  :config
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map))

;; TODO Possibly replace with smartparens
(use-package paredit
  :ensure t
  :delight
  :commands paredit-mode
  :hook ((emacs-lisp-mode clojure-mode) . paredit-mode)
  :bind (("C-M-u" . paredit-backward-up)
         ("C-M-n" . paredit-forward-up)
         ("M-S" . paredit-splice-sexp-killing-backward)
         ("M-R" . paredit-raise-sexp)
         ("M-)" . paredit-wrap-round)
         ("M-]" . paredit-wrap-square)
         ("M-}" . paredit-wrap-curly)))

(use-package eldoc
  :delight
  :commands eldoc-mode
  :hook (prog-mode . eldoc-mode))

(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode)))

(use-package smart-semicolon
  :ensure t
  :delight
  :hook ((php-mode typescript-mode) . smart-semicolon-mode))

(use-package python
  :mode ("\\.py" . python-mode)
  :interpreter ("ipython" . python-mode)
  :config
  (setq python-indent-offset 4
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

(use-package php-mode
  :ensure t
  :mode ("\\.php" . php-mode)
  :hook (php-mode . lsp-deferred)
  :config
  (setq php-mode-template-compatibility nil
        php-mode-coding-style 'psr2))

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts" . typescript-mode)
         ("\\.tsx" . typescript-mode))
  :hook (typescript-mode . lsp-deferred))

(use-package toml-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook (rust-mode . lsp-deferred))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . (lambda ()
                          (interactive)
                          (when (derived-mode-p 'go-mode)
                            (lsp-organize-imports)
                            (lsp-format-buffer)))))
  :bind (:map go-mode-map
              ("<f9>" . (lambda () (interactive) (compile "go run .")))))

(use-package haskell-mode
  :ensure t)

(use-package clojure-mode
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package org
  :pin gnu
  :config
  (require 'epa-file)
  (require 'org-crypt)
  (require 'org-protocol)
  (declare-function org-link-set-parameters "ext:org")
  (declare-function org-crypt-use-before-save-magic "ext:org-crypt")
  (defvar org-html-doctype)
  (defvar org-html-htmlize-output-type)
  (defvar org-crypt-disable-auto-save)
  (defvar org-crypt-key)
  (defvar org-capture-templates)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq
   org-tags-exclude-from-inheritance '("crypt")
   org-crypt-disable-auto-save 'encrypt
   org-crypt-key (getenv "CRYPTO_KEY")
   org-src-fontify-natively t
   org-return-follows-link t
   org-agenda-files '("~/safe/org/tasks.org")
   org-archive-location (format-time-string
                         "~/safe/org/archive/tasks-%Y.org::* Finished Tasks"
                         (current-time))
   org-link-frame-setup '((file . find-file))
   org-html-doctype "html5"
   org-html-htmlize-output-type 'css
   org-log-into-drawer "LOGBOOK"
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "PAUSED(p@)" "WAIT(w!)" "|" "DONE(d!)" "CANCELLED(c@)"))
   org-M-RET-may-split-line nil
   org-capture-templates '(
    ("l" "Link" entry (file+headline "~/safe/org/bookmarks.org" "Links")
     "* %a\n  %?\n  Added: %U")
    ("L" "Link with quote" entry (file+headline "~/safe/org/bookmarks.org" "Links")
     "* %a\n  #+BEGIN_QUOTE\n  %i\n  #+END_QUOTE\n  %?\n  Added: %U")))
  (org-link-set-parameters
   "thunderlink"
   :follow (lambda (path)
     "Opens an email in Thunderbird with ThunderLink."
     (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-c d" . org-decrypt-entry)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/safe/org-roam")
  (org-roam-completion-everywhere t)
  :config
  (org-roam-db-autosync-enable)
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)))

(use-package pass
  :ensure t)

(use-package auth-source-pass
  :ensure t
  :pin melpa
  :config
  (auth-source-pass-enable))

(use-package link-hint
  :ensure t)

(use-package move-text
  :ensure t
  :bind
  ("M-p" . move-text-up)
  ("M-n" . move-text-down))

(use-package orgalist
  :ensure t
  :commands orgalist-mode)

(load custom-file)

(dolist  (p '(my-mu4e my-transmission my-functions))
  (require p nil t))

(defun my-project-ag (&optional options)
  "Search the current project with ag.

OPTIONS, if non-nil, is a string containing additional options to
be passed to ag. It is read from the minibuffer if the function
is called with a `\\[universal-argument]' prefix argument."
  (interactive)
  (if (project-current t)
    (let* ((ivy--actions-list (copy-sequence ivy--actions-list))
           (ignored "")
           (counsel-ag-base-command
            (let ((counsel-ag-command counsel-ag-base-command))
              (counsel--format-ag-command ignored "%s"))))

      ;; `counsel-ag' requires a single `\\[universal-argument]'
      ;; prefix argument to prompt for initial directory and a double
      ;; `\\[universal-argument]' prefix argument to prompt for extra
      ;; options. But since the initial directory is always the
      ;; project root here, prompt for ortpions with a single
      ;; `\\[universal-argument]' prefix argument prefix argument as
      ;; well.
      (when (= (prefix-numeric-value current-prefix-arg) 4)
        (setq current-prefix-arg '(16)))
      (counsel-ag nil (project-root (project-current))
                  options
                  (concat (car (if (listp counsel-ag-base-command)
                                   counsel-ag-base-command
                                 (split-string counsel-ag-base-command)))
                          ": ")))))

(defun my-project-rg (&optional options)
  "Search the current project with rg.

OPTIONS, if non-nil, is a string containing additional options to
be passed to rg. It is read from the minibuffer if the function
is called with a `\\[universal-argument]' prefix argument."
  (interactive)
  (if (project-current t)
    (let* ((ivy--actions-list (copy-sequence ivy--actions-list))
           (ignored "")
           (counsel-rg-base-command
            (let ((counsel-ag-command counsel-rg-base-command))
              (counsel--format-ag-command ignored "%s"))))
      ;; `counsel-rg' requires a single `\\[universal-argument]'
      ;; prefix argument to prompt for initial directory and a double
      ;; `\\[universal-argument]' prefix argument to prompt for extra
      ;; options. But since the initial directory is always the
      ;; project root here, prompt for ortpions with a single
      ;; `\\[universal-argument]' prefix argument prefix argument as
      ;; well.
      (when (= (prefix-numeric-value current-prefix-arg) 4)
        (setq current-prefix-arg '(16)))
      (counsel-rg nil
                  (project-root (project-current))
                  options
                  (concat (car (if (listp counsel-rg-base-command)
                                   counsel-rg-base-command
                                 (split-string counsel-rg-base-command)))
                          ": ")))))

(provide 'init)

;;; init.el ends here
