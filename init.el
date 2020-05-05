;;; init.el --- The Emacs configuration

;;; Commentary:

;;; Code:

(setq
 ;; Backups - set it up early to not be affected by any errors below
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-make-backup-files t

 font-use-system-font t
 inhibit-startup-message t
 initial-scratch-message nil
 scroll-conservatively 100000
 scroll-margin 0

 ;; Don't ring on any ocasion
 ring-bell-function 'ignore
 ;; Increse GC threshold to 100Mb
 gc-cons-threshold 100000000)

(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))
;; Packages
(eval-when-compile
  (require 'package)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("org" . "http://orgmode.org/elpa/")
          ("melpa" . "http://melpa.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p '(diminish use-package))
    (unless (package-installed-p p)
      (package-install p)))

  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package no-littering
  :config
  (add-to-list 'load-path no-littering-etc-directory)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; UX
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable the functionality disabled by default
(put 'narrow-to-region 'disabled nil)

(use-package zenburn-theme
  :config
  ;(load-theme 'zenburn t)
  )

(use-package doom-themes
  :config
  (load-theme 'doom-one t))


;; Start maximized
;; Alternative way:
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Also following line to ~/.Xresources to have a maximized window immediately
;; emacs.fullscreen: maximized
;(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))
(add-to-list 'default-frame-alist '(undecorated . t))

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Forbid tabs by default
;; Use C-q to insert TAB (C-q <tab>)
(setq-default indent-tabs-mode nil)

;; Default tab width is 4
(setq-default tab-width 4)

;; Show column number on mode line
(column-number-mode 1)

;; Show buffer size on mode line
(size-indication-mode t)

;; Highlight current line
(global-hl-line-mode)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Unclutter the UI
(menu-bar-mode -1)
(tool-bar-mode -1)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show time on the mode line, only in text terminal
(when (not (display-graphic-p))
  (display-time-mode t))

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

;; Highlight matching paren
(use-package paren
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

(use-package whitespace-cleanup-mode
  :config
  (setq
   whitespace-style
   '(trailing lines space-before-tab indentation space-after-tab)
   whitespace-line-column 100)
  (global-whitespace-cleanup-mode))

  (use-package beginend
    :diminish beginend-global-mode
    :hook
    ((dired-mode . beginend-dired-mode)
     (prog-mode . beginend-prog-mode)
     (message-mode . beginend-message-mode)
     (mu4e-view-mode . beginend-message-mode)
     (mu4e-compose-mode . beginend-message-mode)
     (occur-mode . beginend-occur-mode)
     (ibuffer-mode-hook . beginend-ibuffer-mode)
     (magit-status-mode . beginend-magit-status-mode))
    :config
    (dolist (mode beginend-modes) (diminish (cdr mode))))

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

(use-package wgrep)

(use-package eyebrowse
  :bind (("M-1" . eyebrowse-switch-to-window-config-1)
         ("M-2" . eyebrowse-switch-to-window-config-2)
         ("M-3" . eyebrowse-switch-to-window-config-3))
  :config
  (setq
   eyebrowse-new-workspace t)
  (eyebrowse-mode))

(use-package swiper
  :diminish 'ivy-mode
  :config
  (use-package counsel)
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

(use-package lsp-mode
  :commands lsp
  :config
  (use-package lsp-ui
    :config
    (defvar lsp-intelephense-storage-path)
    (defvar lsp-intelephense-stubs)
    (setq lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-file-watch-threshold 20000
          lsp-intelephense-storage-path
          (no-littering-expand-var-file-name "intelephense")
          lsp-intelephense-stubs ["apache" "bcmath" "bz2" "calendar" "com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant" "exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase" "intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli" "oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql" "pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode" "Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets" "sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg" "sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "xml" "xmlreader" "xmlrpc" "xmlwriter" "Zend OPcache" "zip" "zlib" "redis"]))
  (use-package company-lsp
    :commands company-lsp))

(use-package multiple-cursors
  :config
  (require 'mc-hide-unmatched-lines-mode)
  (setq
   hum/lines-to-expand 1)
  :bind (("C-S-p" . mc/mark-pop)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; emacs lisp
(use-package elisp-mode
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package ace-window
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-dispatch-always t
           aw-background t
           aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package avy
  :bind (("M-g s" . avy-goto-char-timer)
         ("M-g l" . goto-line))
  :config
  (setq avy-background t))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go)
         ("M-g O" . dumb-jump-go-other-window)
         ("M-g b" . dumb-jump-back))
  :config
  (dumb-jump-mode))

(use-package flycheck
  :demand
  :diminish 'flycheck-mode
  :config
  (declare-function global-flycheck-mode "ext:flycheck")
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-checker-error-threshold nil)
  (global-flycheck-mode))

(use-package company
  :demand
  :diminish 'company-mode
  :bind (("C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  :config
  (global-company-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; TODO Configure this
(use-package spaceline-config
  :ensure spaceline
  :config
  (setq powerline-default-separator 'wave)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (when (fboundp 'spaceline-emacs-theme)
    (spaceline-emacs-theme)))

(use-package undo-tree
  :demand
  :diminish 'undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))

(use-package web-mode
  :mode (("\\.html\\.twig" . web-mode)
         ("\\.html" . web-mode)
         ("\\.vue" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

(use-package evil
  :commands evil-mode)

(use-package ag
  :commands ag)

(use-package magit)

;; TODO Possibly replace with smartparens
(use-package paredit
  :diminish 'paredit-mode
  :commands paredit-mode
  :bind (("C-M-u" . paredit-backward-up)
         ("C-M-n" . paredit-forward-up)
         ("M-S" . paredit-splice-sexp-killing-backward)
         ("M-R" . paredit-raise-sexp)
         ("M-(" . paredit-wrap-round)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

(use-package eldoc
  :diminish 'eldoc-mode
  :commands eldoc-mode)

(use-package projectile
  :demand
  :diminish 'projectile-mode
  :config
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf
        projectile-enable-caching t)
  (projectile-mode t)
  (use-package counsel-projectile
    :config
    (counsel-projectile-mode)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode)))

(use-package smart-semicolon)

(use-package yaml-mode
  :mode ("\\.yml" . yaml-mode))

(use-package elpy
  :config
  (setq
   elpy-rpc-virtualenv-path (no-littering-expand-var-file-name "elpy/rpc-venv")
   elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :interpreter ("ipython" . python-mode)
  :config
  (setq python-indent-offset 4
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (elpy-enable))

;;; npm i -g intelephense
(use-package php-mode
  :mode ("\\.php" . php-mode)
  :hook (php-mode . (lambda
                      ()
                      (subword-mode 1)
                      (lsp)
                      (smart-semicolon-mode)))
  :config
  (setq php-mode-template-compatibility nil
        php-mode-coding-style 'psr2))

(use-package haskell-mode)

(use-package clojure-mode
  :config
  (use-package cider
    :config
    (setq
     nrepl-hide-special-buffers t
     ;; go right to the REPL buffer when it's finished connecting
     cider-repl-pop-to-buffer-on-connect t
     ;; When there's a cider error, show it's buffer and switch to it
     cider-show-error-buffer t
     cider-auto-select-error-buffer t
     ;; Wrap when navigating history.
     cider-repl-wrap-history t)
  (use-package clj-refactor)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  :bind (:map clojure-mode-map
              ("<f5>" . cider-ns-refresh))))

(use-package htmlize)

(use-package org
  :pin org
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
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "PAUSED(p@)" "|" "DONE(d!)" "CANCELLED(c@)"))
   org-M-RET-may-split-line nil
   org-capture-templates '(
    ("l" "Link" entry (file+headline "~/safe/org/tasks.org" "Links")
     "* %a\n  %?\n  Added: %U")
    ("L" "Link with quote" entry (file+headline "~/safe/org/tasks.org" "Links")
     "* %a\n  #+BEGIN_QUOTE\n  %i\n  #+END_QUOTE\n  %?\n  Added: %U")))
  (org-link-set-parameters
   "thunderlink"
   :follow (lambda (path)
     "Opens an email in Thunderbird with ThunderLink."
     (start-process "myname" nil "thunderbird" "-thunderlink" (concat "thunderlink:" path))))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   :map org-mode-map
   ("C-c d" . org-decrypt-entry)))

(use-package pass)

(use-package auth-source-pass
  :pin melpa
  :config
  (auth-source-pass-enable))

(dolist  (p '(my-mu4e my-transmission))
  (require p nil t))

(use-package hydra
  :bind (("M-i" . hydra-file/body)
         ("M-p" . hydra-dev/body))
  :config
  (defhydra hydra-file (:color teal :hint nil)
    "
%s(concat \"Project: \" (or (projectile-project-root) \"none\"))
  _f_: project file     _a_: project ag           _i_: project ibuffer  _g_: magit status
  _F_: file             _A_: ag                   _I_: ibuffer          _G_: magit dispatch
  _d_: project dir      _o_: project multi-occur  _b_: project buffer   _h_: magit file dispatch
  _D_: dir              _O_: multi-occur          _B_: buffer           _p_: switch project
  _r_: project recentf
  _R_: recentf
"
    ("f" counsel-projectile-find-file)
    ("F" counsel-find-file)
    ("d" counsel-projectile-find-dir)
    ("D" counsel-dired)
    ("r" projectile-recentf)
    ("R" counsel-recentf)
    ("a" counsel-projectile-ag)
    ("A" counsel-ag)
    ("o" projectile-multi-occur)
    ("O" multi-occur)
    ("b" counsel-projectile-switch-to-buffer)
    ("B" counsel-switch-buffer)
    ("i" projectile-ibuffer)
    ("I" counsel-ibuffer)
    ("g" magit-status)
    ("G" magit-dispatch)
    ("h" magit-file-dispatch)
    ("p" counsel-projectile-switch-project)
    ("q" nil "cancel" :color blue))
  (defhydra hydra-dev (:color amaranth :hint nil)
    "
  _e_: list errors  _d_: toggle doc
  _j_: next error
  _k_: prev error
"
    ("e" (lambda () (interactive) (flycheck-list-errors) (set-frame-selected-window nil (get-buffer-window flycheck-error-list-buffer))) :color blue)
    ("j" flycheck-next-error)
    ("k" flycheck-previous-error)
    ("d" (lambda () (interactive) (with-selected-window (frame-selected-window) (lsp-describe-thing-at-point))) :color blue)
    ("q" nil "cancel" :color blue)))

(provide 'init)
;;; init.el ends here
