;;; init.el --- The Emacs configuration

;;; Commentary:

;;; Code:
(defconst my-base-dir "~/.emacs.d"
  "The root of Emacs configuration.")
(defconst my-settings-dir (expand-file-name "settings" my-base-dir)
  "A place to store custom configs.")
(defconst my-persistence-dir (expand-file-name "persistence" my-base-dir)
  "A root of local data files.")
(defconst my-backup-dir (expand-file-name "backup" my-persistence-dir)
  "A root of backup and autosave files.")
(defconst my-lein-command "/home/spaun/bin/lein")

(dolist (dir `(,my-settings-dir ,my-persistence-dir ,my-backup-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

;; Backups - set it up early to not be affected by any errors below
(setq backup-by-copying t
      backup-directory-alist `((".*" . ,my-backup-dir))
      auto-save-file-name-transforms `((".*" ,my-backup-dir t))
      auto-save-list-file-prefix my-backup-dir
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(add-to-list 'load-path my-settings-dir)

(setq custom-file (expand-file-name "custom.el" my-settings-dir))
(if (file-exists-p custom-file) (load custom-file))

(setq ring-bell-function 'ignore)

;; Packages

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(diminish
    solarized-theme
    subatomic-theme
    use-package
    zenburn-theme
    doom-themes))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(require 'use-package)

(setq use-package-always-ensure t)

;; UX
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable the functionality disabled by default
(put 'narrow-to-region 'disabled nil)

;(load-theme 'doom-nord-light t)
(load-theme 'zenburn t)
;(load-theme 'solarized-dark t)
;(load-theme 'solarized-light t)
;(load-theme 'subatomic t)

(setq font-use-system-font t
      inhibit-startup-message t
      initial-scratch-message nil
      scroll-conservatively 100000
      scroll-margin 0)

;; Start maximized
;; Alternative way:
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Also following line to ~/.Xresources to have a maximized window immediately
;; emacs.fullscreen: maximized
;(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

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

;; Highlight matching paren
(use-package paren
  :demand
  :config
  (setq show-paren-delay 0)
  (show-paren-mode t))

(use-package whitespace-cleanup-mode
  :demand
  :config
  (setq
   whitespace-style
   '(trailing lines space-before-tab indentation space-after-tab)
   whitespace-line-column 100)
  (global-whitespace-cleanup-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package recentf
  :demand
  :bind (("C-x f" . recentf-open-files))
  :config
  (defun my-recentf-exclude-p (file)
    "Recentf exclude predicate (borrowed from bbatsov's prelude"
    (let ((file-dir (file-truename (file-name-directory file))))
      (seq-some
       (lambda (dir) (string-prefix-p dir file-dir))
       (seq-map 'file-truename (list my-persistence-dir package-user-dir)))))
  (add-to-list 'recentf-exclude 'my-recentf-exclude-p)
  (setq
   recentf-save-file (expand-file-name "recentf" my-persistence-dir)
   recentf-max-saved-items 500
   recentf-max-menu-items 15
   recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package swiper
  :ensure t
  :config
  (use-package counsel
    :ensure t)
  (defvar ivy-use-virtual-buffers nil)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package lsp-mode
  :commands lsp
  :config
  (use-package lsp-ui
    :config
    (setq lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil))
  (use-package company-lsp
    :commands company-lsp))

(use-package bookmark
  :demand
  :config
  (setq
   bookmark-default-file (expand-file-name "bookmarks" my-persistence-dir)
   bookmark-save-flag 1))

(use-package eshell
  :config
  (setq
   eshell-directory-name (expand-file-name "eshell" my-persistence-dir)))

(use-package multiple-cursors
  :config
  (require 'mc-hide-unmatched-lines-mode)
  (setq
   mc/list-file (expand-file-name "mc-lists.el" my-persistence-dir)
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
  :ensure t
  :bind (("M-g s" . avy-goto-char-timer)
         ("M-g l" . goto-line))
  :config
  (setq avy-background t))

(use-package flycheck
  :demand
  :diminish "FC"
  :config
  (declare-function global-flycheck-mode "ext:flycheck")
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode))

(use-package company
  :demand
  :diminish 'company-mode
  :bind (("C-<tab>" . company-complete))
  :config
  (global-company-mode))

(use-package rainbow-delimiters
  :demand
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; TODO Configure this
(use-package spaceline-config
  :ensure spaceline
  :demand
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
  :mode "\\.html\\.twig"
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package evil
  :commands evil-mode)

(use-package ag
  :commands ag)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)))

;; TODO Possibly replace with smartparens
(use-package
  paredit
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
  (setq projectile-cache-file
        (expand-file-name "projectile.cache" my-persistence-dir)
        projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" my-persistence-dir))
  (projectile-mode t))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :mode (("yasnippet/snippets" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode)))

;;; npm i -g intelephense
(use-package php-mode
  :hook (php-mode . (lambda () (subword-mode 1) (lsp)))
  :config
  (setq php-mode-template-compatibility nil
        php-mode-coding-style 'psr2))

(use-package haskell-mode)

(use-package clojure-mode
  :config
  (use-package cider
    :config
    (setq
     cider-lein-command my-lein-command
     nrepl-hide-special-buffers t
     ;; go right to the REPL buffer when it's finished connecting
     cider-repl-pop-to-buffer-on-connect t
     ;; When there's a cider error, show it's buffer and switch to it
     cider-show-error-buffer t
     cider-auto-select-error-buffer t
     ;; Where to store the cider history.
     cider-repl-history-file (expand-file-name "cider-history" my-persistence-dir)
     ;; Wrap when navigating history.
     cider-repl-wrap-history t)
  (use-package clj-refactor)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  :bind (:map clojure-mode-map
              ("<f5>" . cider-ns-refresh))))

(use-package org
  :config
  (require 'epa-file)
  (require 'org-crypt)
  (declare-function org-link-set-parameters "ext:org")
  (declare-function org-crypt-use-before-save-magic "ext:org-crypt")
  (defvar org-html-doctype)
  (defvar org-html-htmlize-output-type)
  (epa-file-enable)
  (org-crypt-use-before-save-magic)
  (setq
   org-tags-exclude-from-inheritance (quote ("crypt"))
   auto-save-default nil
   org-src-fontify-natively t
   org-return-follows-link t
   org-agenda-files '("~/org/tasks.org")
   org-archive-location "~/org/tasks-archive.org::* Finished Tasks"
   org-link-frame-setup '((file . find-file))
   org-html-doctype "html5"
   org-html-htmlize-output-type 'css
   org-log-into-drawer "LOGBOOK"
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "PAUSED(p!)" "|" "DONE(d!)" "CANCELLED(c@)"))
   org-M-RET-may-split-line nil)
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

(use-package htmlize)

(provide 'init)
;;; init.el ends here
