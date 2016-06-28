(setq font-use-system-font t
      inhibit-startup-message t
      initial-scratch-message nil
      default-tab-width 4
      show-paren-delay 0
      scroll-conservatively 100000
      scroll-margin 0)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(blink-cursor-mode -1)
(column-number-mode 1)
(display-time-mode t)
(show-paren-mode t)
(global-hl-line-mode)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; Undo Tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-diff t)

;; Forbid tabs by default
;; Use C-q to insert TAB (C-q <tab>)
(setq-default indent-tabs-mode nil)

(setq whitespace-style
      '(trailing lines space-before-tab indentation space-after-tab)
      whitespace-line-column 100)

(global-whitespace-cleanup-mode)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Theme
;(load-theme 'solarized-dark t)
;(load-theme 'solarized-light t)
(load-theme 'zenburn t)
;(load-theme 'subatomic t)

(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "YAS"))
(eval-after-load "eldoc" '(diminish 'eldoc-mode "ED"))
(eval-after-load "paredit" '(diminish 'paredit-mode "PE"))
(eval-after-load "tagedit" '(diminish 'tagedit-mode "TE"))
(eval-after-load "projectile" '(diminish 'projectile-mode "Prj"))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode "Undo"))

(require 'powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(setq powerline-default-separator 'zigzag)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


(require 'company)
(global-company-mode)

;; Smex
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" my-persistence-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;; Perspective
(persp-mode)

;; Projectile
(require 'projectile)
(setq projectile-cache-file (expand-file-name
                             "projectile.cache"
                             my-persistence-dir)
      projectile-known-projects-file (expand-file-name
                                      "projectile-bookmarks.eld"
                                      my-persistence-dir)
      projectile-switch-project-action 'projectile-persp-switch-project)
(projectile-global-mode t)

;; Perspective - Projectile integration
(require 'persp-projectile)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" my-persistence-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Bookmarks
(setq
 bookmark-default-file (expand-file-name "bookmarks" my-persistence-dir)
 bookmark-save-flag 1)

;; Quick navigation
(require 'avy)

(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t
      aw-skope "frame")

;; Eshell
(setq eshell-directory-name (expand-file-name "eshell" my-persistence-dir))


;; Mutiple Curssors
(require 'multiple-cursors)
(setq mc/list-file (expand-file-name ".mc-lists.el" my-persistence-dir))

;; Hide unmatched lines
(require 'mc-hide-unmatched-lines-mode)
(setq hum/lines-to-expand 1)


;; Start maximized
;; Alternative way:
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Also following line to ~/.Xresources to have a maximized window immediately
;; emacs.fullscreen: maximized
;(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'ui-setup)
