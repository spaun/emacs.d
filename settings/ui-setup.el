(setq font-use-system-font t
      inhibit-startup-message t
      default-tab-width 4
      show-paren-delay 0
      ;; scroll-preserve-screen-position 1
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

;; Forbid tabs by default
;; Use C-q to insert TAB (C-q <tab>)
(setq-default indent-tabs-mode nil)

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

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(autoload 'ace-jump-mode-pop-mark "ace-jump-mode")
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))


;; Smex
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" my-persistence-dir))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Projectile
(require 'projectile)
(setq projectile-cache-file
  (expand-file-name "projectile.cache" my-persistence-dir))
(setq projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld" my-persistence-dir))
(projectile-global-mode t)

(provide 'ui-setup)
