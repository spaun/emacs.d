;;; init.el --- The Emacs configuration

;;; Commentary:

;;; Code:
(defconst my-base-dir "~/.emacs.d"
  "The root of Emacs configuration.")
(defconst my-settings-dir (expand-file-name "settings" my-base-dir)
  "A place to store custom configs.")
(defconst my-persistence-dir (expand-file-name "persistence" my-base-dir)
  "A root of local data files.")
(defconst my-backup-dir (expand-file-name "backups" my-persistence-dir)
  "A root of backup and autosave files.")

(dolist (dir `(,my-settings-dir ,my-persistence-dir ,my-backup-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

;; Backups - set it up early to not be affected by any errors below
(setq backup-by-copying t
      backup-directory-alist `(("." . ,my-backup-dir))
      auto-save-file-name-transforms `(("." ,my-backup-dir t))
      auto-save-list-file-prefix my-backup-dir
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(add-to-list 'load-path my-settings-dir)

(setq custom-file (expand-file-name "custom.el" my-settings-dir))
(if (file-exists-p custom-file) (load custom-file))

;; Packages

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)


(defvar my-packages
  '(
    ag
    clojure-mode
    clojure-mode-extra-font-locking
    clj-refactor
    cider
    ensime
    flx-ido
    flycheck-clojure
    haskell-mode
    hydra
    js2-mode
    ido-at-point
    ido-ubiquitous
    ido-vertical-mode
    magit
    markdown-mode
    multiple-cursors
    org
    paredit
    perspective
    persp-projectile
    php-mode
    projectile
    region-bindings-mode
    smex
    tagedit
    whitespace-cleanup-mode
    yasnippet
))

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'ui-setup)
(require 'mode-mappings)
(require 'key-bindings)

(eval-after-load 'ido '(require 'ido-setup))
(eval-after-load 'org '(require 'org-setup))
(eval-after-load 'clojure-mode '(require 'clojure-setup))
(put 'narrow-to-region 'disabled nil)

(use-package
  diminish)

(use-package
  ace-window
  :commands ace-window
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-dispatch-always nil
           aw-skope "frame")
  :bind ("C-x o" . ace-window))

(use-package
  avy
  :commands (avy-goto-char-2 avy-goto-word-1 avy-goto-line)
  :bind (("M-g c" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-g g" . avy-goto-line)))

(use-package
  flycheck
  :demand
  :diminish "FC"
  :config
  (setq
   flycheck-check-syntax-automatically '(save idle-change mode-enabled)
   flycheck-emacs-lisp-load-path (quote inherit))
  (global-flycheck-mode))

(use-package
  company
  :demand
  :diminish 'company-mode
  :config
  (global-company-mode))

(use-package
  rainbow-delimiters
  :demand
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package
  powerline
  :demand
  :config
  (powerline-default-theme)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (setq powerline-default-separator 'zigzag))

(use-package
  undo-tree
  :demand
  :diminish 'undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t))

(use-package
  web-mode
  :mode "\\.html\\.twig"
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package
  evil
  :commands evil-mode)

(use-package
  zenburn-theme
  :defer t)

(use-package
  solarized-theme
  :defer t)

(use-package
  subatomic-theme
  :defer t)

;;; init.el ends here
