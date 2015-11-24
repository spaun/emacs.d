;; Config
(defvar my-base-dir "~/.emacs.d"
  "The root of emacs configuration")
(defvar my-backup-dir (expand-file-name "backups" my-base-dir)
  "A root of backup and autosave files")
(defvar my-session-dir (expand-file-name "sessions" my-base-dir)
  "A root of saved session files")
(defvar my-settings-dir (expand-file-name "settings" my-base-dir)
  "A place to store custom configs")

(dolist (dir `(,my-backup-dir ,my-session-dir ,my-settings-dir))
  (unless (file-exists-p dir)
    (make-directory dir)))

(add-to-list 'load-path my-settings-dir)

(setq custom-file (expand-file-name "custom.el" my-settings-dir))
(if (file-exists-p custom-file) (load custom-file))


;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ace-jump-mode
    clojure-mode
    clojure-mode-extra-font-locking
    clj-refactor
    cider
    diminish
    evil
    flx-ido
    flycheck
    flycheck-clojure
    haskell-mode
    js2-mode
    ido-at-point
    ido-ubiquitous
    ido-vertical-mode
    magit
    markdown-mode
    org
    paredit
    powerline
    projectile
    rainbow-delimiters
    scala-mode2
    smex
    solarized-theme
    subatomic-theme
    tagedit
    yasnippet
    zenburn-theme))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Backups
(setq backup-by-copying t
      backup-directory-alist `(("." . ,my-backup-dir))
      auto-save-file-name-transforms `(("." ,my-backup-dir t))
      auto-save-list-file-prefix my-backup-dir
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; emacs lisp
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(require 'ui-setup)
(require 'mode-mappings)
(require 'key-bindings)

(eval-after-load 'ido '(require 'ido-setup))
(eval-after-load 'org '(require 'org-setup))
(eval-after-load 'clojure-mode '(require 'clojure-setup))
(eval-after-load 'flycheck '(require 'flycheck-setup))
