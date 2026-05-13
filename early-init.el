;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(setenv "LSP_USE_PLISTS" "true")

(require 'xdg)

(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache" (xdg-cache-home)))

(setq
 package-user-dir
 (expand-file-name "emacs/site-lisp/elpa" (xdg-data-home))
 treesit-extra-load-path
 (list (expand-file-name "emacs/treesit" (xdg-data-home))))

(provide 'early-init)
;;; early-init.el ends here
