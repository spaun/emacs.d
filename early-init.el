;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(setenv "LSP_USE_PLISTS" "true")

(setq
 package-user-dir (expand-file-name "var/elpa" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
