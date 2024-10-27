;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:

;;; Code:
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "var/eln-cache/" user-emacs-directory)))

(setenv "LSP_USE_PLISTS" "true")

(setq
 package-user-dir (expand-file-name "var/elpa" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
