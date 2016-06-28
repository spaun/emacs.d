;;; flycheck-setup.el --- Flycheck configuration
;;; Commentary:
;;; Code:

(require 'flycheck)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(global-flycheck-mode)

(provide 'flycheck-setup)

;;; flycheck-setup.el ends here
