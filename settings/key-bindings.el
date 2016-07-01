(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Region Bindings
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map "q" 'region-bindings-mode-off)
(define-key region-bindings-mode-map "v" 'rectangle-mark-mode)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "P" 'mc/skip-to-previous-like-this)
(define-key region-bindings-mode-map (kbd "M-p") 'mc/unmark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "N" 'mc/skip-to-next-like-this)
(define-key region-bindings-mode-map (kbd "M-n") 'mc/unmark-next-like-this)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-p") 'mc/mark-pop)

(global-set-key (kbd "C-x f") 'recentf-open-files)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(provide 'key-bindings)
