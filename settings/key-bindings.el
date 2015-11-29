(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Region Bindings
(require 'region-bindings-mode)
(region-bindings-mode-enable)
(define-key region-bindings-mode-map "q" 'region-bindings-mode-off)
(define-key region-bindings-mode-map "v" 'rectangle-mark-mode)
(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "M-g c") 'avy-goto-char-2)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g g") 'avy-goto-line)

(global-set-key (kbd "C-x f") 'recentf-open-files)

(provide 'key-bindings)
