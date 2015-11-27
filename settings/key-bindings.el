(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Ace-Jump
;; (define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
;;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
;; (if (boundp 'evil-normal-state-map)
;;     (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))


;; Region Bindings
;(require 'region-bindings-mode)
;(region-bindings-mode-enable)
;(define-key region-bindings-mode-map "q" 'region-bindings-mode-off)
;(define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
;(define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
;(define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
;(define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended)

(require 'avy)

(require 'key-chord)

(key-chord-mode 1)

(setq avy-background t)
(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "jk" 'avy-goto-char-2)

(key-chord-define-global "ma" 'mc/mark-all-like-this)
(key-chrod-define-global "pp" 'mc/mark-previous-like-this)
(key-chord-define-global "nn" 'mc/mark-next-like-this)
(key-chord-define-global "mm" 'mc/mark-more-like-this-extended)


(provide 'key-bindings)
