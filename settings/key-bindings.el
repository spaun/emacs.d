(global-set-key (kbd "C-x C-b") 'ibuffer)

;;Ace-Jump
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(if (boundp 'evil-normal-state-map)
    (define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode))

(provide 'key-bindings)
