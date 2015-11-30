(require 'clojure-mode)
(require 'clojure-mode-extra-font-locking)
(require 'clj-refactor)

(add-hook 'clojure-mode 'paredit-mode)
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq nrepl-hide-special-buffers t
      cider-lein-command "/home/spaun/bin/lein")

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "<f5>") 'cider-namespace-refresh)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show it's buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file (expand-file-name "cider-history" my-persistence-dir))

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

(provide 'clojure-setup)
