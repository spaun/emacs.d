(require 'ido)
(ido-mode t)

(setq ido-save-directory-list-file (expand-file-name "ido-last" my-persistence-dir)
      ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")

(require 'ido-at-point)
(ido-at-point-mode)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(provide 'ido-setup)
