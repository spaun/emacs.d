(require 'org-crypt)

(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt"))
      org-crypt-key nil
      auto-save-default nil
      org-src-fontify-natively t
      org-return-follows-link t)
(global-set-key (kbd "C-c d") 'org-decrypt-entry)

(provide 'org-setup)
