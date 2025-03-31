  (use-package vimish-fold
    :ensure
    :after evil)

  (use-package evil-vimish-fold
    :ensure
    :after vimish-fold
    :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(defvar --vimish-fold-directory (concat user-emacs-directory ".vimish-fold"))
(if (not (file-exists-p --vimish-fold-directory))
        (make-directory --vimish-fold-directory t))
(setq vimish-fold-dir --vimish-fold-directory)
