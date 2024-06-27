(defvar --backup-directory (concat user-emacs-directory ".backups"))
(defvar --auto-save-directory (concat user-emacs-directory ".auto-save"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(if (not (file-exists-p --auto-save-directory))
        (make-directory --auto-save-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq auto-save-file-name-transforms   `(("\\`\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat --auto-save-directory "/" "\\2") t)))
(setq lock-file-name-transforms auto-save-file-name-transforms)

(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )
