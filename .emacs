  (setq inhibit-startup-screen t)

  ;;;  Don't use this...  Use the version at the bottom, instead.
  ;(advice-add 'run-hooks
  ;:before (lambda (hooks) (dolist (s hooks) (message "Hook %s running" (symbol-name s)))))

  ;;; NOTE: Emacs regexps are a little funny: https://www.emacswiki.org/emacs/RegularExpression
  ;;; \s (whitespace)      ==>  \\s-
  ;;; \S (non-whitespace)  ==>  \\S-
  ;;; \d (digit)           ==>  use [[:digit:]]

  ;;; https://www.emacswiki.org/emacs/ELPA
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")
                                ;'("melpa-stable" . "https://stable.melpa.org/packages/")
                                ;'("gnu" . "https://elpa.gnu.org/packages/")  ;;; installed by default
                                ;'("org" . "http://orgmode.org/elpa/"))       ;;; deprecated for org >= 9.5
                                  )
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-and-compile
    (setq use-package-always-ensure t
          use-package-expand-minimally t))

  ;;; Fixed in Emacs 29?
  ;;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg
  (add-to-list 'image-types 'svg)

  (defun emacs-log (&rest msg)
    (interactive "s")
    (let ((save-silently t)
          (message-log-max nil))
      (append-to-file
       (format "[%s @ %s] %s\n" (format-time-string "%Y-%m-%d %H:%M:%S.%06N") (selected-frame) (apply 'format msg)) nil
       (format "%s/%s" (getenv "HOME" (selected-frame)) "Library/Logs/emacs.log"))))

  (defun debug-log (&rest msg)
    "Call `emacs-log` only if 'emacs-debug-p is set"
    (interactive "s")
    (if (and (boundp 'emacs-debug-p) emacs-debug-p) (apply 'emacs-log msg)))

  ;;; Silence `Loading...` Startup Messages
  (defun load-file (file &optional noerror nomessage)
    "Load the Lisp file named FILE."
    ;; This is a case where .elc and .so/.dll make a lot of sense.
    (interactive (list (let ((completion-ignored-extensions
                              (remove module-file-suffix
                                      (remove ".elc"
                                              completion-ignored-extensions))))
                         (read-file-name "Load file: " nil nil 'lambda))))
    (load (expand-file-name file) noerror nomessage t))

  (load-file (concat user-emacs-directory "backups.el") nil t)
  (load-file (concat user-emacs-directory "evil.el") nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("6b560fdb96dc14a69e7764256685210ac52d736b78f4d7c6f3f0d1bfe1d66289"
     "db30d380f37bd62baca0ee17d5d7c02b407b29c5f0fbaf17e2f1a834ff91fe3b"
     "800fcde54796d5849cfa8f3f2036558680b677a7642cdcfa22977b702cbf3bc8"
     "da256e0c843818c5239fec7cbb39a6bf25cfc335cd99d879300e161be0f0a156"
     "59b8cf739df8e41766e13af94c0f7c9ee6f1848fd5894b9993257a946baff85e"
     "5b88afec54a049bab2bd316b7c6467bc30657b53238771364f34b20b8db73552"
     "ced84dde85957960f03114b49e39e6607fe3d474dff82a096593099583f45781"
     "896ac2ea90e85e9f3cd70a2ac2830b3bcffaa49d3a0bb9e3435b350616424a16"
     "bdc69f8a43add813bfc1b90e9b05ea7626212b6b2392382bd2cb552e790273db"
     default))
 '(enable-recursive-minibuffers t)
 '(menu-bar-mode nil)
 '(minibuffer-depth-indicate-mode t)
 '(package-selected-packages
   '(color-theme dart-mode htmlize lsp-mode markdown-mode mmm-mode perl-ts-mode
     prettier prettier-js prettier-mode tree-sitter use-package xclip))
 '(safe-local-variable-values '((mmm-classes . sh-here-doc) (mmm-classes . here-doc)))
 '(tab-bar-close-last-tab-choice 'delete-frame)
 '(vc-follow-symlinks nil)
 '(xterm-extra-capabilities nil))

  (use-package markdown-mode
    :ensure t
    :mode ("README\\.md\\'" . gfm-mode)
    :init (setq markdown-command "multimarkdown"))

  ;;; https://www.reddit.com/r/emacs/comments/u3r59b/making_register_work_in_evil_mode_in_terminal/
  (use-package xclip :ensure t)
  (setq select-enable-clipboard nil)    ; Do not tie unnamed register "" to system clipboard
  (xclip-mode t)

  (use-package mmm-mode :ensure t)
  (setq mmm-global-mode 'maybe)
  (setq mmm-submode-decoration-level 0)
  (setq mmm-mode-ext-classes-alist nil)
  (add-to-list 'mmm-mode-ext-classes-alist '('sh-mode nil 'sh-here-doc))
  (add-to-list 'mmm-mode-ext-classes-alist '('sh-script nil 'sh-here-doc))

  (setq mmm-here-doc-mode-alist nil)
  (add-to-list 'mmm-here-doc-mode-alist '("PERL" . cperl-mode))

  (use-package htmlize :ensure t)
  (use-package lsp-mode :ensure t)
  (use-package dart-mode :ensure t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background unspecified)))))

  (define-key minibuffer-local-completion-map " " 'minibuffer-complete-and-exit)
  (define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)
  (define-key minibuffer-local-completion-map (kbd "C-u") (lambda() (interactive) (kill-line 0)))

  (put 'narrow-to-region 'disabled nil)
  (setq initial-buffer-choice t)    ;;; Always default to `*scratch*`, not previous buffer

  ;;; Disable electric-indent-mode
  (electric-indent-mode -1)
  (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
  (add-hook 'cperl-mode-hook (lambda()
                               (setq indent-line-function (lambda()))
                               (local-unset-key (kbd "{"))
                               (local-unset-key (kbd "}"))
                               (local-unset-key (kbd ";"))
                               (local-unset-key (kbd ":"))
                               ))

  ;;; Make Option+Shift+\ and Option+\ Insert Guillemets
  (define-key (current-global-map) "\e\\" #'(lambda() (interactive) (insert-char #x00AB)))
  (define-key (current-global-map) "\e|"  #'(lambda() (interactive) (insert-char #x00BB)))
  (define-key evil-read-key-map (kbd "ESC") (make-sparse-keymap))
  (define-key evil-read-key-map (kbd "\e\e") #'keyboard-quit)
  (define-key evil-read-key-map (kbd "\e\\") #'(lambda() (interactive) #x00AB))
  (define-key evil-read-key-map (kbd "\e|")  #'(lambda() (interactive) #x00BB))

  ;;; Make "find-file" et al default to `emacsclient`s PWD
  (defun find-file-read-args (prompt mustmatch)
    (message "FIND FILE READ ARGS: pwd=(%s) PWD=(%s) ==> %s" (pwd) (getenv "PWD" (selected-frame)) (file-name-as-directory (getenv "PWD" (selected-frame))))
    (list (read-file-name prompt (file-name-as-directory (getenv "PWD" (selected-frame))) (getenv "PWD" (selected-frame)) mustmatch)
          t))

  ;;; From https://jamiecollinson.com/blog/my-emacs-config/
  (setq-default indent-tabs-mode nil)

  ;;; Except...
  (define-key evil-normal-state-map  (kbd "<backtab>") 'indent-for-tab-command)
  (define-key evil-motion-state-map  (kbd "<backtab>") 'indent-for-tab-command)
  (define-key evil-insert-state-map  (kbd "<backtab>") 'indent-for-tab-command)
  (define-key evil-replace-state-map (kbd "<backtab>") 'indent-for-tab-command)

; (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (add-to-list 'auto-mode-alist `(,(concat "^" abbreviated-home-dir "\\.bash[^/]*\\'") . sh-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . javascript-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . mhtml-mode))

  ;;; use cperl-mode instead of perl-mode
  ;;; http://xahlee.info/emacs/emacs/emacs_perl_vs_cperl_mode.html
  (setq auto-mode-alist (rassq-delete-all 'perl-mode auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.\\(p\\([lm]\\)\\)\\'" . cperl-mode))

  (setq interpreter-mode-alist (rassq-delete-all 'perl-mode interpreter-mode-alist))
  (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

  ;;; Tree-Sitter-Perl "Better" ?
  (setq treesit-language-source-alist 
    '((perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl" "release"))))

(unless window-system
  (custom-set-faces
   '(default ((t (:background unspecified))))))


; apropos-match-face
; region
; secondary-selection
; tooltip

;(call-interactively #'(lambda () (interactive)
;                        (message "%s" (propertize "Some Message" 'face 'evil-ex-info)) (sit-for 2)
;                        (message "%s" (propertize "Some Message" 'face '(:foreground "#ffd7d7" :weight nil))) (sit-for 2)
;                        ))

;;; Possible values:
; face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark space-before-tab missing-newline-at-eof))
(global-whitespace-mode 1)


;(defun before-make-frame-handler () (message "BEFORE MAKE FRAME HANDLER"))
;(remove-hook 'before-make-frame-hook 'before-make-frame-handler)

;(defadvice message (after message-tail activate)
;  (with-current-buffer "*Messages*"
;    (goto-char (point-max))))

(advice-add 'keyboard-quit :around #'keyboard-quit-quiet)
(defun keyboard-quit-quiet (orig &rest args)
 ;(keyboard-escape-quit)   ;;; NO!  This calls `delete-other-windows`  :-(
  (let ((message-log-max nil))
    (condition-case foo
        (apply orig args)
      (quit (message "Quit")))))

(defun buf (x)
  (if (bufferp x) x
    (find-buffer-visiting x)))

(defun set-frame-background-mode (&optional f)
  (setq f (or f (selected-frame)))

 ;(message "SET-FRAME-BACKGROUND-MODE BEFORE: f=%s selected=%s -- %s" f (selected-frame) (frame-parameter f 'background-mode))
  (set-frame-parameter f 'background-mode (pcase (upcase (or (getenv "LC_TERM_BG_COLOR" f) ""))
                                            ("#FFFFFF" 'light)
                                            ("#000000" 'dark)
                                            (_         'dark)
                                            ))
 ;(message "SET-FRAME-BACKGROUND-MODE DURING: PWD=(%s) CUR_BG=(%s) %s" (getenv "PWD" f) (getenv "CUR_BG" f) (frame-parameter f 'background-mode))

  (when (not (getenv "PWD" f))
    ;(message "SET-FRAME-BACKGROUND-MODE: PWD unset; switching from %s to 'dark mode" (frame-parameter f 'background-mode))
    (set-frame-parameter f 'background-mode 'dark)
    (add-to-list 'default-frame-alist '(background-color . unspecified))
    (add-to-list 'default-frame-alist '(foreground-color . unspecified))

    (set-face-attribute 'default nil
                        :background 'unspecified
                        :foreground 'unspecified))
 ;(message "SET-FRAME-BACKGROUND-MODE AFTER: f=%s selected=%s -- %s" f (selected-frame) (frame-parameter f 'background-mode))
  (face-set-after-frame-default f))

(add-hook 'after-make-frame-functions 'set-frame-background-mode)

;(defun show-frame-parameters (f)
;   ;(message "AFTER MAKE FRAME HOOK -- FRAME=%s PARAMETERS: %s" f (frame-parameters f))
;   (let ((p (frame-parameters f)))
;      (dolist (word p) (message "Frame %s Parameter: %s" f word))))
;
;;(add-hook 'after-make-frame-functions (lambda(f) (message "AFTER MAKE FRAME HOOK: %s" (buffer-list (selected-frame)))))
;;(add-hook 'server-after-make-frame-hook 'cd-pwd-env)
;(add-hook 'after-make-frame-functions 'show-frame-parameters)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Kudos to bpalmer in #emacs for this/these (the former effectively expands to the latter...)  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (cl-loop for b in (buffer-list) if (with-current-buffer b (memq (frame-parameter nil 'client) server-buffer-clients)) collect b)
;
; (let (l) (dolist (b (buffer-list (selected-frame)))
;            (when (with-current-buffer b (memq (frame-parameter nil 'client) server-buffer-clients)) (setq l (cons b l)))) (nreverse l))

(defun debug-hooks (&rest hooks)
   (dolist (s hooks) (message "[%s @ %s] Hook %s running" (format-time-string "%Y-%m-%d %H:%M:%S.%06N") (selected-frame) (symbol-name s))))

;(advice-add 'run-hooks :before #'debug-hooks)
;(advice-remove 'run-hooks #'debug-hooks)
