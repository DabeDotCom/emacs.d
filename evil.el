  (use-package evil         :ensure t)
  (use-package evil-mc      :ensure t)
  (use-package evil-numbers :ensure t)
  (use-package vimrc-mode   :ensure t)

  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\(\\..*\\)?\\)?\\'" . vimrc-mode))

  (require 'evil)
  (evil-mode 1)

  (evil-select-search-module 'evil-search-module 'evil-search)
  (setq evil-ex-search-incremental nil)
  (setq evil-split-window-below t)
  (evil-set-undo-system 'undo-redo)
  (setq evil-search-wrap-ring-bell t)
  (setq evil-numbers-pad-default t)

  (customize-set-variable 'evil-auto-indent nil)
  (customize-set-variable 'evil-undo-system 'undo-redo)
  (customize-set-variable 'completions-format 'vertical)

  (load-theme 'vim-colors t t)
  (enable-theme 'vim-colors)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  From https://www.emacswiki.org/emacs/AlarmBell  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; (setq ring-bell-function 'ignore)    ;;; <== This is *TOO* silent

  (setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit abort-minibuffers
                        evil-ex evil-find-char previous-line next-line evil-forward-char evil-backward-char evil-ex-delete-backward-char
                       ;evil-ex-search-next evil-ex-search-previous
                       ))
          (message "DING: %s" this-command)
          (ding))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Clear the screen after suspend/resume (^Z/fg) without      ;;;
  ;;;  overwriting the last $LINES rows of the scrollback buffer  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;;; Note: This (Below) only works on the main `Emacs.app` (server) process...  :-(
  ; (add-hook 'suspend-resume-hook (lambda () (send-string-to-terminal "\e[H\e[2J")
  ;                                     (redraw-display)))

  (setq no-redraw-on-reenter t)

  (defun system-pid-running-p (pid)
    (with-temp-buffer
      (and (= 0 (call-process "/bin/sh" nil t t "-c" (format "ps -o stat= -p %s" pid)))
           (string-match "[SR]" (replace-regexp-in-string "\n\\'" "" (buffer-string))) t)))

  (defun resume-tty-redraw-frame (tty)
    (send-string-to-terminal "\e[H\e[2J" tty)
    (dolist (frame (frames-on-display-list tty))
;(emacs-log (format "RESUME-TTY FRAME %s TTY %s -- REDRAW" frame tty))
      (redraw-frame frame)))

  (add-hook 'resume-tty-functions 'resume-tty-redraw-frame)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make ":e" et al default to `emacsclient`s PWD  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun cd-pwd-env ()
    (let ((cur (getenv "PWD" (selected-frame))))
      (when cur
        (cd cur))))

  (add-hook 'evil-local-mode-hook 'cd-pwd-env)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Work with `vim-cli.sh` to parse 'evil-cli-args into 'evil-frame-buffers  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun evil-cli-args-lists ()
    "Returns a list of two lists: file buffers and commands"
    (let (bufs cmds)

      ;;; Parse Args into `l` List of File Buffers and `cmds`
      (dolist (arg (frame-parameter (selected-frame) 'evil-cli-args))
        (if (string-match "^\\+\\(.*\\)" arg)
            (setq cmds (append cmds (list (match-string 1 arg))))
          (setq bufs (append bufs (list (find-file-noselect (expand-file-name arg (getenv "PWD" (selected-frame)))))))))

      ;;; Buffers in Current Client
      (dolist (buf (buffer-list (selected-frame)))
        (when (with-current-buffer buf (memq (frame-parameter (selected-frame) 'client) server-buffer-clients))
          (setq bufs (cons buf bufs))))

      (list bufs cmds)))

  (defun set-evil-frame-buffers (bufs)
    (set-frame-parameter (selected-frame) 'evil-frame-buffers (or bufs (list (get-buffer "*scratch*")))))

  (defun evil-cli-bufs () (car  (evil-cli-args-lists)))
  (defun evil-cli-cmds () (cadr (evil-cli-args-lists)))

  (defun init-evil-cli-args ()
;(emacs-log (format "INIT EVIL-CLI-ARGS (%s)" (frame-parameter nil 'evil-cli-args)))
    (pcase-let ((`(,bufs ,cmds) (evil-cli-args-lists)))
      (set-evil-frame-buffers bufs)
      (when bufs (switch-to-buffer (car bufs)))
      (condition-case err
          (if cmds (dolist (cmd cmds)
                     (when (string-match "^\\(\\S-+\\)\\(?:\\s-+\\(.*\\)\\)?$" cmd)
                       (evil-ex-call-command nil (match-string 1 cmd) (match-string 2 cmd)))))
        (error
         (ding)
         (message "%s" (propertize (cadr err) 'face 'evil-ex-info))
         (sit-for 3)
        ))
    ))

  (add-hook 'server-after-make-frame-hook 'init-evil-cli-args)
  (add-hook 'server-after-make-frame-hook 'cd-pwd-env)

  ;;; Make `:n` /et al/ skip non-filename buffers
  (defun set-buffer-file-name-predicate ()
    (set-frame-parameter nil 'buffer-predicate (lambda(buf) (if (buffer-file-name buf) t nil))))
  (add-hook 'evil-local-mode-hook 'set-buffer-file-name-predicate)

  (defun update-evil-frame-buffers ()
    (when (not (active-minibuffer-window))   ;;; Throttle `buffer-list-update-hook`
;(emacs-log (format "UPDATE-EVIL-FRAME-BUFFERS(%s) => <%s>" (selected-frame) (frame-parameter (selected-frame) 'evil-frame-buffers)))

      ;;; Select "Live" Buffers From 'evil-frame-buffers List
      (let ((l (seq-filter 'buffer-live-p (frame-parameter (selected-frame) 'evil-frame-buffers))))

        ;;; Add Any File Buffers In This Frame That Aren't Already In 'evil-frame-buffers
;       (dolist (b (mapcar 'window-buffer (window-list (selected-frame))))
;         (with-current-buffer b
;           (when (and (not (memq b (frame-parameter (selected-frame) 'evil-frame-buffers)))
;                      (buffer-file-name b))
;             (setq l (append l (list b))))))

        ;;; Persist Changes Back To 'evil-frame-buffers Frame Parameter
        (when (not (equal (frame-parameter (selected-frame) 'evil-frame-buffers) l))
;(emacs-log (format "SETTING evil-frame-buffers %s TO %s" (selected-frame) l))
          (set-frame-parameter (selected-frame) 'evil-frame-buffers l)))))

 ;(add-hook 'buffer-list-update-hook 'update-evil-frame-buffers)
  (add-hook 'window-configuration-change-hook 'update-evil-frame-buffers)

; (defun evil-frame-buffers-select-window-advice (&rest args)
;    (emacs-log (format "EVIL-SELECT-WINDOW[%s]:  BUFFER=(%s)  FRAME=(%s)" args (window-buffer) (window-frame))))
; (advice-add 'select-window    :after #'evil-frame-buffers-select-window-advice)

  (advice-add 'switch-to-buffer :after #'evil-frame-buffers-switch-to-buffer-advice)
  (defun evil-frame-buffers-switch-to-buffer-advice (&rest args)
    (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))
      (when (and (not (buffer-file-name nil))
                 frame-buffers
                 (not (memq (window-buffer) frame-buffers)))
(progn (debug-log (format "EVIL-SWITCH-TO-BUFFER:  ADDING \"%s\" => (%s)" (window-buffer) frame-buffers))
        (set-frame-parameter nil 'evil-frame-buffers (append frame-buffers (list (window-buffer))))
)
      )))

  (advice-add 'find-file :after #'evil-frame-buffers-find-file-after)
  (defun evil-frame-buffers-find-file-after (&rest args)
    (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))
;(debug-log (format "FIND-FILE-AFTER(%s -- %s) => (%s)" args (window-buffer) frame-buffers))
      (set-frame-parameter nil 'evil-frame-buffers (append frame-buffers (list (window-buffer))))
    )
  )

  (advice-add 'bury-buffer :around #'evil-frame-buffers-bury-buffer-around-advice)
  (defun evil-frame-buffers-bury-buffer-around-advice (orig &rest args)
    (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))

      ;;; Remove Current Buffer From 'evil-frame-buffers
(debug-log (format "EVIL-BURY-BUFFER:  REMOVING \"%s\" => (%s)" (window-buffer) frame-buffers))
      (set-frame-parameter nil 'evil-frame-buffers (remove (window-buffer) frame-buffers))

      ;;; Call Original 'bury-buffer
      (apply orig args)

      ;;; If Last Buffer, Switch To "*scratch*"
      (if (not (frame-parameter nil 'evil-frame-buffers))
          (switch-to-buffer "*scratch*")
;       (if (prev-evil-frame-buffers) (evil-prev-buffer) (evil-next-buffer)))
        (if (prev-evil-frame-buffers)
            (switch-to-buffer (car (reverse (prev-evil-frame-buffers))))
          (if (next-evil-frame-buffers)
              (switch-to-buffer (car (next-evil-frame-buffers)))
            (switch-to-buffer (car frame-buffers))
          )))
    ))


  (defun prev-evil-frame-buffers (&optional window)
    "List Previous Frame Buffers in WINDOW"
    (interactive)
    (setq window (or window (selected-window)))
    (let* ((frame (window-frame window))
           (buffer (window-buffer window))
           (frame-buffers (frame-parameter frame 'evil-frame-buffers))
           l)
      (catch 'done
        (dolist (b (seq-filter 'buffer-live-p frame-buffers))
;(debug-log (format "PREV-EVIL-FRAME-BUFFERS:  SEARCHING %s FOR \"%s\" == \"%s\" <%s>" frame buffer b frame-buffers))
          (when (equal b buffer) (throw 'done l))
          (setq l (cons b l))))
      l))

  (defun next-evil-frame-buffers (&optional window)
    "List Next Frame Buffers in WINDOW"
    (interactive)
    (setq window (or window (selected-window)))
    (let* ((frame (window-frame window))
           (buffer (window-buffer window))
           (frame-buffers (frame-parameter frame 'evil-frame-buffers))
           l)
      (catch 'done
        (dolist (b (reverse (seq-filter 'buffer-live-p frame-buffers)))
;(debug-log (format "NEXT-EVIL-FRAME-BUFFERS:  SEARCHING %s FOR \"%s\" == \"%s\" <%s>" frame buffer b frame-buffers))
          (when (equal b buffer) (throw 'done l))
          (setq l (cons b l))))
      l))


;  (message "CURRENT PREFIX ARG: %s" current-prefix-arg)
;  (message "EVIL EX RANGE: %s" evil-ex-range)
;  (message "EVIL EX CMD: %s" evil-ex-cmd)
;  (message "EVIL EX COMMAND: %s" evil-ex-command)
;  (message "EVIL EX HISTORY: %s" (car evil-ex-history))
;  (message "EVIL EX EXPRESSION: %s" (car evil-ex-expression))
;  (message "EVIL EX ARGUMENT: %s" evil-ex-argument)

  (evil-define-interactive-code "<num>"
    "A number before the ':', after the ':', or after the 'command'"
    (list (cond
     (current-prefix-arg
      (prefix-numeric-value current-prefix-arg))
     (evil-ex-argument
      (string-to-number evil-ex-argument))
     (evil-ex-history
      (let ((hist (car evil-ex-history)))
        (cond
         ((string-match "^\\.,\\.\\+\\([0-9]+\\)" hist)
          (+ 1 (string-to-number (match-string 1 hist))))
         ((string-match "^\\([-+]?[0-9]+\\)" hist)
          (string-to-number (match-string 1 hist)))
         (t 1))))
     (t nil))))

  (evil-define-command nth-evil-frame-buffer (&optional count)
    "Return the COUNT'th buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (if (= count 0)
        (progn (message "%s" (propertize "E939: positive count required" 'face 'evil-ex-info)) nil)
      (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))
        (when (< count 0)
          (setq count (- (length frame-buffers) (- 0 count 1))))
        (if (< count 1)
            (progn (message "%s" (propertize "E164: Cannot go before first file" 'face 'evil-ex-info)) nil)
          (if (< (length frame-buffers) count)
              (progn (message "%s" (propertize "E165: Cannot go beyond last file" 'face 'evil-ex-info)) nil)
            (let ((b (nth (- count 1) frame-buffers)))
              (if (not (buffer-live-p b))
                  (progn (message "%s -- %s" (propertize "E165: Cannot go beyond last file" 'face 'evil-ex-info) b) nil)
                b)))))))

  (evil-define-command nth-last-evil-frame-buffer (&optional count)
    "Return the COUNT'th buffer from the end of the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (nth-evil-frame-buffer (- 0 count)))

  (defun nth-prev-evil-frame-buffer (&optional count)
    "Return the COUNT'th previous buffer in the `evil-frame-buffers` list"
    (interactive "num")
    (if (<= (length (frame-parameter nil 'evil-frame-buffers)) 1)
        (progn (message "%s" (propertize "E163: There is only one file to edit" 'face 'evil-ex-info)) nil)
      (setq count (or count 1))
      (if (< count 0)
          (evil-next-buffer (- 0 count))
        (let ((prev-buffers (prev-evil-frame-buffers)))
          (if (< (length prev-buffers) count)
              (progn (message "%s" (propertize "E164: Cannot go before first file" 'face 'evil-ex-info)) nil)
            (let ((b (nth (- count 1) prev-buffers)))
              (if (not (buffer-live-p b))
                  (progn (message "%s -- %s" (propertize "E164: Cannot go before first file" 'face 'evil-ex-info) b) nil)
                b)))))))

  (defun nth-next-evil-frame-buffer (&optional count)
    "Return the COUNT'th next buffer in the `evil-frame-buffers` list"
    (interactive "num")
    (if (<= (length (frame-parameter nil 'evil-frame-buffers)) 1)
        (progn (message "%s" (propertize "E163: There is only one file to edit" 'face 'evil-ex-info)) nil)
      (setq count (or count 1))
      (if (< count 0)
          (evil-prev-buffer (- 0 count))
        (let ((next-buffers (next-evil-frame-buffers)))
          (if (< (length next-buffers) count)
              (progn (message "%s" (propertize "E165: Cannot go beyond last file" 'face 'evil-ex-info)) nil)
            (let ((b (nth (- count 1) next-buffers)))
              (if (not (buffer-live-p b))
                  (progn (message "%s -- %s" (propertize "E165: Cannot go beyond last file" 'face 'evil-ex-info) b) nil)
                b)))))))

  (evil-define-command evil-nth-buffer (&optional count)
    "Switch to the COUNT'th buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-evil-frame-buffer count)))
      (when b
        (switch-to-buffer b))))

  (evil-define-command evil-nth-last-buffer (&optional count)
    "Switch to the COUNT'th buffer from the end of the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-last-evil-frame-buffer count)))
      (when b
        (switch-to-buffer b))))

  (evil-define-command evil-first-buffer ()
    "Switch to the first buffer in the `evil-frame-buffers` list"
    (evil-nth-buffer))

  (evil-define-command evil-last-buffer ()
    "Switch to the last buffer in the `evil-frame-buffers` list"
    (evil-nth-last-buffer))

  (evil-define-command evil-prev-buffer (&optional count)
    "Switch to the COUNT'th previous buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-prev-evil-frame-buffer count)))
      (when b
(debug-log (format "EVIL-PREV-BUFFER:  SWITCHING TO #%s: \"%s\"" count b))
        (switch-to-buffer b))))

  (evil-define-command evil-next-buffer (&optional count)
    "Switch to the COUNT'th next buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-next-evil-frame-buffer count)))
      (when b
(debug-log (format "EVIL-PREV-BUFFER:  SWITCHING TO #%s: \"%s\"" count b))
        (switch-to-buffer b))))

  (evil-define-command evil-split-prev-buffer (&optional count)
    "Switch to the COUNT'th previous buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-prev-evil-frame-buffer count)))
      (when b
        (evil-window-split)
        (switch-to-buffer b))))

  (evil-define-command evil-split-next-buffer (&optional count)
    "Switch to the COUNT'th next buffer in the `evil-frame-buffers` list"
    (interactive "<num>")
    (setq count (or count 1))
    (let ((b (nth-next-evil-frame-buffer count)))
      (when b
        (evil-window-split)
        (switch-to-buffer b))))

  (evil-define-command evil-split-all-buffers (&optional count)
    "Split frame into at most COUNT windows; one for each buffer in the `evil-frame-buffers` list"
    (interactive "P")
    (let ((bufs (evil-cli-bufs)))
      (when bufs
        (setq count (or count (when evil-ex-argument (string-to-number evil-ex-argument)) (length bufs)))
        (set-evil-frame-buffers bufs)
        (switch-to-buffer (car bufs))
        (delete-other-windows)
        (dotimes (_ (- (min count (length bufs)) 1))
          (evil-split-next-buffer))
        (evil-window-top-left)
    )))

  (evil-ex-define-cmd "fir[st]" 'evil-nth-buffer)
  (evil-ex-define-cmd "la[st]" 'evil-nth-last-buffer)
  (evil-ex-define-cmd "argu[ment]" 'evil-nth-buffer)
  (evil-ex-define-cmd "nth" 'evil-nth-buffer)
  (evil-ex-define-cmd "n[ext]" 'evil-next-buffer)
  (evil-ex-define-cmd "p[revious]" 'evil-prev-buffer)
  (evil-ex-define-cmd "sn[ext]" 'evil-split-next-buffer)
  (evil-ex-define-cmd "sp[revious]" 'evil-split-prev-buffer)
  (evil-ex-define-cmd "sall" 'evil-split-all-buffers)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  ":be" (erase-buffer) == delete-window and remove from evil-frame-buffers, but don't ":bd" globally  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-command evil-ex-erase-buffer (buffer &optional bang)
    "Remove a buffer from the current frame's 'evil-frame-buffers list."
    (interactive "<b><!>")
    (let ((buffer (or buffer (current-buffer)))
          (frame-buffers (frame-parameter nil 'evil-frame-buffers)))
      ;(when bang
      ;  (set-buffer-modified-p nil)
      ;  (dolist (process (process-list))
      ;    (when (eq (process-buffer process) buffer)
      ;      (set-process-query-on-exit-flag process nil))))

      (setq frame-buffers (remove buffer frame-buffers))
      (set-frame-parameter nil 'evil-frame-buffers frame-buffers)

      (dolist (window (window-list (selected-frame)))
        (when (eq buffer (window-buffer window))
          (if (not (= 1 (length (window-list (selected-frame)))))
              (delete-window window)
            (if (not frame-buffers)
                (delete-frame)
              (if (next-evil-frame-buffers)
                  (switch-to-buffer (car (next-evil-frame-buffers)))
                (if (prev-evil-frame-buffers)
                    (switch-to-buffer (car (reverse (prev-evil-frame-buffers))))
                  (switch-to-buffer (car frame-buffers))
                ))))))))

  (evil-ex-define-cmd "be[rase]" 'evil-ex-erase-buffer)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `^L` in "*Messages*" clear the buffer  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun clear-messages ()
    (interactive)
    (let ((size messages-buffer-max-lines))
      (setq messages-buffer-max-lines 0)
      (message "%s" "")
      (setq messages-buffer-max-lines (or size 1000))))

  (define-key evil-motion-state-map (kbd "C-l")
              (lambda()
                (interactive)
                (when (equal "*Messages*" (buffer-name (current-buffer)))
                  (clear-messages))
                (redraw-frame)
                (evil-ex-nohighlight)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `^g` show the buffer-file-name  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-key evil-motion-state-map (kbd "C-g") (lambda() (interactive) (message buffer-file-name)))
  (define-key evil-normal-state-map (kbd "C-g") (lambda() (interactive) (message buffer-file-name)))
  (define-key evil-insert-state-map (kbd "C-g") (lambda() (interactive) (message buffer-file-name)))
  (define-key evil-replace-state-map (kbd "C-g") (lambda() (interactive) (message buffer-file-name)))

  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c C-x") 'evil-numbers/dec-at-pt)

  (define-key evil-normal-state-map (kbd "C-w 0") 'delete-window)
  (define-key evil-normal-state-map (kbd "C-w 1") 'delete-other-windows)
  (define-key evil-normal-state-map (kbd "C-w 2") 'split-window-below)
  (define-key evil-normal-state-map (kbd "C-w b") 'switch-to-buffer)

  (evil-define-command evil-maximize-window-next (count)
    "Maximize the height of the COUNT'th next window"
    (interactive "<c>")
    (dotimes (_ (or count 1))
      (evil-window-next nil))
      (evil-window-set-height nil)
      (evil-window-set-width nil))

  (evil-define-command evil-maximize-window-prev (count)
    "Maximize the height of the COUNT'th next window"
    (interactive "<c>")
    (dotimes (_ (or count 1))
      (evil-window-prev nil))
      (evil-window-set-height nil)
      (evil-window-set-width nil))

  (define-key evil-normal-state-map (kbd "C-w C-i") 'evil-maximize-window-next)
  (define-key evil-normal-state-map (kbd "C-w }") 'evil-maximize-window-next)
  (define-key evil-normal-state-map (kbd "C-w {") 'evil-maximize-window-prev)
  (define-key evil-normal-state-map (kbd "C-w .") 'evil-window-set-height)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `_` a "word" character  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;;; These Don't Seem To "Stick"
  ;
  ; (modify-syntax-entry ?_ "w")    ;;; Make `_` a word character
  ; (modify-syntax-entry ?_ "w" prog-mode-syntax-table)    ;;; Make `_` a word character
  ;
  ;;; This Makes `w` Jump Symbol-By-Symbol (e.g., `with-eval-after-load` is one "Word")
  ;
  ; (with-eval-after-load 'evil
  ;   (defalias #'forward-evil-word #'forward-evil-symbol)
  ;   ;; make evil-search-word look for symbol rather than word boundaries
  ;   (setq-default evil-symbol-word-search t))

  ;;; This one seems to Do What I Mean, though...
  (add-hook 'prog-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `{` and `}` jump to blank lines  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-hook 'evil-local-mode-hook
            (lambda()
              (set-variable 'paragraph-separate "[ 	]*$")
              (set-variable 'paragraph-start    "\\|[ 	]*$")))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Don't recenter when jumping around  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq scroll-conservatively most-positive-fixnum)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  I don't want 'evil-emacs-state at *ALL*  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-key evil-motion-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-emacs-state-map (kbd "C-z") 'suspend-frame)
  (define-key evil-insert-state-map (kbd "C-z") (kbd "C-q C-z"))
  (define-key evil-replace-state-map (kbd "C-z") (kbd "C-q C-z"))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `Esc` "quit" (https://wikemacs.org/index.php/Evil#Configuration)  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Fix Emacs Movement Keys In The Completion Minibuffer  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-operator evil-backward-delete-args (beg end type register yank-handler)
    :motion evil-forward-char
    (interactive "<R><x>")
    (move-beginning-of-line nil)
    ;;; TODO: What if we're already "in" the command?
    (evil-forward-word-begin)
    (evil-backward-word-end)
    (evil-forward-char 2)

    (if (not (>= (+ 1 (point)) (save-excursion (evil-move-end-of-line) (point))))
        (evil-delete (point) beg type register yank-handler)
      (evil-delete-whole-line 2 end type register yank-handler)))

  (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
  (define-key evil-ex-completion-map "\C-b" 'evil-backward-char)
  (define-key evil-ex-completion-map "\C-d" 'evil-delete-char)
  (define-key evil-ex-completion-map "\C-f" 'evil-forward-char)
  (define-key evil-ex-completion-map "\C-k" 'evil-delete-line)
  (define-key evil-ex-completion-map "\C-u" 'evil-backward-delete-args)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Override Custom Face Colors  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;(set-face-attribute 'evil-ex-info nil :foreground "#ffd7d7")
  ;(set-face-attribute 'evil-ex-lazy-highlight nil :background "#999" :foreground "#000")


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  "Unbreak" Info Mode  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ;;; - Reset "RET" => `select-reference-this-line'
  ;;; - Reset "TAB" => `move-to-next-xref'

  (evil-define-key 'motion Info-mode-map
    (kbd "RET") (lookup-key Info-mode-map (kbd "RET"))
    (kbd "TAB") (lookup-key Info-mode-map (kbd "TAB")))


 (advice-add 'delete-window :before #'evil-frame-buffers-delete-window-before)
 (defun evil-frame-buffers-delete-window-before (&rest args)
 (debug-log (format "DELETE-WINDOW-BEFORE(%s) => (%s / %s) [%s] <%s>" args (selected-window) (get-buffer-window-list (current-buffer) nil t) (frame-first-window) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 )

 (advice-add 'delete-window :after #'evil-frame-buffers-delete-window-after)
 (defun evil-frame-buffers-delete-window-after (&rest args)
 (debug-log (format "DELETE-WINDOW-AFTER(%s) => (%s / %s) [%s] <%s>" args (selected-window) (get-buffer-window-list (current-buffer) nil t) (frame-first-window) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 )

 ;(defadvice delete-window (around delete-frame-if-one-win activate)
 ;  "If WINDOW is the only one in its frame, then `delete-frame` too."
 ;  (if (fboundp 'with-selected-window)   ; Emacs 22+
 ;(progn (emacs-log (format "NEW DELETE WINDOW(%s) => (%s)" (selected-window) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 ;      (with-selected-window
 ;          (or (ad-get-arg 0)  (selected-window))
 ;        (if (one-window-p t) (delete-frame) ad-do-it))
 ;)
 ;(emacs-log (format "OLD DELETE WINDOW(%s) => (%s)" (selected-window) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 ;    (save-current-buffer
 ;      (select-window (or (ad-get-arg 0)  (selected-window)))
 ;      (if (one-window-p t) (delete-frame) ad-do-it))))
 ;
 (advice-add 'evil-delete-buffer :before #'evil-frame-buffers-evil-delete-buffer-before)
 (defun evil-frame-buffers-evil-delete-buffer-before (&rest args)
 (debug-log (format "EVIL-DELETE-BUFFER-BEFORE(%s) => (%s / %s) <%s>" args (selected-window) (get-buffer-window-list (current-buffer) nil t) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 )

 (advice-add 'evil-delete-buffer :after #'evil-frame-buffers-evil-delete-buffer-after)
 (defun evil-frame-buffers-evil-delete-buffer-after (&rest args)
 (debug-log (format "EVIL-DELETE-BUFFER-AFTER(%s) => (%s / %s) <%s>" args (selected-window) (get-buffer-window-list (current-buffer) nil t) (frame-parameter (selected-frame) 'evil-frame-buffers)))
 )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `:bd` Remove Current Buffer From 'evil-frame-buffers  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (advice-add 'kill-buffer :before #'evil-frame-buffers-kill-buffer-before)
  (defun evil-frame-buffers-kill-buffer-before (&rest args)
    (when (not (car args))
      (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))
(debug-log (format "KILL-BUFFER-BEFORE(%s -- %s) => (%s)" args (window-buffer) frame-buffers))
(debug-log (format "KILL-BUFFER-BEFORE(NEXT) => (%s)" (next-evil-frame-buffers)))
(debug-log (format "KILL-BUFFER-BEFORE(PREV) => (%s)" (prev-evil-frame-buffers)))
        (set-frame-parameter nil 'evil-frame-buffers (remove (window-buffer) frame-buffers))
      )))

  ;;; ... and delete the frame, if it's the last one.

  (advice-add 'kill-buffer :after #'evil-frame-buffers-kill-buffer-after)
  (defun evil-frame-buffers-kill-buffer-after (&rest args)
    (when (not (car args))
      (let ((frame-buffers (frame-parameter nil 'evil-frame-buffers)))
(debug-log (format "KILL-BUFFER-AFTER(%s -- %s) => (%s)" args (window-buffer) frame-buffers))
(debug-log (format "KILL-BUFFER-AFTER(NEXT) => (%s)" (next-evil-frame-buffers)))
(debug-log (format "KILL-BUFFER-AFTER(PREV) => (%s)" (prev-evil-frame-buffers)))
        (if (not frame-buffers)
            (delete-frame)
          (if (next-evil-frame-buffers)
              (switch-to-buffer (car (next-evil-frame-buffers)))
            (if (prev-evil-frame-buffers)
                (switch-to-buffer (car (reverse (prev-evil-frame-buffers))))
              (switch-to-buffer (car frame-buffers))
            ))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Make `:q` only quit *THIS* `emacsclient` window/buffer/frame  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-command evil-quit (&optional force)
    "Close the current window, current frame, current tab, Emacs."
    :repeat nil
    (interactive "<!>")
    (condition-case err
(progn (debug-log (format "EVIL-QUIT: DELETING WINDOW %s FROM %s on %s <%s>" (selected-window) (selected-frame) (frame-parameter nil 'tty) (frame-parameter nil 'evil-frame-buffers)))
          (delete-window)
(debug-log        (format "EVIL-QUIT: DELETED  WINDOW")))

      (error
        (condition-case nil
(progn (debug-log (format "EVIL-QUIT: FINAL WINDOW %s on %s <%s> -- %s" (selected-frame) (frame-parameter nil 'tty) (frame-parameter nil 'evil-frame-buffers) err))
            (progn
              ;;; If we want a list of which other frames our buffer(s) are in...
              ; (dolist (buf (frame-parameter nil 'evil-frame-buffers))
              ;   (let (others)
              ;     (dolist (frame (frame-list))
              ;       (when (not (equal frame (selected-frame)))
              ;         (when (memq buf (frame-parameter frame 'evil-frame-buffers))
              ;           (setq others (cons frame others)))))
              ;
              ;     (dolist (frame others)
              ;       (emacs-log (format "EVIL-QUIT: WINDOW %s IN OTHER %s on %s <%s>" buf frame (frame-parameter frame 'tty) (frame-parameter frame 'evil-frame-buffers))))
              ;   ))

              ;;; If we just want to know whether or not it's elsewhere...
              (let (others)
                (dolist (frame (frame-list))
                  (when (not (equal frame (selected-frame)))
                    (dolist (buf (frame-parameter frame 'evil-frame-buffers))
                      (setq others (cons buf others)))))

(debug-log        (format "EVIL-QUIT: OTHERS (%s)" others))
                (dolist (buf (frame-parameter nil 'evil-frame-buffers))
(debug-log        (format "EVIL-QUIT: TESTING (%s) vs. (%s)" buf others))
                  (when (not (memq buf others))
(debug-log        (format "EVIL-QUIT: DELETING OTHERS (%s)" buf))
                    (kill-buffer buf))))

(debug-log        (format "EVIL-QUIT: DELETING FRAME %s" (selected-frame)))
              (delete-frame))
(debug-log        (format "EVIL-QUIT: DELETED  FRAME"))
)

          (error
           (condition-case nil
(progn (debug-log (format "EVIL-QUIT: DELETING TAB..."))
               (tab-bar-close-tab)
(debug-log        (format "EVIL-QUIT: DELETED  TAB")))
             (error
              (if force
(progn (debug-log (format "EVIL-QUIT: KILLING EMACS..."))
                  (kill-emacs)
(debug-log        (format "EVIL-QUIT: KILLED EMACS")))
                (save-buffers-kill-emacs)))))))))

  (evil-define-command evil-quit-all (&optional bang)
    "Close all evil-frame-buffers in the current frame."
    :repeat nil
    (interactive "<!>")
    (dolist (buf (frame-parameter nil 'evil-frame-buffers))
      (with-current-buffer buf (evil-quit bang))))


  ;;; From `https://github.com/DabeDotCom/evil` => `search-wrap-ring-bell--ex`
  (defun evil-ex-search (&optional count)
    "Search forward or backward COUNT times for the current ex search pattern.
The search pattern is determined by `evil-ex-search-pattern' and
the direcion is determined by `evil-ex-search-direction'."
    (setq evil-ex-search-start-point (point)
          evil-ex-last-was-search t
          count (or count 1))
    (let ((orig (point))
          wrapped)
      (dotimes (_ (or count 1))
        (when (eq evil-ex-search-direction 'forward)
          (unless (eobp) (forward-char))
          ;; maybe skip end-of-line
          (when (and (not evil-move-beyond-eol) (eolp) (not (eobp)))
            (forward-char)))
        (let ((res (evil-ex-find-next nil nil (not evil-search-wrap))))
          (cond
           ((not res)
            (goto-char orig)
            (signal 'search-failed
                    (list (evil-ex-pattern-regex evil-ex-search-pattern))))
           ((eq res 'wrapped) (setq wrapped t)))))
      (if wrapped
          (let (message-log-max)
            (when evil-search-wrap-ring-bell (ding))
            (if (eq evil-ex-search-direction 'forward)
                (message "%s" (propertize "Search hit BOTTOM, continuing at TOP" 'face 'evil-ex-info))
              (message "%s" (propertize "Search hit TOP, continuing at BOTTOM" 'face 'evil-ex-info)))))
      (goto-char (match-beginning 0))
      (setq evil-ex-search-match-beg (match-beginning 0)
            evil-ex-search-match-end (match-end 0))
      (evil-ex-search-goto-offset evil-ex-search-offset)
      (evil-ex-search-activate-highlight evil-ex-search-pattern)))

  (evil-define-motion evil-forward-char (count &optional crosslines noerror)
    "Move cursor to the right by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the end
of the line or the buffer; just return nil."
    :type exclusive
    (interactive "<c>" (list evil-cross-lines
                             (evil-kbd-macro-suppress-motion-error)))
    (cond
     ((not crosslines)
      ;; For efficiency, narrow the buffer to the projected
      ;; movement before determining the current line
      (evil-with-restriction (point) (+ (point) (or count 1) 1)
        (condition-case err
            (evil-narrow-to-line (forward-char count))
          (error
           (unless noerror (signal (car err) (cdr err))
           ;; Restore the previous command (this one never happened).
           ;; This preserves the current column if the previous command
           ;; was `evil-next-line' or `evil-previous-line'.
           (setq this-command last-command)
                   )))))
     (noerror
      (condition-case nil
          (evil-forward-char count crosslines nil)
        (error)))
     (t
      (evil-motion-loop (nil (or count 1))
        (forward-char)
        ;; don't put the cursor on a newline
        (and (not evil-move-beyond-eol)
             (not (evil-visual-state-p))
             (not (evil-operator-state-p))
             (eolp) (not (eobp)) (not (bolp))
             (forward-char))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  This was copied from `evil` commit 67115c894f4be23bec843cacf0622500bb1b38e1        ;;;
  ;;;  It also swaps the "(unless noerror (signal ...))" above "(setq this-command ...)"  ;;;
  ;;;  I think that was so "ring-bell" gets the right "evil-backward-char" command        ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-motion evil-backward-char (count &optional crosslines noerror)
    "Move cursor to the left by COUNT characters.
Movement is restricted to the current line unless CROSSLINES is non-nil.
If NOERROR is non-nil, don't signal an error upon reaching the beginning
of the line or the buffer; just return nil."
    :type exclusive
    (interactive "<c>" (list evil-cross-lines
                             (evil-kbd-macro-suppress-motion-error)))
    (cond
     ((not crosslines)
      ;; Restrict movement to the current line
      (evil-with-restriction (- (point) (or count 1)) (1+ (point))
        (condition-case err
            (evil-narrow-to-line (backward-char count))
          (error
           (unless noerror (signal (car err) (cdr err)))
           ;; Restore the previous command (this one never happened).
           ;; This preserves the current column if the previous command
           ;; was `evil-next-line' or `evil-previous-line'.
           (setq this-command last-command)
           ))))
     (noerror
      (condition-case nil
          (evil-backward-char count crosslines nil)
        (error nil)))
     (t
      (evil-motion-loop (nil (or count 1))
        (backward-char)
        ;; don't put the cursor on a newline
        (unless (or (evil-visual-state-p) (evil-operator-state-p))
          (evil-adjust-cursor))))))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  `:w` even if not-buffer-modified  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (advice-add 'evil-write :before #'evil-write-before)
  (defun evil-write-before (&rest args)
    (set-buffer-modified-p t))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;  Only `:wa` open windows in current frame  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (evil-define-command evil-write-all (bang)
    "Save all buffers in current frame visiting a file.
If BANG is non nil then read-only buffers are saved, too,
otherwise they are skipped."
    :repeat nil
    :move-point nil
    (interactive "<!>")
    (let ((cur-buffers (delete-dups (mapcar #'window-buffer (window-list nil)))))
      (if bang
          (save-some-buffers t
                           #'(lambda ()
                               (member (current-buffer) cur-buffers)))
        ;; save only buffer that are not read-only and
        ;; that are visiting a file
        (save-some-buffers t
                           #'(lambda ()
                               (and (not buffer-read-only)
                                    (buffer-file-name)
                                    (member (current-buffer) cur-buffers)))))))


;;; [WIP]  By default, `99u` followed by a single `C-r` redoes *ALL* ninety-nine steps.
;;; This is not what I want...  And I still haven't figured out a way around it, yet.
;;;
;;; The problem, methinks, is that `evil` tries to be "undo-agnostic" -- i.e., it defers
;;; to `undo-redo` or `undo-tree`, etc. -- so really, the solution must require changes
;;; to `undo-redo` (simple.el.gz) itself...

  (evil-define-command evil-undo (count)
    "Undo COUNT changes in buffer using `evil-undo-function'."
    :repeat abort
    (interactive "*p")
    (evil--check-undo-system)
    (dotimes (_ (or count 1))
        (funcall evil-undo-function 1)
        (message "evil-undo %d" _)
        (evil-start-undo-step)
        (evil-end-undo-step)
      ))

  (evil-define-command evil-redo (count)
    "Undo COUNT changes in buffer using `evil-redo-function'."
    :repeat abort
    (interactive "*p")
    (evil--check-undo-system)
    (dotimes (_ (or count 1))
        (funcall evil-redo-function 1)
        (message "evil-redo %d" _)
      ))
