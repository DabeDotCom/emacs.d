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

;;; This one's easier: see if there are any overlays that start after the current point,
;;; if so, just call 'vimish-fold-next-fold like normal...
;;; Else, see if there are any BEFORE the current point and do the same from the beginning;
;;; Otherwise, if we're AT the beginning of an overlay, "say" we wrapped.  :-D
(defun vimish-fold-forward-fold ()
  "Jump to next fold, allowing for wrapping."
  (interactive)
  (cond
   ((cl-nset-difference
     (vimish-fold--folds-in (point) (point-max))
     (overlays-at (point)))
    (vimish-fold-next-fold))
   ((cl-nset-difference
     (vimish-fold--folds-in (point-min) (point))
     (overlays-at (point)))
    (goto-char (point-min))
    (vimish-fold-next-fold)
    (when evil-search-wrap-ring-bell (ding))
    (message "%s" (propertize "Search hit BOTTOM, continuing at TOP" 'face 'evil-ex-info)))
   ((overlays-at (point))
    (when evil-search-wrap-ring-bell (ding))
    (message "%s" (propertize "Search hit BOTTOM, continuing at TOP" 'face 'evil-ex-info)))
   (t
    (message "%s" (propertize "No folds found" 'face 'evil-ex-info)))))

;;; This one's slightly trickier, because if we're *IN* a fold, I want it to jump back to
;;; the beginning of that overlay...
(defun vimish-fold-backward-fold ()
  "Jump to previous fold (or beginning of current fold), allowing for wrapping."
  (interactive)
  (let* ((folds-before-point
         (cl-nset-difference
           (vimish-fold--folds-in (point-min) (point))
           (overlays-at (point))))
        (folds-after-point
         (cl-nset-difference
           (vimish-fold--folds-in (point) (point-max))
           (overlays-at (point))))
        (folds-at-point (or (overlays-at (point)) nil))
        (beg* (if folds-at-point (overlay-start (nth 0 folds-at-point))))
        (end* (if folds-at-point (overlay-end   (nth 0 folds-at-point)))))
  (cond
   ((and (overlays-at (point)) (not (eq (point) beg*)))
    (let ((beg* (overlay-start (nth 0 folds-at-point))))
      (if (eq beg* (point))
          (progn
            (when evil-search-wrap-ring-bell (ding))
            (message "%s" (propertize "Search hit TOP, continuing at BOTTOM" 'face 'evil-ex-info)))
        (goto-char beg*))))
   (folds-before-point
    (vimish-fold-previous-fold))
   ((cl-nset-difference
     (vimish-fold--folds-in (point) (point-max))
     (overlays-at (point)))
    (goto-char (point-max))
    (vimish-fold-previous-fold)
    (when evil-search-wrap-ring-bell (ding))
    (message "%s" (propertize "Search hit TOP, continuing at BOTTOM" 'face 'evil-ex-info)))
   (t
    (message "%s" (propertize "No folds found" 'face 'evil-ex-info))))))

(define-key evil-motion-state-map "zn" 'vimish-fold-forward-fold)
(define-key evil-motion-state-map "zp" 'vimish-fold-backward-fold)
(define-key evil-motion-state-map "Zn" 'vimish-fold-backward-fold)
