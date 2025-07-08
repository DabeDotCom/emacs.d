;;; vim-colors.el --- Load vim-colors theme(s)

;;; Based on:
;;
;; - Copyright (C) 2003 by Michael Soulier
;; - Copyright (C) 2013 by Syohei YOSHIDA

;; - Author: Syohei YOSHIDA <syohex@gmail.com>
;; - URL: https://github.com/emacs-jp/replace-colorthemes
;; - Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Port of vim-colors theme from `color-themes'

;;; Code:

;; `enable-theme` and `theme-choose-variant` seem to be global across all frames... «sigh»

(load-theme 'vim-colors      t t)
(load-theme 'vim-colors-ansi t t)
(enable-theme 'vim-colors)

;;; vim-colors.el ends here

; Make sure the highlighting doesn't extend beyond the end of line (copy/paste bug)
; See: https://emacs.stackexchange.com/a/46813
;
; NOTE: It doesn't work for non-fontified trailing whitespace, though...
;
(add-hook 'evil-local-mode-hook (lambda() (font-lock-add-keywords nil '(("\n" . (0 nil t))) t)))

;;; This used to be required because hooks receive no parameters, but
;;; 'set-frame-background-mode needed one; now the latter is &optional
;;; and uses `(setq f (or f (selected-frame)))` to provide a default.
;
;(defun set-evil-frame-background-mode()
;  (message "EVIL-LOCAL-MODE-HOOK: Setting Frame Background Mode (%s)" (selected-frame))
;  (set-frame-background-mode (selected-frame)))

(add-hook 'evil-local-mode-hook 'set-frame-background-mode)
