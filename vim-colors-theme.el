;;; vim-colors-theme.el --- vim-colors theme

;; Copyright (C) 2003 by Michael Soulier
;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/emacs-jp/replace-colorthemes
;; Version: 0.01

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

(deftheme vim-colors
  "vim-colors theme")

(custom-theme-set-faces
 'vim-colors

 '(default ((t (:background unspecified :foreground unspecified))))
 '(mouse ((t (:background "#ffffff"))))
 '(cursor ((t (:background "#ffffff"))))
 '(border ((t (:background "#d7d7d7"))))

 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

 '(apropos-match-face ((t (:background "#d7d700"))))
 '(apropos-property-face ((t (:italic t :weight bold))))
 '(apropos-symbol-face  ((t (:weight bold))))
 '(cperl-here-face ((t (:foreground "#ff00ff"))))
 '(cperl-invalid-face ((t (:underline t))))
 '(cperl-pod-face ((t (:foreground "#0000ff"))))
 '(cperl-pod-head-face ((t (:foreground "#008787"))))
 '(help-highlight-face ((t (:underline t))))
 '(ispell-highlight-face ((t (:background "#afffaf"))))
 '(list-matching-lines-face ((t (:weight bold))))

 '(tags-tag-face ((t (:background "#000000" :foreground "#ffffff"))))
 '(view-highlight-face ((t (:background "#afffaf"))))
 '(widget-mouse-face ((t (:background "#afffaf"))))

 '(Info-title-4-face ((t (:weight bold))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:italic t :weight bold))))
 '(comint-highlight-input ((t (:weight bold))))
 '(comint-highlight-prompt ((t (:foreground "#00afaf"))))
 '(cperl-array-face ((t (:foreground "#00d7d7" :weight bold))))
 '(cperl-hash-face ((t (:foreground "#00d7d7" :weight bold))))
;'(cperl-nonoverridable-face ((t (:foreground "#af87af"))))
 '(cperl-nonoverridable-face ((t (:foreground "#ffd75f" :weight normal))))

 '(font-lock-builtin-face ((t (:foreground "#ffd75f"))))
 '(font-lock-comment-face ((t (:foreground "#00afaf" :background unspecified))))
 '(font-lock-constant-face ((t (:foreground "#ffffff"))))
 '(font-lock-doc-face ((t (nil))))
 '(font-lock-function-name-face ((t (:foreground "#00d7d7" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#ffd75f" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#af87af" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#ffd75f"))))
 '(font-lock-variable-name-face ((t (:foreground "#00d7d7" :weight bold))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#d70000"))))
 '(fringe ((t (nil))))
 '(header-line ((t (:background "#000000" :foreground "#d7d7d7"))))
 '(highlight ((t (:background "#afffaf"))))
 '(info-header-node ((t (nil))))
 '(info-header-xref ((t (nil))))
 '(info-menu-5 ((t (:foreground "#ff0000"))))
 '(info-menu-header ((t (:weight bold))))
 '(info-node ((t (:italic t :weight bold :foreground "#af0000"))))
 '(info-xref ((t (:weight bold :foreground "#870087"))))
 '(isearch ((t (:background "#870087" :foreground "#afd7ff"))))
 '(isearch-lazy-highlight-face ((t (:background "#afffff"))))        ;;; (:background "#878787" :foreground "#000000")  ?
 '(italic ((t (:italic t))))

 '(mode-line ((t (:background "#afafaf" :foreground "#000000"))))
 '(region ((t (:background "#ffd787"))))
 '(rpm-spec-dir-face ((t (:foreground "#00d700"))))
 '(rpm-spec-doc-face ((t (:foreground "#d700d7"))))
 '(rpm-spec-ghost-face ((t (:foreground "#d70000"))))
 '(rpm-spec-macro-face ((t (:foreground "#af00ff"))))
 '(rpm-spec-package-face ((t (:foreground "#d70000"))))
 '(rpm-spec-tag-face ((t (:foreground "#0000ff"))))
 '(scroll-bar ((t (:background "#afafaf" :foreground "#000000"))))
 '(secondary-selection ((t (:background "#d7d700"))))
 '(sh-heredoc-face ((t (:foreground "#d7af87"))))
 '(show-paren-match-face ((t (:background "#00d7d7"))))
 '(show-paren-mismatch-face ((t (:background "#af00ff" :foreground "#d7d7d7"))))
 '(tool-bar ((t (:background "#afafaf" :foreground "#000000"))))
 '(tooltip ((t (:background "#ffffd7" :foreground "#000000"))))
 '(trailing-whitespace ((t (:background "#d70000"))))
 '(underline ((t (:underline t))))
 '(sh-quoted-exec ((t (:foreground "#af87af"))))

;'(whitespace-tab ((t (:inherit leading-tab-face)))
 '(whitespace-tab ((((background dark))  :background "#000000" :foreground "#ffd7d7")
                   (((background light)) :background "#ffffff" :foreground "#5f0000")))
 '(whitespace-space-before-tab ((((background dark))  :background "#5f0000" :foreground "#ffff00")
                                (((background light)) :background "#ffd700" :foreground "#5f0000")))

 '(widget-button-face ((t (:weight bold))))
 '(widget-button-pressed-face ((t (:foreground "#d70000"))))
 '(widget-documentation-face ((t (:foreground "#000000"))))
 '(widget-field-face ((t (:background "#d7d7d7"))))
 '(widget-inactive-face ((t (:foreground "#00af87"))))
 '(widget-single-line-field-face ((t (:background "#d7d7d7"))))

 '(evil-ex-isearch ((t (:background "#d7ffd7" :foreground "#000000"))))
 '(evil-ex-lazy-highlight ((t (:background "#878787" :foreground "#000000"))))
 '(evil-ex-search ((t (:background "#d700d7" :foreground "#000000"))))
 '(evil-ex-info ((t (:foreground "#ffd7d7"))))

 '(smerge-markers ((t (:foreground "#5fd7d7"))))
 '(smerge-upper ((t (:foreground "#d75f00"))))
 '(smerge-lower ((t (:foreground "#00d75f"))))
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vim-colors)

;;; vim-colors-theme.el ends here

; Make sure the highlighting doesn't extend beyond the end of line (copy/paste bug)
; See: https://emacs.stackexchange.com/a/46813
;
; NOTE: It doesn't work for non-fontified trailing whitespace, though...
;
(add-hook 'evil-local-mode-hook (lambda() (font-lock-add-keywords nil '(("\n" . (0 nil t))) t)))

(setq frame-background-mode (pcase (upcase (or (getenv "CUR_BG" (selected-frame)) ""))
                              ("#FFFFFF" 'light)
                              ("#000000" 'dark)
                              (_         'dark)
                              ))

(if (not (getenv "PWD" (selected-frame)))
    (progn
      (setq frame-background-mode 'dark)
      (add-to-list 'default-frame-alist '(background-color . unspecified))
      (add-to-list 'default-frame-alist '(foreground-color . unspecified))

      (set-face-attribute 'default nil
                          :background 'unspecified
                          :foreground 'unspecified)))
