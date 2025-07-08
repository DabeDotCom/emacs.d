;;; vim-colors-theme.el --- vim-colors theme

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
;; Port of vim-colors theme from 'color-themes'

;;; Colors:
;;
;;          Black = "#000000"
;;            Red = "#d70000"
;;          Green = "#5faf00"
;;         Yellow = "#d7af00"
;;           Blue = "#0087af"
;;        Magenta = "#af87af"
;;           Cyan = "#00afaf"
;;          White = "#d7d7d7"
;;   Bright Black = "#878787"
;;     Bright Red = "#ff0000"
;;   Bright Green = "#87d700"
;;  Bright Yellow = "#ffd700"
;;    Bright Blue = "#87afd7"
;; Bright Magenta = "#ff00ff"
;;    Bright Cyan = "#00d7d7"
;;   Bright White = "#ffffff"

;;; Code:

(deftheme vim-colors
  "vim-colors theme"
  :kind 'color-scheme
  :family 'vim-colors)

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
 '(cperl-array-face ((t (:foreground "#00d7d7"))))
 '(cperl-hash-face ((t (:foreground "#00d7d7"))))
;'(cperl-nonoverridable-face ((t (:foreground "#d787d7"))))
 '(cperl-nonoverridable-face ((t (:foreground "#d7af00" :weight normal))))

;; font-lock-mode faces
 '(font-lock-builtin-face ((t (:foreground "#d7af00"))))
 '(font-lock-comment-face ((t (:foreground "#00afaf" :background unspecified))))
 '(font-lock-constant-face ((t (:foreground "#ffffff"))))
 '(font-lock-doc-face ((t (nil))))
 '(font-lock-function-name-face ((t (:foreground "#00d7d7" :weight bold))))
 '(font-lock-function-call-face ((t (:foreground "#00d7d7" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#d7af00" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#d787d7" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#d7af00"))))
 '(font-lock-variable-name-face ((t (:foreground "#00d7d7" :weight normal))))
 '(font-lock-warning-face ((t (:weight bold :foreground "#ff0000"))))
 '(perl-non-scalar-variable ((t (:inherit font-lock-variable-name-face))))

 '(fringe ((t (nil))))
 '(header-line ((t (:background "#000000" :foreground "#d7d7d7"))))
 '(highlight ((t (:background "#87d700" :foreground "#878787"))))
 '(info-header-node ((t (nil))))
 '(info-header-xref ((t (nil))))
 '(info-menu-5 ((t (:foreground "#ff0000"))))
 '(info-menu-header ((t (:weight bold))))
 '(info-node ((t (:italic t :weight bold :foreground "#af0000"))))
 '(info-xref ((t (:weight bold :foreground "#870087"))))
 '(isearch ((t (:background "#870087" :foreground "#afd7ff"))))
 '(isearch-lazy-highlight-face ((t (:background "#afffff"))))        ;;; (:background "#878787" :foreground "#000000")  ?
 '(italic ((t (:italic t))))

 '(minibuffer-prompt ((t (:foreground unspecified))))
 '(mode-line ((t (:background "#d7d7d7" :foreground "#000000"))))
 '(mode-line-inactive ((t (:background "#878787" :foreground "#000000"))))
 '(mode-line-buffer-id ((t (:foreground "#000000"))))

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
 '(sh-quoted-exec ((t (:foreground "#87afff" :weight bold))))
 '(sh-heredoc ((t (:foreground "#d787d7"))))

;'(whitespace-tab ((t (:inherit leading-tab-face)))
 '(whitespace-tab ((((background dark))  :background "#000000" :foreground "#878787" :weight bold)
                   (((background light)) :background "#ffffff" :foreground "#878787" :weight bold)))
 '(whitespace-space-before-tab ((((background dark))  :background "#d70000" :foreground "#ffd700")
                                (((background light)) :background "#ffd700" :foreground "#d70000")))

 '(button ((t (:foreground "#87afff" :weight bold :underline t))))
 '(help-face-button ((t (:foreground "#87afff" :weight bold :underline t))))
 '(widget-button-face ((t (:weight bold))))
 '(widget-button-pressed-face ((t (:foreground "#d70000"))))
 '(widget-documentation-face ((t (:foreground "#000000"))))
 '(widget-field-face ((t (:background "#d7d7d7"))))
 '(widget-inactive-face ((t (:foreground "#00af87"))))
 '(widget-single-line-field-face ((t (:background "#d7d7d7"))))

 '(evil-ex-isearch ((t (:background "#d7ffd7" :foreground "#000000"))))
 '(evil-ex-lazy-highlight ((t (:background "#878787" :foreground "#000000" :weight normal))))
 '(evil-ex-search ((t (:background "#d700d7" :foreground "#000000"))))
 '(evil-ex-info ((t (:foreground "#ffd7d7"))))

 '(smerge-markers ((t (:foreground "#5fd7d7"))))
 '(smerge-upper ((t (:foreground "#d75f00"))))
 '(smerge-lower ((t (:foreground "#00d75f"))))

;'(lazy-highlight ((t (:background "#cff" :foreground "#000"))))
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vim-colors)

;;; vim-colors-theme.el ends here
