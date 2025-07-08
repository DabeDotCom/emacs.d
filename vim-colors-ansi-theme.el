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

;;; Code:

;;; Colors:
;;
;;          Black = "#000000"
;;            Red = "#d70000"
;;          Green = "#5faf00"
;;         Yellow = "#d7af00"
;;           Blue = "#0087af"
;;        Magenta = "#d787d7"
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

(deftheme vim-colors-ansi
  "vim-colors ansi theme (\e[30-37m, etc)"
  :kind 'color-scheme
  :family 'vim-colors)

(custom-theme-set-faces
 'vim-colors-ansi

 '(default ((t (:background unspecified :foreground unspecified))))
 '(mouse ((t (:background "bright white"))))
 '(cursor ((t (:background "bright white"))))
 '(border ((t (:background "white"))))

 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

 '(apropos-match-face ((t (:background "yellow"))))
 '(apropos-property-face ((t (:italic t :weight bold))))
 '(apropos-symbol-face  ((t (:weight bold))))
 '(cperl-here-face ((t (:foreground "bright magenta"))))
 '(cperl-invalid-face ((t (:underline t))))
 '(cperl-pod-face ((t (:foreground "bright blue"))))
 '(cperl-pod-head-face ((t (:foreground "cyan"))))
 '(help-highlight-face ((t (:underline t))))
 '(ispell-highlight-face ((t (:background "bright white"))))
 '(list-matching-lines-face ((t (:weight bold))))

 '(tags-tag-face ((t (:background "black" :foreground "bright white"))))
 '(view-highlight-face ((t (:background "bright white"))))
 '(widget-mouse-face ((t (:background "bright white"))))

 '(Info-title-4-face ((t (:weight bold))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:italic t :weight bold))))
 '(comint-highlight-input ((t (:weight bold))))
 '(comint-highlight-prompt ((t (:foreground "cyan"))))
 '(cperl-array-face ((t (:foreground "cyan"))))
 '(cperl-hash-face ((t (:foreground "cyan"))))
;'(cperl-nonoverridable-face ((t (:foreground "bright black"))))
 '(cperl-nonoverridable-face ((t (:foreground "yellow" :weight normal))))

;; font-lock-mode faces
 '(font-lock-builtin-face ((t (:foreground "yellow" :weight normal))))
 '(font-lock-comment-face ((t (:foreground "cyan" :background unspecified))))
 '(font-lock-constant-face ((t (:foreground "bright white"))))
 '(font-lock-doc-face ((t (nil))))
 '(font-lock-function-name-face ((t (:foreground "bright cyan" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "yellow" :weight normal))))
 '(font-lock-string-face ((t (:foreground "magenta" :weight normal))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "bright cyan" :weight normal))))
 '(font-lock-warning-face ((t (:weight bold :foreground "red"))))
 '(perl-non-scalar-variable ((t (:inherit font-lock-variable-name-face))))

 '(fringe ((t (nil))))
 '(header-line ((t (:background "black" :foreground "white"))))
 '(highlight ((t (:background "bright green" :foreground "bright black"))))
 '(info-header-node ((t (nil))))
 '(info-header-xref ((t (nil))))
 '(info-menu-5 ((t (:foreground "bright red"))))
 '(info-menu-header ((t (:weight bold))))
 '(info-node ((t (:italic t :weight bold :foreground "red"))))
 '(info-xref ((t (:weight bold :foreground "magenta"))))
 '(isearch ((t (:background "magenta" :foreground "bright white"))))
 '(isearch-lazy-highlight-face ((t (:background "bright white"))))        ;;; (:background "bright black" :foreground "black")  ?
 '(italic ((t (:italic t))))

 '(minibuffer-prompt ((t (:foreground unspecified))))
 '(mode-line ((t (:background "white" :foreground "black"))))
 '(mode-line-inactive ((t (:background "bright black" :foreground "black"))))
 '(mode-line-buffer-id ((t (:foreground "black"))))

 '(region ((t (:background "yellow"))))
 '(rpm-spec-dir-face ((t (:foreground "green"))))
 '(rpm-spec-doc-face ((t (:foreground "magenta"))))
 '(rpm-spec-ghost-face ((t (:foreground "red"))))
 '(rpm-spec-macro-face ((t (:foreground "bright magenta"))))
 '(rpm-spec-package-face ((t (:foreground "red"))))
 '(rpm-spec-tag-face ((t (:foreground "bright blue"))))
 '(scroll-bar ((t (:background "bright black" :foreground "black"))))
 '(secondary-selection ((t (:background "yellow"))))
 '(sh-heredoc-face ((t (:foreground "white"))))
 '(show-paren-match-face ((t (:background "cyan"))))
 '(show-paren-mismatch-face ((t (:background "bright magenta" :foreground "white"))))
 '(tool-bar ((t (:background "bright black" :foreground "black"))))
 '(tooltip ((t (:background "bright white" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(underline ((t (:underline t))))
 '(sh-quoted-exec ((t (:foreground "bright blue" :weight bold))))
 '(sh-heredoc ((t (:foreground "magenta"))))

;'(whitespace-tab ((t (:inherit leading-tab-face)))
 '(whitespace-tab ((((background dark))  :background "black" :foreground "bright black" :weight bold)
                   (((background light)) :background "bright white" :foreground "bright black" :weight bold)))
 '(whitespace-space-before-tab ((((background dark))  :background "red" :foreground "bright yellow")
                                (((background light)) :background "bright yellow" :foreground "red")))

 '(button ((t (:foreground "bright blue" :weight bold :underline t))))
 '(help-face-button ((t (:foreground "bright blue" :weight bold :underline t))))
 '(widget-button-face ((t (:weight bold))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "black"))))
 '(widget-field-face ((t (:background "white"))))
 '(widget-inactive-face ((t (:foreground "cyan"))))
 '(widget-single-line-field-face ((t (:background "white"))))

 '(evil-ex-isearch ((t (:background "bright white" :foreground "black"))))
 '(evil-ex-lazy-highlight ((t (:background "bright black" :foreground "black" :weight normal))))
 '(evil-ex-search ((t (:background "magenta" :foreground "black"))))
 '(evil-ex-info ((t (:foreground "bright white"))))

 '(smerge-markers ((t (:foreground "bright cyan"))))
 '(smerge-upper ((t (:foreground "red"))))
 '(smerge-lower ((t (:foreground "green"))))

;'(lazy-highlight ((t (:background "bright cyan" :foreground "black"))))
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vim-colors-ansi)

;;; vim-colors-ansi-theme.el ends here
