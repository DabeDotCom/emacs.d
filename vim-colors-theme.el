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
 '(border ((t (:background "white"))))

 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

 '(apropos-match-face ((t (:background "yellow"))))
 '(apropos-property-face ((t (:italic t :weight bold))))
 '(apropos-symbol-face  ((t (:weight bold))))
 '(cperl-here-face ((t (:foreground "#ff00ff"))))
 '(cperl-invalid-face ((t (:underline t))))
 '(cperl-pod-face ((t (:foreground "blue"))))
 '(cperl-pod-head-face ((t (:foreground "#008b8b"))))
 '(help-highlight-face ((t (:underline t))))
 '(ispell-highlight-face ((t (:background "darkseagreen2"))))
 '(list-matching-lines-face ((t (:weight bold))))

 '(tags-tag-face ((t (:background "#000000" :foreground "#ffffff"))))
 '(view-highlight-face ((t (:background "darkseagreen2"))))
 '(widget-mouse-face ((t (:background "darkseagreen2"))))

 '(Info-title-4-face ((t (:weight bold))))
 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:italic t :weight bold))))
 '(comint-highlight-input ((t (:weight bold))))
 '(comint-highlight-prompt ((t (:foreground "dark blue"))))
 '(cperl-array-face ((t (:foreground "#37e5e7" :weight bold))))
 '(cperl-hash-face ((t (:foreground "#37e5e7" :weight bold))))
;'(cperl-nonoverridable-face ((t (:foreground "#bc93b6"))))
 '(cperl-nonoverridable-face ((t (:foreground "#fcea60" :weight normal))))

 '(font-lock-builtin-face ((t (:foreground "#fcea60"))))
 '(font-lock-comment-face ((t (:foreground "#00a7aa" :background unspecified))))
 '(font-lock-constant-face ((t (:foreground "#ffffff"))))
 '(font-lock-doc-face ((t (nil))))
 '(font-lock-function-name-face ((t (:foreground "#37e5e7" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#fcea60" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#bc93b6" :weight normal))))
 '(font-lock-type-face ((t (:foreground "#fcea60"))))
 '(font-lock-variable-name-face ((t (:foreground "#37e5e7" :weight bold))))
 '(font-lock-warning-face ((t (:weight bold :foreground "Red"))))
 '(fringe ((t (nil))))
 '(header-line ((t (:background "grey20" :foreground "grey90"))))
 '(highlight ((t (:background "darkseagreen2"))))
 '(info-header-node ((t (nil))))
 '(info-header-xref ((t (nil))))
 '(info-menu-5 ((t (:foreground "red1"))))
 '(info-menu-header ((t (:weight bold))))
 '(info-node ((t (:italic t :weight bold :foreground "brown"))))
 '(info-xref ((t (:weight bold :foreground "magenta4"))))
 '(isearch ((t (:background "magenta4" :foreground "lightskyblue1"))))
 '(isearch-lazy-highlight-face ((t (:background "paleturquoise"))))        ;;; (:background "#999" :foreground "#000")  ?
 '(italic ((t (:italic t))))

 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(region ((t (:background "lightgoldenrod2"))))
 '(rpm-spec-dir-face ((t (:foreground "green"))))
 '(rpm-spec-doc-face ((t (:foreground "magenta"))))
 '(rpm-spec-ghost-face ((t (:foreground "red"))))
 '(rpm-spec-macro-face ((t (:foreground "purple"))))
 '(rpm-spec-package-face ((t (:foreground "red"))))
 '(rpm-spec-tag-face ((t (:foreground "blue"))))
 '(scroll-bar ((t (:background "grey75" :foreground "#000000"))))
 '(secondary-selection ((t (:background "yellow"))))
 '(sh-heredoc-face ((t (:foreground "tan"))))
 '(show-paren-match-face ((t (:background "turquoise"))))
 '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
 '(tool-bar ((t (:background "grey75" :foreground "black"))))
 '(tooltip ((t (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red"))))
 '(underline ((t (:underline t))))
 '(sh-quoted-exec ((t (:foreground "#bc93b6"))))

;'(whitespace-tab ((t (:inherit leading-tab-face)))
 '(whitespace-tab ((((background dark))  :background "#330000" :foreground "#FFCCCC")
                   (((background light)) :background "#FFF0F0" :foreground "#663333")))
 '(whitespace-space-before-tab ((((background dark))  :background "#660000" :foreground "#FFFF00")
                                (((background light)) :background "#FFCC00" :foreground "#663300")))

 '(widget-button-face ((t (:weight bold))))
 '(widget-button-pressed-face ((t (:foreground "red"))))
 '(widget-documentation-face ((t (:foreground "dark green"))))
 '(widget-field-face ((t (:background "gray85"))))
 '(widget-inactive-face ((t (:foreground "dim gray"))))
 '(widget-single-line-field-face ((t (:background "gray85"))))

 '(evil-ex-isearch ((t (:background "#cfc" :foreground "#000"))))
 '(evil-ex-lazy-highlight ((t (:background "#999" :foreground "#000"))))
 '(evil-ex-search ((t (:background "#c0c" :foreground "#000"))))
 '(evil-ex-info ((t (:foreground "#ffd7d7"))))

 '(smerge-markers ((t (:foreground "#6CC"))))
 '(smerge-upper ((t (:foreground "#C60"))))
 '(smerge-lower ((t (:foreground "#0C6"))))
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
      (add-to-list 'default-frame-alist '(background-color . "black"))
      (add-to-list 'default-frame-alist '(foreground-color . "white"))

      (set-face-attribute 'default nil
                          :background "black"
                          :foreground "white")))
