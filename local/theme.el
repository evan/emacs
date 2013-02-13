(require 'color-theme)

(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")

  (let ((base03  "#171717")
        (base02  "#222222")
        (base01  "#444444")
        (base00  "#666666")
        (base0   "#888888")
        (base1   "#999999")
        (base2   "#eeeeee")
        (base3   "#ffffff")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#d30102")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0))
    (color-theme-install
     `(color-theme-solarized
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode))
       ;; basic faces
       (default ((t (:foreground ,base0))))
       (cursor ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
       (escape-glyph-face ((t (:foreground ,red))))
       (fringe ((t (:foreground ,base01 :background ,base02))))
       (header-line ((t (:foreground ,base0 :background ,base2))))
       (highlight ((t (:background ,base02))))
       (isearch ((t (:foreground ,yellow :inverse-video t))))
       (menu ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (mode-line
        ((t (:foreground ,base1 :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       ;; (mode-line-buffer-id ((t (:foreground ,base1))))
       (mode-line-inactive
        ((t (:foreground ,base01  :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       (region ((t (:background ,base01))))
       (secondary-selection ((t (:background ,base02))))
       (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
       (vertical-border ((t (:foreground ,base0))))
       ;; customize faces
       (custom-button
        ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed
        ((t (:inherit custom-button-mouse
                      :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-documentation ((t (:inherit default))))
       (custom-group-tag ((t (:foreground ,orange :bold t))))
       (custom-link ((t (:foreground ,violet))))
       (custom-variable-tag ((t (:foreground ,orange :bold t))))
       ;; diff faces
       (diff-added ((t (:foreground ,green :inverse-video t))))
       (diff-changed ((t (:foreground ,yellow :inverse-video t))))
       (diff-removed ((t (:foreground ,red :inverse-video t))))
       ;; font-lock faces
       (font-lock-builtin-face ((t (:foreground ,green))))
       (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
       (font-lock-constant-face ((t (:foreground ,cyan))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face ((t (:foreground ,green))))
       (font-lock-string-face ((t (:foreground ,cyan))))
       (font-lock-type-face ((t (:foregound ,yellow))))
       (font-lock-variable-name-face ((t (:foregound ,blue))))
       (font-lock-warning-face ((t (:foreground ,red :bold t))))

       ;; show-paren-mode
       (show-paren-match-face ((t (:background ,cyan :foreground ,base3))))
       (show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))

       ;; org-mode
       (org-hide ((t (:foreground ,base03))))
       (org-todo ((t (:foreground ,red :bold t))))
       (org-done ((t (:foreground ,green :bold t))))
       ))))

(defun color-theme-solarized-dark () (interactive) (color-theme-solarized 'dark))
(defun color-theme-solarized-light () (interactive) (color-theme-solarized 'light))

(defun $ (f &rest args) (apply f args))

(defun mf-color-theme-template (properties extra-faces)
  (let* ((prop (lambda (key default) (or (car (cdr (assoc key properties))) default)))
         (foreground ($ prop 'foreground "black"))
         (background ($ prop 'background "white"))
         (mouse ($ prop 'mouse foreground))
         (cursor ($ prop 'cursor foreground))
         (border ($ prop 'border foreground))
         (bg-mode ($ prop 'bg-mode 'light))

         ;; colors

         (red-color-list ($ prop 'red '("red3" "red4" "IndianRed1")))
         (red-fg (car red-color-list))
         (red-hi (cadr red-color-list))
         (red-bg (caddr red-color-list))

         (green-color-list ($ prop 'green '("chartreuse4" "dark green" "DarkOliveGreen1")))
         (green-fg (car green-color-list))
         (green-hi (cadr green-color-list))
         (green-bg (caddr green-color-list))

         (blue-color-list ($ prop 'blue '("blue3" "blue4" "LightSteelBlue2")))
         (blue-fg (car blue-color-list))
         (blue-hi (cadr blue-color-list))
         (blue-bg (caddr blue-color-list))

         (cyan-color-list ($ prop 'cyan '("cyan3" "cyan4" "DarkSlateGray1")))
         (cyan-fg (car cyan-color-list))
         (cyan-hi (cadr cyan-color-list))
         (cyan-bg (caddr cyan-color-list))

         (yellow-color-list ($ prop 'yellow '("goldenrod3" "goldenrod4" "LightYellow2")))
         (yellow-fg (car yellow-color-list))
         (yellow-hi (cadr yellow-color-list))
         (yellow-bg (caddr yellow-color-list))

         (magenta-color-list ($ prop 'magenta '("magenta3" "magenta4" "orchid1")))
         (magenta-fg (car magenta-color-list))
         (magenta-hi (cadr magenta-color-list))
         (magenta-bg (caddr magenta-color-list))

         (orange-color-list ($ prop 'orange '("DarkOrange3" "DarkOrange4" "tan1")))
         (orange-fg (car orange-color-list))
         (orange-hi (cadr orange-color-list))
         (orange-bg (caddr orange-color-list))

         (purple-color-list ($ prop 'purple '("purple3" "purple4" "MediumPurple1")))
         (purple-fg (car purple-color-list))
         (purple-hi (cadr purple-color-list))
         (purple-bg (caddr purple-color-list))

         (gray-color-list ($ prop 'gray '("gray45" "gray30" "gray85")))
         (gray-fg (car gray-color-list))
         (gray-hi (cadr gray-color-list))
         (gray-bg (caddr gray-color-list))
         )
    `(((foreground-color . ,foreground)
       (background-color . ,background)
       (mouse-color . ,mouse)
       (cursor-color . ,cursor)
       (border-color . ,border)
       (background-mode . ,bg-mode))
      ((Man-overstrike-face . bold)
       (Man-underline-face . underline)
       (apropos-keybinding-face . underline)
       (apropos-label-face . italic)
       (apropos-match-face . secondary-selection)
       (apropos-property-face . bold-italic)
       (apropos-symbol-face . bold)
       (goto-address-mail-face . italic)
       (goto-address-mail-mouse-face . secondary-selection)
       (goto-address-url-face . bold)
       (goto-address-url-mouse-face . highlight)
       (help-highlight-face . underline)
       (list-matching-lines-face . bold)
       (view-highlight-face . highlight))

      ,@(append extra-faces
                `((default ((t (:foreground ,foreground :background ,background :stipple nil
                                            :inverse-video nil :box nil :strike-through nil :overline nil
                                            :underline nil :slant normal :weight normal :width normal))))

                  (bold ((t (:bold t))))
                  (bold-italic ((t (:bold t :italic t))))
                  (calendar-today-face ((t (:underline t))))
                  (cperl-array-face ((t (:foreground ,blue-fg :background ,yellow-bg :bold t))))
                  (cperl-hash-face ((t (:foreground ,red-fg :background ,yellow-bg :bold t :italic t))))
                  (cperl-nonoverridable-face ((t (:foreground ,green-fg))))
                  (custom-button-face ((t (nil))))
                  (custom-changed-face ((t (:foreground ,foreground :background ,blue-fg))))
                  (custom-documentation-face ((t (nil))))
                  (custom-face-tag-face ((t (:underline t))))
                  (custom-group-tag-face ((t (:foreground ,blue-fg :underline t))))
                  (custom-group-tag-face-1 ((t (:foreground ,red-fg :underline t))))
                  (custom-invalid-face ((t (:foreground ,yellow-fg :background ,red-fg))))
                  (custom-modified-face ((t (:foreground ,foreground :background ,blue-fg))))
                  (custom-rogue-face ((t (:foreground ,magenta-bg :background ,foreground))))
                  (custom-saved-face ((t (:underline t))))
                  (custom-set-face ((t (:foreground ,blue-fg :background ,background))))
                  (custom-state-face ((t (:foreground ,green-hi))))
                  (custom-variable-button-face ((t (:bold t :underline t))))
                  (custom-variable-tag-face ((t (:foreground ,blue-fg :underline t))))
                  (diary-face ((t (:foreground ,red-fg))))
                  (ediff-current-diff-face-A ((t (:foreground ,red-hi :background ,green-bg))))
                  (ediff-current-diff-face-Ancestor ((t (:foreground ,foreground :background ,magenta-fg))))
                  (ediff-current-diff-face-B ((t (:foreground ,purple-fg :background ,yellow-fg))))
                  (ediff-current-diff-face-C ((t (:foreground ,blue-hi :background ,magenta-bg))))
                  (ediff-even-diff-face-A ((t (:foreground ,foreground :background ,gray-bg))))
                  (ediff-even-diff-face-Ancestor ((t (:foreground ,background :background ,gray-fg))))
                  (ediff-even-diff-face-B ((t (:foreground ,background :background ,gray-fg))))
                  (ediff-even-diff-face-C ((t (:foreground ,foreground :background ,gray-bg))))
                  (ediff-fine-diff-face-A ((t (:foreground ,blue-hi :background ,blue-bg))))
                  (ediff-fine-diff-face-Ancestor ((t (:foreground ,foreground :background ,green-fg))))
                  (ediff-fine-diff-face-B ((t (:foreground ,foreground :background ,cyan-fg))))
                  (ediff-fine-diff-face-C ((t (:foreground ,foreground :background ,blue-fg))))
                  (ediff-odd-diff-face-A ((t (:foreground ,background :background ,gray-fg))))
                  (ediff-odd-diff-face-Ancestor ((t (:foreground ,foreground :background ,gray-bg))))
                  (ediff-odd-diff-face-B ((t (:foreground ,foreground :background ,gray-bg))))
                  (ediff-odd-diff-face-C ((t (:foreground ,background :background ,gray-fg))))
                  (eshell-ls-archive-face ((t (:foreground ,purple-hi :bold t))))
                  (eshell-ls-backup-face ((t (:foreground ,orange-fg))))
                  (eshell-ls-clutter-face ((t (:foreground ,orange-hi :bold t))))
                  (eshell-ls-directory-face ((t (:foreground ,blue-fg :bold t))))
                  (eshell-ls-executable-face ((t (:foreground ,green-fg :bold t))))
                  (eshell-ls-missing-face ((t (:foreground ,red-fg :bold t))))
                  (eshell-ls-product-face ((t (:foreground ,orange-fg))))
                  (eshell-ls-readonly-face ((t (:foreground ,yellow-hi))))
                  (eshell-ls-special-face ((t (:foreground ,magenta-hi :bold t))))
                  (eshell-ls-symlink-face ((t (:foreground ,cyan-hi :bold t))))
                  (eshell-ls-unreadable-face ((t (:foreground ,gray-hi))))
                  (eshell-prompt-face ((t (:foreground ,red-hi :bold t))))
                  (eshell-test-failed-face ((t (:foreground ,orange-hi :bold t))))
                  (eshell-test-ok-face ((t (:foreground ,green-hi :bold t))))
                  (excerpt ((t (:italic t))))
                  (fixed ((t (:bold t))))
                  (flymake-errline ((t (:background ,red-bg))))
                  (flymake-warnline ((t (:background ,purple-bg))))
                  (flyspell-duplicate-face ((t (:foreground ,yellow-hi :bold t :underline t))))
                  (flyspell-incorrect-face ((t (:foreground ,orange-hi :bold t :underline t))))
                  (font-lock-builtin-face ((t (:foreground ,purple-fg))))
                  (font-lock-comment-face ((t (:foreground ,red-hi))))
                  (font-lock-constant-face ((t (:foreground ,blue-hi))))
                  (font-lock-function-name-face ((t (:foreground ,blue-fg))))
                  (font-lock-keyword-face ((t (:foreground ,purple-hi))))
                  (font-lock-string-face ((t (:foreground ,orange-fg))))
                  (font-lock-type-face ((t (:foreground ,green-fg))))
                  (font-lock-variable-name-face ((t (:foreground ,yellow-hi))))
                  (font-lock-warning-face ((t (:foreground ,red-fg :bold t))))
                  (fringe ((t (:foreground ,gray-bg :background ,gray-bg))))
                  (gnus-cite-attribution-face ((t (:italic t))))
                  (gnus-cite-face-1 ((t (:foreground ,blue-hi))))
                  (gnus-cite-face-10 ((t (:foreground ,purple-fg))))
                  (gnus-cite-face-11 ((t (:foreground ,cyan-fg))))
                  (gnus-cite-face-2 ((t (:foreground ,red-fg))))
                  (gnus-cite-face-3 ((t (:foreground ,green-hi))))
                  (gnus-cite-face-4 ((t (:foreground ,orange-fg))))
                  (gnus-cite-face-5 ((t (:foreground ,green-fg))))
                  (gnus-cite-face-6 ((t (:foreground ,purple-hi))))
                  (gnus-cite-face-7 ((t (:foreground ,blue-fg))))
                  (gnus-cite-face-8 ((t (:foreground ,magenta-fg))))
                  (gnus-cite-face-9 ((t (:foreground ,purple-hi))))
                  (gnus-emphasis-bold ((t (:bold t))))
                  (gnus-emphasis-bold-italic ((t (:bold t :italic t))))
                  (gnus-emphasis-italic ((t (:italic t))))
                  (gnus-emphasis-underline ((t (:underline t))))
                  (gnus-emphasis-underline-bold ((t (:bold t :underline t))))
                  (gnus-emphasis-underline-bold-italic ((t (:bold t :italic t :underline t))))
                  (gnus-emphasis-underline-italic ((t (:italic t :underline t))))
                  (gnus-group-mail-1-empty-face ((t (:foreground ,magenta-fg))))
                  (gnus-group-mail-1-face ((t (:foreground ,magenta-fg :bold t))))
                  (gnus-group-mail-2-empty-face ((t (:foreground ,magenta-bg))))
                  (gnus-group-mail-2-face ((t (:foreground ,magenta-bg :bold t))))
                  (gnus-group-mail-3-empty-face ((t (:foreground ,magenta-hi))))
                  (gnus-group-mail-3-face ((t (:foreground ,magenta-hi :bold t))))
                  (gnus-group-mail-low-empty-face ((t (:foreground ,magenta-hi))))
                  (gnus-group-mail-low-face ((t (:foreground ,magenta-hi :bold t))))
                  (gnus-group-news-1-empty-face ((t (:foreground ,green-fg))))
                  (gnus-group-news-1-face ((t (:foreground ,green-fg :bold t))))
                  (gnus-group-news-2-empty-face ((t (:foreground ,cyan-hi))))
                  (gnus-group-news-2-face ((t (:foreground ,cyan-hi :bold t))))
                  (gnus-group-news-3-empty-face ((t (nil))))
                  (gnus-group-news-3-face ((t (:bold t))))
                  (gnus-group-news-low-empty-face ((t (:foreground ,green-hi))))
                  (gnus-group-news-low-face ((t (:foreground ,green-hi :bold t))))
                  (gnus-header-content-face ((t (:foreground ,red-hi :italic t))))
                  (gnus-header-from-face ((t (:foreground ,red-fg))))
                  (gnus-header-name-face ((t (:foreground ,purple-fg))))
                  (gnus-header-newsgroups-face ((t (:foreground ,blue-hi :italic t))))
                  (gnus-header-subject-face ((t (:foreground ,red-hi))))
                  (gnus-signature-face ((t (:italic t))))
                  (gnus-splash-face ((t (:foreground ,green-fg))))
                  (gnus-summary-cancelled-face ((t (:foreground ,yellow-fg :background ,foreground))))
                  (gnus-summary-high-ancient-face ((t (:foreground ,blue-fg :bold t))))
                  (gnus-summary-high-read-face ((t (:foreground ,green-fg :bold t))))
                  (gnus-summary-high-ticked-face ((t (:foreground ,red-fg :bold t))))
                  (gnus-summary-high-unread-face ((t (:bold t))))
                  (gnus-summary-low-ancient-face ((t (:foreground ,blue-hi :italic t))))
                  (gnus-summary-low-read-face ((t (:foreground ,green-hi :italic t))))
                  (gnus-summary-low-ticked-face ((t (:foreground ,red-hi :italic t))))
                  (gnus-summary-low-unread-face ((t (:italic t))))
                  (gnus-summary-normal-ancient-face ((t (:foreground ,blue-fg))))
                  (gnus-summary-normal-read-face ((t (:foreground ,green-fg))))
                  (gnus-summary-normal-ticked-face ((t (:foreground ,red-fg))))
                  (gnus-summary-normal-unread-face ((t (nil))))
                  (gnus-summary-selected-face ((t (:underline t))))
                  (highlight ((t (:background ,green-bg))))
                  (highlight-changes-delete-face ((t (:foreground ,red-fg :underline t))))
                  (highlight-changes-face ((t (:foreground ,red-fg))))
                  (highline-face ((t (:background ,cyan-bg))))
                  (holiday-face ((t (:background ,magenta-bg))))
                  (info-menu-5 ((t (:underline t))))
                  (info-node ((t (:bold t :italic t))))
                  (info-xref ((t (:bold t))))
                  (italic ((t (:italic t))))
                  (makefile-space-face ((t (:background ,magenta-bg))))
                  (message-cited-text-face ((t (:foreground ,red-fg))))
                  (message-header-cc-face ((t (:foreground ,blue-hi))))
                  (message-header-name-face ((t (:foreground ,blue-bg))))
                  (message-header-newsgroups-face ((t (:foreground ,blue-hi :bold t :italic t))))
                  (message-header-other-face ((t (:foreground ,cyan-fg))))
                  (message-header-subject-face ((t (:foreground ,blue-fg :bold t))))
                  (message-header-to-face ((t (:foreground ,blue-hi :bold t))))
                  (message-header-xheader-face ((t (:foreground ,blue-fg))))
                  (message-separator-face ((t (:foreground ,gray-fg))))
                  (mode-line ((t (:foreground ,background :background ,foreground))))
                  (mode-line-buffer-id ((t (:foreground ,background :background ,foreground))))
                  (mode-line-mousable ((t (:foreground ,background :background ,foreground))))
                  (mode-line-mousable-minor-mode ((t (:foreground ,background :background ,foreground))))
                  (region ((t (:background ,gray-bg))))
                  (secondary-selection ((t (:background ,cyan-bg))))
                  (show-paren-match-face ((t (:background ,cyan-fg))))
                  (show-paren-mismatch-face ((t (:foreground ,background :background ,purple-fg))))
                  (speedbar-button-face ((t (:foreground ,green-hi))))
                  (speedbar-directory-face ((t (:foreground ,blue-hi))))
                  (speedbar-file-face ((t (:foreground ,cyan-hi))))
                  (speedbar-highlight-face ((t (:background ,green-fg))))
                  (speedbar-selected-face ((t (:foreground ,red-fg :underline t))))
                  (speedbar-tag-face ((t (:foreground ,gray-fg))))
                  (term-black ((t (:foreground ,foreground))))
                  (term-blackbg ((t (:background ,foreground))))
                  (term-blue ((t (:foreground ,blue-fg))))
                  (term-bluebg ((t (:background ,blue-fg))))
                  (term-bold ((t (:bold t))))
                  (term-cyan ((t (:foreground ,cyan-fg))))
                  (term-cyanbg ((t (:background ,cyan-fg))))
                  (term-default-bg ((t (nil))))
                  (term-default-bg-inv ((t (nil))))
                  (term-default-fg ((t (nil))))
                  (term-default-fg-inv ((t (nil))))
                  (term-green ((t (:foreground ,green-fg))))
                  (term-greenbg ((t (:background ,green-fg))))
                  (term-invisible ((t (nil))))
                  (term-invisible-inv ((t (nil))))
                  (term-magenta ((t (:foreground ,magenta-fg))))
                  (term-magentabg ((t (:background ,magenta-fg))))
                  (term-red ((t (:foreground ,red-fg))))
                  (term-redbg ((t (:background ,red-fg))))
                  (term-underline ((t (:underline t))))
                  (term-white ((t (:foreground ,foreground))))
                  (term-whitebg ((t (:background ,foreground))))
                  (term-yellow ((t (:foreground ,yellow-fg))))
                  (term-yellowbg ((t (:background ,yellow-fg))))
                  (underline ((t (:underline t))))
                  (vcursor ((t (:foreground ,blue-fg :background ,cyan-fg :underline t))))
                  (vhdl-font-lock-attribute-face ((t (:foreground ,purple-fg))))
                  (vhdl-font-lock-directive-face ((t (:foreground ,blue-fg))))
                  (vhdl-font-lock-enumvalue-face ((t (:foreground ,yellow-hi))))
                  (vhdl-font-lock-function-face ((t (:foreground ,purple-hi))))
                  (vhdl-font-lock-prompt-face ((t (:foreground ,red-fg :bold t))))
                  (vhdl-font-lock-reserved-words-face ((t (:foreground ,orange-fg :bold t))))
                  (vhdl-font-lock-translate-off-face ((t (:background ,gray-fg))))
                  (vhdl-speedbar-architecture-face ((t (:foreground ,blue-fg))))
                  (vhdl-speedbar-architecture-selected-face ((t (:foreground ,blue-fg :underline t))))
                  (vhdl-speedbar-configuration-face ((t (:foreground ,yellow-fg))))
                  (vhdl-speedbar-configuration-selected-face ((t (:foreground ,yellow-fg :underline t))))
                  (vhdl-speedbar-entity-face ((t (:foreground ,green-fg))))
                  (vhdl-speedbar-entity-selected-face ((t (:foreground ,green-fg :underline t))))
                  (vhdl-speedbar-instantiation-face ((t (:foreground ,orange-hi))))
                  (vhdl-speedbar-instantiation-selected-face ((t (:foreground ,orange-hi :underline t))))
                  (vhdl-speedbar-package-face ((t (:foreground ,gray-fg))))
                  (vhdl-speedbar-package-selected-face ((t (:foreground ,gray-fg :underline t))))
                  (viper-minibuffer-emacs-face ((t (:foreground ,foreground :background ,green-bg))))
                  (viper-minibuffer-insert-face ((t (:foreground ,foreground :background ,magenta-bg))))
                  (viper-minibuffer-vi-face ((t (:foreground ,green-fg :background ,gray-bg))))
                  (viper-replace-overlay-face ((t (:foreground ,foreground :background ,green-bg))))
                  (viper-search-face ((t (:foreground ,foreground :background ,yellow-bg))))
                  (widget-button-face ((t (:bold t))))
                  (widget-button-pressed-face ((t (:foreground ,red-fg))))
                  (widget-documentation-face ((t (:foreground ,green-fg))))
                  (widget-field-face ((t (:background ,gray-bg))))
                  (widget-inactive-face ((t (:foreground ,gray-fg))))
                  (widget-single-line-field-face ((t (:background ,gray-bg)))))))))


(defun color-theme-mf-dark ()
  (interactive)

  (color-theme-install
   `(color-theme-mf-dark
     ,@(mf-color-theme-template
        '((foreground "#dddddd")
          (background "#171717")
          (border "#171717")
          (cursor "#888888")
          (bg-mode 'dark)

          (red ("#D56458" "#D56458" "#572823"))
          (green ("#91E31F" "#C4E45E" "#223607"))
          (blue ("#73B7F4" "#73B7F4" "blue4"))
          (cyan ("#7DF4E0" "#7DF4E0" "cyan4"))
          (yellow ("#E4D561" "#E4D561" "DarkGoldenrod4"))
          (magenta ("VioletRed3" "DeepPink3" "HotPink4"))
          (orange ("#D58554" "#D58554" "orange4"))
          (purple ("#A75FCD" "#A75FCD" "purple4"))
          (gray ("gray65" "gray85" "gray15")))

        '((fringe ((t (:background "#171717"))))
          (vertical-border ((t (:foreground "#191919"))))
          (mode-line ((t (:foreground "white" :background "#222222" :box (:line-width 1 :color "#222222")))))
          (mode-line-buffer-id ((t (:foreground nil :background nil))))
          (mode-line-mousable ((t (:foreground nil :background nil))))
          (mode-line-mousable-minor-mode ((t (:foreground nil :background nil))))
          (mode-line-inactive ((t (:inherit mode-line :foreground "#444444" :background "#222222" :box (:line-width 1 :color "#222222")))))

          (region ((t (:foreground nil :background "#383838"))))
          (hl-line ((t (:background "#222222"))))
          (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))

          (font-lock-builtin-face ((t (:foreground "#729fcf"))))
          (font-lock-comment-face ((t (:foreground "#666666"))))
          (font-lock-variable-name-face ((t (:foreground "#E4B44A"))))
          (font-lock-keyword-face ((t (:foreground "#729fcf"))))
          (font-lock-string-face ((t (:foreground "#999999"))))
          (font-lock-type-face ((t (:foreground"#C4E45E"))))
          (font-lock-function-name-face ((t (:foreground "#D56458"))))
          (font-lock-warning-face ((t (:foreground "#ea5353" :bold t))))

          (ido-only-match ((t (:foreground "#C4E45E" :bold t))))
          (ido-subdir ((t (:foreground "#729fcf"))))

          (show-paren-match-face ((t (:background "gray22"))))
          (show-paren-mismatch-face ((t (:foreground "white" :background "red")))))))))


(defun color-theme-mf-light ()
  (interactive)

  (color-theme-install
   `(color-theme-mf-light
     ,@(mf-color-theme-template
        '((cursor "IndianRed2")

          (red ("brown4" "red" "RosyBrown1"))
          (green ("chartreuse4" "green4" "DarkOliveGreen1"))
          (blue ("RoyalBlue4" "blue4" "LightSteelBlue1"))
          (cyan ("turquoise4" "cyan4" "PaleTurquoise1"))
          (yellow ("DarkGoldenrod3" "goldenrod4" "LightGoldenrod1"))
          (magenta ("HotPink4" "DeepPink4" "pink1"))
          (orange ("DarkOrange4" "OrangeRed4" "burlywood1"))
          (purple ("MediumPurple4" "DarkOrchid4" "thistle2")))

        '((fringe ((t (:background "white"))))
          (mode-line ((t (:background "grey30" :foreground "white" :box (:line-width 1 :color "grey20")))))
          (mode-line-buffer-id ((t (:foreground nil :background nil))))
          (mode-line-mousable ((t (:foreground nil :background nil))))
          (mode-line-mousable-minor-mode ((t (:foreground nil :background nil))))
          (mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey20" :box (:line-width 1 :color "grey75") :weight light))))
          (region ((t (:background "gray85"))))
          (hl-line ((t (:background "gray95"))))
          (minibuffer-prompt ((t (:foreground "black" :bold nil))))

          (font-lock-builtin-face ((t (:foreground "DodgerBlue3"))))
          (font-lock-comment-face ((t (:foreground "tomato4"))))
          (font-lock-function-name-face ((t (:foreground "blue1"))))
          (font-lock-keyword-face ((t (:foreground "maroon4"))))
          (font-lock-string-face ((t (:foreground "sienna"))))
          (font-lock-type-face ((t (:foreground"DarkOliveGreen"))))
          (font-lock-variable-name-face ((t (:foreground "#444455"))))
          (font-lock-warning-face ((t (:foreground "#ea5353" :bold t))))

          (show-paren-match-face ((t (:background "#fff4d0"))))
          (show-paren-mismatch-face ((t (:foreground "white" :background "red")))))))))

(color-theme-mf-dark)
