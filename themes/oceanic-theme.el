(deftheme oceanic "my custom theme")
(custom-theme-set-faces
  'oceanic
;;  '(default ((t (:foreground "#ffffff" :background "#1B2630"))))
  '(default ((t (:foreground "#cfcfcf" :background "#2d2d2d"))))
  '(cursor ((t (:background "#1b8ac0"))))
  '(fringe ((t (:background "#1a1a1a"))))
  '(region ((t (:background "#505f77"))))
  '(font-lock-builtin-face ((t (:foreground "#ecaa4b" :weight normal))))
  '(font-lock-comment-face ((t (:foreground "#6D6D6D" :weight normal))))
  '(font-lock-function-name-face ((t (:foreground "#F2AAEC" :weight normal))))
  '(font-lock-keyword-face ((t (:foreground "#E47D80" :weight normal))))
  '(font-lock-string-face ((t (:foreground "#8AD6F2" :weight normal))))
  '(font-lock-type-face ((t (:foreground"#ecbdf4" :weight normal))))
  '(font-lock-constant-face ((t (:foreground "#78BDD6" :weight normal))))
  '(font-lock-variable-name-face ((t (:foreground "#FFFFFF" :weight normal))))
  '(minibuffer-prompt ((t (:foreground "#729fcf" :weight normal))))
  '(font-lock-warning-face ((t (:foreground "red"  :weight normal))))
  '(font-lock-type-face ((t (:foreground "#FCB666" :weight normal))))
  '(fringe ((t (:foreground "#ffffff" :background "#1B2630" :weight normal))))
  '(highlight ((t (:background "#33414D" :weight normal))))
  '(linum ((t (:foreground "#555555" :weight normal))))
  '(mode-line ((t (:background "#005b87" :foreground "#ffffff"
                               :box "#005b87" :weight normal))))
  '(mode-line-highlight ((t (:box nil))))
  '(mode-line-inactive
    ((t (:inherit mode-line :background "#555555" :foreground "white"
          :box "#555555" :weight normal))))

  ;; Parens
  '(show-paren-match ((t (:background "yellow"))))
  '(show-paren-mismatch ((t (:foreground "#F9F2CE"))))

  ;; Highlighting
  ;;'(hl-line ((t (:background ,active-color))))
  ;;'(highline-face ((t (:background ,active-color))))
  ;;'(highlight ((t (:background ,highlight-color))))
  ;;'(highlight-symbol-face ((t (:background ,secondary-color))))
  '(isearch ((t (:foreground "black" :background "yellow"))))
  '(lazy-highlight ((t (:foreground "black" :background "#ECAA4B"))))
  ;;'(primary-selection ((t (:background ,selection-color))))
  ;;'(region ((t (:background ,selection-color))))
  ;;'(secondary-selection ((t (:background ,secondary-color))))
  )
(provide-theme 'oceanic)
