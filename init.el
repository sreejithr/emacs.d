;; Setting the default load-directory
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;; Adding custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Setting line numbers to all files
(global-linum-mode 1)

;; Offset the number by two spaces to work around some weird fringe 
(setq linum-format "%3d ")

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

;; Set C style to k&r
(setq c-default-style "k&r"
          c-basic-offset 4)

;; Highlighting current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#efefef")

;; GUI specific settings
(when (window-system)
  ;; Setting the color scheme
  (load-theme 'espresso t)
  (set-face-italic-p 'italic nil)

  ;; Disabling the fringe
  (set-fringe-mode '(0 . 0))

  ;; Disable the scrollbar
  (scroll-bar-mode -1)
  
  ;; Setting the default font
  (set-face-attribute 'default nil :font "Meslo LG L-12")
  ;; Disable the toolbar
  (tool-bar-mode -1))

;; Disable the menubar
(menu-bar-mode -1)

;; Setting up Marmalade
(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Width and Height
(add-to-list 'default-frame-alist '(height . 49))
(add-to-list 'default-frame-alist '(width . 102)) 


;; On enter new line and indent
(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'c-mode 'set-newline-and-indent)


;; SLIME settings
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Python settings 
;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

; Automatically indent code after RET
(electric-indent-mode +1)

;; Python Rope
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c C-p")

;; Multi-Term settings
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
;; Without this the prompt would have weird characters like 4m
(setenv "TERMINFO" "~/.terminfo")

;; Disable the gaudy colors in shell
(setq ansi-color-names-vector		; better contrast colors
      ["black" "red4" "chartreuse4" "goldenrod3"
       "DodgerBlue4" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Tramp settings
(setq tramp-default-method "ssh")
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;; Javascript mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'git)

;; IDO mode.
(require 'ido)
(ido-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(newsticker-html-renderer nil)
 '(newsticker-url-list (quote (("Reddit Programming" "http://www.reddit.com/r/programming/.rss" nil nil nil) ("Reddit Python" "http://www.reddit.com/r/python/.rss" nil nil nil) ("Reddit C" "http://www.reddit.com/r/C_Programming/.rss" nil nil nil) ("Sutter's Mill" "http://herbsutter.com/feed/" nil nil nil)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(js2-private-function-call ((t nil)))
 '(js2-warning ((t (:underline "brown4")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Function to enable fullscreen 
;(defun toggle-fullscreen ()
;  "Toggle full screen"
;  (interactive)
;  (set-frame-parameter
;     nil 'fullscreen
;     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)


