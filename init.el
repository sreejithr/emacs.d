;; For backword word delete
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; Setting the default load-directory
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

;; Adding custom theme directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Setting line numbers to all files
(global-linum-mode 1)

;; Setting column number
(column-number-mode 1)

;; Offset the number by two spaces to work around some weird fringe 
(setq linum-format " %3d ")

;; Disable splash screen
(setq inhibit-startup-message t)

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

;; Set C style to linux
(setq c-default-style "linux"
          c-basic-offset 8)

;; Set the cursor to a bar
(setq default-cursor-type 'bar)

;; Disable the menubar for terminal
(menu-bar-mode -1)

(load-theme 'oceanic t)

;; GUI specific settings
;; Load the customizations after an emacsclient startsup.
(defun disable-crappy-frames (&optional frame)
  "Disables scrollbars, toolbars and fringe while in graphical mode."
  (when (or window-system frame)
    ;; Highlighting current line
    (global-hl-line-mode 1)

    (set-face-italic-p 'italic nil)

    ;; Disabling the fringe
    (set-fringe-mode '(0 . 0))

    ;; Disable the scrollbar
    (scroll-bar-mode -1)

    ;; Setting the default font
    (set-default-font "Liberation Mono 12")
    ;;(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
    ;;(set-face-attribute 'default nil :height 150)
    ;;(set-face-attribute 'default nil :font "Liberation Mono 9")

    ;; Disable the toolbar
    (tool-bar-mode -1)))

(disable-crappy-frames)
;; Disabling bold fonts
(set-face-bold-p 'bold nil)

;; Setting up Marmalade, gnu and melpa
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; Have those awesome matching pairs
(electric-pair-mode t)


(defun pycheck-on-save ()
  "Use a linter to check file on save for python buffers."
  (interactive)
  (if (eq major-mode 'python-mode)
      (python-check (concat "epylint " buffer-file-name))
    "Not a python buffer"))

;;(add-hook 'after-save-hook 'pycheck-on-save)

;; Python Rope
;; (require 'pymacs)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-shortcuts nil)
;; (setq ropemacs-local-prefix "C-c C-p")


;; Cython mode
;;(autoload 'cython-mode "cython-mode" "Loads mode for Cython files." t)
;;(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
;;(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))

;; CMake mode
;;(autoload 'cmake-mode "cmake-mode" "Loads mode for CMake files." t)
;;(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-mode))

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

(setq recentf-auto-cleanup 'never) 

(require 'git)

;; IDO mode.
(require 'ido)
(ido-mode t)
(ido-vertical-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Set such that emacs does not use the ugly word-wrapping
(global-visual-line-mode 1)

;; Keybinding to start the shell
(global-set-key (kbd "C-z") 'shell)

;; Setting keybindings for scroll line by line.
(global-set-key (kbd "C-M-g") 'scroll-up-line)
(global-set-key (kbd "C-M-y") 'scroll-down-line)

;; Open terminal in the current directory
(global-set-key (kbd "C-M-;")
                '(lambda ()
                   (interactive)
                   (shell-command
                    (format "open -a /Applications/iTerm.app --args %s"
                            default-directory))))


;; CTags settings
(setq path-to-ctags "/usr/bin/etags")
(defun create-tags (dir-name)
    "Create tags file."
    (interactive "DDirectory: ")
    (let ((full-dir-name (directory-file-name dir-name)))
      (shell-command
       (format "find %s -iname \"*.[c,h]\" | xargs %s -f %s/TAGS -R"
               full-dir-name path-to-ctags full-dir-name))))

;; cscope for emacs.
;(require 'ascope)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-safe-themes (quote ("805ae49b96802cef0cbf96218a80eae720d9abab838bc7a853d8d50f5ff83197" "525ce9f401456c02eedc8cfb39dad28ffccfd17b1e25d83cd8f0748d76225c14" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "d6e98500f46f207c1e14a6facc7d55c1ed463a221415768c086310514ddbeed7" "6229b49d2311e403f24383a69bd4d249b3d92eb64e38a62b735824d57444232b" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "9c18f9e7e62aa3572044943a89fc477c1094e699502d9bb8b8c7231c556e8d63" "3b9470f0a19817fd7a6f737a745a52faf66bc648af90bd6ef1a55e62ee2e0e33" "2fc5680862f16d65dce33536d89ef96dc820c20cfc929d1cdcc2d2eabfff8abf" "40310b1ea4b1d8d6b29624dab09a814dc5ffe61da805e54f839403ee8426748a" "ed3944f5b5174942ed528e28bec8022ec3e1f4b99ede73ceec6a75e69e87a89c" default)))
 '(ecb-options-version "2.40")
 '(elpy-default-minor-modes (quote (eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)))
 '(foreground-color "#708183")
 '(ruler-mode-current-column-char 42)
 '(ruler-mode-fill-column-char 124)
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 40) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0))))
 '(speedbar-hide-button-brackets-flag t)
 '(speedbar-use-images nil)
 '(sr-speedbar-right-side nil))

(setq gdb-many-windows t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ruler-mode-column-number ((t (:inherit ruler-mode-default :foreground "dark gray"))) t)
 '(ruler-mode-comment-column ((t (:inherit ruler-mode-default :foreground "dark gray"))) t)
 '(ruler-mode-current-column ((t (:inherit ruler-mode-default :foreground "Red" :weight bold))) t)
 '(ruler-mode-default ((t (:inherit default :foreground "grey64"))) t)
 '(speedbar-tag-face ((t (:foreground "gray80"))) t))

;; My Shortcuts
(global-set-key (kbd "C-S-f") 'speedbar-get-focus)

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; To graphically indicate the character limit in a line
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "#555555")

;; Eldoc mode for C
(setq c-eldoc-includes "`pkg-config glib-2.0 tokyocabinet --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; A few of my own customizations
;; Disable all extras of GUI.
(add-hook 'server-visit-hook 'disable-crappy-frames)
(add-hook 'after-make-frame-functions 'disable-crappy-frames)

;; Move past a given character, like vims f
(defun move-past-next-char (x)
  "Move the next occurrence of the character x"
  (interactive "k")
  (progn
    (search-forward x)))

(global-set-key "\C-\M-f" 'move-past-next-char)

;; A few key bindings that I would want to remember
;; C-x r <SPC> <char-for-register> - Mark a register
;; C-x r j <char-for-register> - Jump to a register
;; C-u C-<SPC> jump to the previous mark in the buffer

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Full screen in Linux
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth))
  (progn
    (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))  ;; no toolbar
    (menu-bar-mode -1) ;;no menubar
    (scroll-bar-mode -1) ;; no scroll bar
    ))

(global-set-key [f11] 'fullscreen)

;; For better html/javascript editing
;; (require 'web-mode)
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)i

;; Python virtual environment
(push "~/Envs/emacs/bin" exec-path)
(setenv "PATH"
        (concat
         "~/Envs/emacs/bin" ":"
         (getenv "PATH")
         ))

;; For elpy
(require 'package)
(add-to-list 'package-archives
            '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)
(elpy-use-ipython)

;; Flymake cursor - To show error message in a line
(require 'flymake-cursor)

;;
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)
;;

;; Neotree
(add-to-list 'load-path "plugins/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; For the annoying 'Mule' warnings
(define-coding-system-alias 'UTF-8 'utf-8)

;; add buffer-local indicator for whether prog-mode-hook has run.
(defun my-set-pmh-ran ()
  (set (make-local-variable 'my-pmh-ran) t))

(add-hook 'prog-mode-hook 'my-set-pmh-ran)

(add-hook 'js2-mode-hook 'my-run-pmh-if-not-ran)
(defun my-run-pmh-if-not-ran ()
  (unless (bound-and-true-p my-pmh-ran)
    (run-hooks 'prog-mode-hook)))

;; js2 mode
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 4) ; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

     ;; add from jslint global variable declarations to js2-mode globals list
     ;; modified from one in http://www.emacswiki.org/emacs/Js2Mode
     (defun my-add-jslint-declarations ()
       (when (> (buffer-size) 0)
         (let ((btext (replace-regexp-in-string
                       (rx ":" (* " ") "true") " "
                       (replace-regexp-in-string
                        (rx (+ (char "\n\t\r "))) " "
                        ;; only scans first 1000 characters
                        (save-restriction (widen) (buffer-substring-no-properties (point-min) (min (1+ 1000) (point-max)))) t t))))
           (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                 (split-string
                  (if (string-match (rx "/*" (* " ") "global" (* " ") (group (*? nonl)) (* " ") "*/") btext)
                      (match-string-no-properties 1 btext) "")
                  (rx (* " ") "," (* " ")) t))
           )))
     (add-hook 'js2-post-parse-callbacks 'my-add-jslint-declarations)))


;; simple-httpd
(add-to-list 'load-path "plugins/emacs-web-server")
(require 'simple-httpd)
(setq httpd-root "/var/www")

;; skewer mode
(add-to-list 'load-path "plugins/skewer-mode")
(require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; Twittering mode
(add-to-list 'load-path "plugins/twittering-mode")
(require 'twittering-mode)

