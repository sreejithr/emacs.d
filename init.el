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

;; To comment region
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)

;; I hate tabs!
(setq-default indent-tabs-mode nil)
(add-hook 'after-change-major-mode-hook 
          '(lambda () 
             (setq-default indent-tabs-mode nil)
             (setq c-basic-indent 4)
             (setq tab-width 4)))

;; Set C style to linux
(setq c-default-style "linux"
      c-basic-offset 4)

;; Set the cursor to a bar
(setq default-cursor-type 'bar)

;; Disable the menubar for terminal
(menu-bar-mode -1)

(load-theme 'solarized-dark t)

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
    (set-face-attribute 'default nil :height 80)
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

;; CMake mode
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "/usr/local/share/cmake/editors/emacs/cmake-mode.el" t)

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

;; IDO mode.
(add-to-list 'load-path "~/.emacs.d/plugins/ido-vertical-mode.el")
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Settings for enforcing to use UNIX endlines
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default default-buffer-file-coding-system 'utf-8-unix)

;; Set such that emacs does not use the ugly word-wrapping
(global-visual-line-mode 1)

;; Setting keybindings for scroll line by line.
(global-set-key (kbd "C-M-j") 'scroll-up-line)
(global-set-key (kbd "C-M-k") 'scroll-down-line)

;; ;; CTags settings
;; (setq path-to-ctags "/usr/bin/etags")
;; (defun create-tags (dir-name)
;;     "Create tags file."
;;     (interactive "DDirectory: ")
;;     (let ((full-dir-name (directory-file-name dir-name)))
;;       (shell-command
;;        (format "find %s -iname \"*.[c,h]\" | xargs %s -f %s/TAGS -R"
;;                full-dir-name path-to-ctags full-dir-name))))

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

;; Shortcut for compiling
(global-set-key [(f9)] 'compile)

;; To graphically indicate the character limit in a line
(add-to-list 'load-path "plugins/fill-column-indicator.el")
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-color "#555555")

;; Eldoc mode for C
;; (setq c-eldoc-includes "`pkg-config glib-2.0 tokyocabinet --cflags` -I./ -I../ ")
;; (load "c-eldoc")
;; (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

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

;; Zsh
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "WORKON_HOME" (shell-command-to-string "source ~/.zshrc; echo -n $WORKON_HOME"))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Python virtual environment
(push (concat (getenv "WORKON_HOME") "emacs/bin") exec-path)
;; (push "~/.Envs/emacs/bin" exec-path)
;; (setenv "PATH"
;;         (concat
;;          "~/.emacs.d/pyenv/emacs" ":"
;;          (getenv "PATH")
;;          ))

;; For elpy
(add-to-list 'load-path "plugins/elpy")
(require 'elpy)
(package-initialize)
(elpy-enable)
(elpy-use-ipython)

;; Flymake cursor - To show error message in a line
(require 'flymake-cursor)

;; For the annoying 'Mule' warnings
(define-coding-system-alias 'UTF-8 'utf-8)

;; Add buffer-local indicator for whether prog-mode-hook has run.
(defun my-set-pmh-ran ()
  (set (make-local-variable 'my-pmh-ran) t))

(add-hook 'prog-mode-hook 'my-set-pmh-ran)

(add-hook 'js2-mode-hook 'my-run-pmh-if-not-ran)
(defun my-run-pmh-if-not-ran ()
  (unless (bound-and-true-p my-pmh-ran)
    (run-hooks 'prog-mode-hook)))

;; skewer mode
;; (add-to-list 'load-path "plugins/skewer-mode")
;; (require 'skewer-mode)
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)

;; Go syntax highlighting
(add-to-list 'load-path "plugins/go-mode")
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

;; To beautify JSON
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "python -m json.tool" (buffer-name) t)
    )
  )

;; Nyan mode
(add-to-list 'load-path "plugins/nyan-mode")
(load "nyan-mode.el")
(nyan-mode 1)
(nyan-start-animation)

;; Git Gutter mode
(add-to-list 'load-path "plugins/git-gutter")
(load "git-gutter.el")
(require 'git-gutter)

;; If you enable global minor mode
(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
(git-gutter:linum-setup)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

;; Jump to next/previous hunk
;;(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; Stage current hunk
(global-set-key (kbd "C-x g s") 'git-gutter:stage-hunk)

;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

; pymacs
(add-to-list 'load-path "~/.emacs.d/plugins/Pymacs")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

; ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (and window-system (eq system-type 'darwin))
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (set-exec-path-from-shell-PATH))

;; Company; For completions
(add-to-list 'load-path "~/.emacs.d/plugins/company")
(add-hook 'after-init-hook 'global-company-mode)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(setq js-indent-level 2)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    ;; paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("htm" "html" "ctp" "phtml"))
(multi-web-global-mode 1)
