;; Add .emacs.d/lisp to load-path
(setq dotfiles-lisp-dir
      (file-name-as-directory
       (concat (file-name-directory
                (or (buffer-file-name) load-file-name))
               "lisp")))
(add-to-list 'load-path dotfiles-lisp-dir)


;; Meta Key CMD
(setq ns-command-modifier (quote meta))

;; don't use tabs for indent
(setq-default indent-tabs-mode nil)

(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;; emacs package management
;; use MELPA stable
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(setq package-pinned-packages
      '((imenu-anywhere . "melpa-stable")
        (spaceline . "melpa-stable")
        ;; (clj-refactor . "melpa-stable")
        ;;        (cider . "melpa-stable")
        ;;(flycheck-clojure . "melpa-stable")
        (flycheck-pos-tip . "melpa-stable")
        (clojure-mode . "melpa-stable")
        (linum-relative . "melpa-stable")
        (aggressive-indent . "melpa-stable")
        (evil-leader . "melpa-stable")
        (evil-visualstart . "melpa-stable")
        (evil-jumper . "melpa-stable")
        (evil-snipe . "melpa-stable")
        (evil . "melpa-stable")
        (evil-commentary . "melpa-stable")))
(package-initialize)


(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(setq ring-bell-function 'ignore)

(unless package-archive-contents
  (package-refresh-contents))

(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; delete the selection with a keypress
(delete-selection-mode t)

;; highlight the current line
(global-hl-line-mode +1)

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(setq package-enable-at-startup nil) ; Don't initialize later as well



(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(setq use-package-verbose t)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(cider-lein-parameters "with-profile +test repl :headless")
 '(cider-repl-display-help-banner nil)
 '(cider-repl-use-pretty-printing t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("138d69908243e827e869330c43e7abc0f70f334dfa90a589e4d8a1f98a1e29af" default)))
 '(fci-rule-color "#383838")
 '(flycheck-disabled-checkers
   (quote
    (emacs-lisp-checkdoc ruby-rubylint clojure-cider-typed clojure-cider-eastwood clojure-cider-kibit)))
 '(helm-adaptive-history-file "~/.emacs.d/helm-history")
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-transformer-show-only-basename nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(helm-split-window-inside-p t)
 '(imenu-auto-rescan t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (cljr-helm clj-refactor feature-mode helm-git flycheck-haskell smart-parens magithub zenburn-theme yaml-mode which-key web-mode volatile-highlights use-package undo-tree toml-mode tagedit swiper-helm sql-indent spacemacs-theme smex smart-mode-line shell-pop scala-mode rust-mode rainbow-mode rainbow-delimiters pytest pt popwin neotree move-text markdown-mode magit-annex kibit-helper json-mode js2-mode jedi inf-ruby imenu-anywhere ido-ubiquitous hl-todo hl-sexp highlight-symbol highlight-parentheses helm-swoop helm-projectile helm-flycheck helm-dired-recent-dirs helm-dired-history helm-descbinds helm-company helm-clojuredocs helm-ag hardcore-mode go-mode github-browse-file git-timemachine git-rebase-mode git-commit-mode flyspell-lazy flycheck-tip flycheck-pos-tip flycheck-joker flycheck-cython flycheck-color-mode-line flycheck-clojure flycheck-cask fic-mode expand-region exec-path-from-shell enh-ruby-mode emoji-cheat-sheet-plus elisp-slime-nav easy-kill discover diff-hl cpputils-cmake counsel company-web company-jedi cmake-mode clojure-mode-extra-font-locking clojure-cheatsheet cljsbuild-mode cider-eval-sexp-fu cask-mode cask browse-at-remote better-defaults avy anzu aggressive-indent)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(projectile-completion-system (quote helm))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" "Godeps")))
 '(safe-local-variable-values
   (quote
    ((scss-mode
      (css-indent-offset . 2))
     (eval cider-register-cljs-repl-type
           (quote figwheel+integrant)
           "(do (require 'figwheel-sidecar.repl-api)
              (require 'integrant.repl)
              (integrant.repl/go)
              (figwheel-sidecar.repl-api/cljs-repl))")
     (eval cider-register-cljs-repl-type
           (quote figwheel+integrant)
           "(do (require 'figwheel-sidecar.repl-api)
               (require 'integrant.repl)
               (integrant.repl/go)
               (figwheel-sidecar.repl-api/cljs-repl))")
     (cider-default-cljs-repl . figwheel+integrant))))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))


(use-package imenu-anywhere
  :ensure t
  :config
  (global-set-key (kbd "C-.") #'imenu-anywhere))

;;Cucumber
(use-package feature-mode
  :ensure t
  :config
  (setq feature-step-search-path "features/**/*steps.clj")
  (setq feature-step-search-gems-path "gems/ruby/*/gems/*/**/*steps.rb"))

(use-package ielm
  :ensure t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))


(use-package pt
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))


(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))



(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package cask
  :ensure t)

(use-package cask-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-jedi
  :ensure t)

(use-package ivy
  :ensure t
  :config
    (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package better-defaults
  :ensure t)

(use-package browse-at-remote
  :ensure t)

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))


(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; Clj-refactor causes problems as cider brings it in
(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  ;; Configure the Clojure Refactoring prefix:
  (cljr-add-keybindings-with-prefix "C-c .")
  :diminish clj-refactor-mode)

(use-package cljsbuild-mode
  :ensure t)

;; Reference https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package flycheck
  :ensure t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (haskell-mode-hook    . haskell-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)

  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
  This lets us fix any errors as quickly as possible, but in a
  clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)

  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled))

  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change.
  This is an overwritten version of the original
  flycheck-handle-idle-change, which removes the forced deferred.
  Timers should only trigger inbetween commands in a single
  threaded system and the forced deferred makes errors never show
  up before you execute another command."
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flycheck-joker
  :ensure t)


(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package flycheck-cask
  :ensure t)

(use-package flyspell-lazy
  :ensure t)

(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package flycheck-tip
  :ensure t)

;; This disables the arrow keys
;; use-package hardcore-mode
(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))

(use-package helm
  :ensure t
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)

  ;; These do not sem to take effect
  (global-set-key (kbd "C-c M-x")     'execute-extended-command) ; old M-x
  (global-set-key (kbd "C-x C-d")     'helm-browse-project)
  (global-set-key (kbd "C-h C-f")     'helm-apropos)
  (global-set-key (kbd "C-h r")       'helm-info-emacs)
  (global-set-key (kbd "C-h i")       'helm-info-at-point)
  (global-set-key (kbd "C-:")         'helm-eval-expression-with-eldoc)
  (global-set-key (kbd "C-,")         'helm-calcul-expression)
  (global-set-key (kbd "C-x C-b")     'helm-buffers-list)
  (global-set-key (kbd "C-c f")       'helm-recentf)
  ;; (global-set-key (kbd "C-x C-f")     'helm-find-files)
  (global-set-key (kbd "M-x")         'helm-M-x)
  (global-set-key (kbd "M-y")         'helm-show-kill-ring)
  (global-set-key (kbd "C-c i")       'helm-imenu)
  (global-set-key (kbd "C-x b")       'helm-mini)
  ;;  (global-set-key (kbd "C-x C-f")     'helm-find-files)
  (global-set-key (kbd "C-c h o")     'helm-occur)

  (define-key global-map [remap jump-to-register]      'helm-register)
  (define-key global-map [remap list-buffers]          'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
  (define-key global-map [remap find-tag]              'helm-etags-select)
  (define-key global-map [remap xref-find-definitions] 'helm-etags-select)

  (helm-adaptive-mode t)
  (helm-mode 1))

;; causes problems cljr helm

(use-package helm-ag
  :ensure t)

(use-package helm-clojuredocs
  :ensure t)

(use-package helm-company
  :ensure t)

(use-package helm-dired-history
  :ensure t)

(use-package helm-dired-recent-dirs
  :ensure t)

(use-package helm-flycheck
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package helm-flycheck
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'helm-swoop))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :bind ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :bind ("M-t" . helm-projectile-find-file)
  :config
  (helm-projectile-on)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package magit
  :ensure t)

(use-package magit-annex
  :ensure t)

(use-package magithub
  :ensure t)

(use-package json-mode
  :ensure t)
(use-package ag
  :ensure t)

(use-package kibit-helper
  :ensure t)

(use-package cider-eval-sexp-fu
  :ensure t)

(use-package crosshairs
  :bind ("M-o c" . crosshairs-mode))

(use-package crux
  :bind ("C-c e i" . crux-find-user-init-file))

(use-package css-mode
  :mode "\\.css\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))


(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
  :commands diff-hl-flydiff-mode)

(use-package diff-mode
  :commands diff-mode)

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  (global-set-key (kbd "C-z") 'undo)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo)
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package swiper-helm
  :ensure t)

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel-dash
  :bind ("C-c C-h" . counsel-dash))

(use-package counsel-gtags
  ;; jww (2017-12-10): Need to configure.
  :disabled t
  :after counsel)

(use-package counsel-osx-app
  :bind* ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app
  :config
  (setq counsel-osx-app-location
        (list "/Applications"
              "/Applications/Misc"
              "/Applications/Utilities"
              (expand-file-name "~/Applications")
              (expand-file-name "~/.nix-profile/Applications")
              "/Applications/Xcode.app/Contents/Applications")))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map [remap projectile-ag]
    #'counsel-projectile-rg))

(use-package counsel-tramp
  :commands counsel-tramp)

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; Side project viewer
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))
;; Javascript

;;
(use-package highlight-symbol
  :ensure t
  :config
  (highlight-symbol-mode t)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package auto-yasnippet
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package yasnippet
  :demand t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (yas-load-directory (emacs-path "snippets"))
  (yas-global-mode 1))





;; Yaml mode support
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

;; Ruby
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'subword-mode))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (setq company-tooltip-align-annotations t))

(use-package sql-indent
  :ensure t)

(use-package github-browse-file
  :ensure t)

;; Python
(use-package jedi
  :ensure t)

(use-package pytest
  :ensure t)

(use-package company-jedi
  :ensure t)

(use-package flycheck-haskell
  :ensure t)

(use-package helm-dired-recent-dirs
  :ensure t)

(use-package helm-dired-history
  :ensure t)

(use-package helm-company
  :ensure t)

(use-package helm-clojuredocs
  :ensure t)

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(global-linum-mode t)


;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
;; Theme
;;(load-file "~/.emacs.d/themes/dracula2-theme.el")
;;(dracula2);





;; Highlight first word for java api calls
;; From https://www.reddit.com/r/Clojure/comments/56e3hp/syntax_highlighting_for_function_calls_in_emacs/
(add-hook 'clojure-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '(("(\\(\\w+\\)\\s-+" 1 font-lock-keyword-face)))))

;; show opening, closing parens
(show-paren-mode)

(require-package 'epl)

(require-package 'exec-path-from-shell)
;; Sort out the $PATH for OSX
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(toggle-scroll-bar -1)

(require 'cider-eldoc)

;; helm
(require 'helm-config)

;;cljr-helm
(use-package cljr-helm
  :ensure t)

;; global set helm
(global-set-key (kbd "M-x") 'helm-M-x)

(autoload 'clojure-mode "clojure-mode" "clojure mode" t)
(add-hook 'cider-mode-hook
        (lambda () (setq next-error-function #'flycheck-next-error-function)))

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(add-hook 'clojure-mode-hook 'cider-mode)
;; Add keybindings for items
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq-local cider-repl-use-pretty-printing t)
            (local-set-key [f5] 'helm-imenu)
            (local-set-key [f6] 'cljr-helm)
            (local-set-key (kbd "<C-f5>") 'cider-test-run-test)
            (cider-auto-test-mode t)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (local-set-key [f6] 'cljr-helm)
            ))

(provide 'ca-clojure)

(add-hook 'after-init-hook 'global-company-mode)


;; Magit for git
(global-set-key (kbd "C-x g") 'magit-status)

;; cider configuration
(setq cider-font-lock-dynamically '(macro core function var))


;; Custom User configurations:
;; If you wish to add additional functionality to your emacs config beyond what is in this setup,
;; simply add a file called "user-customizations.el" to your .emacs.d/lisp/ directory. Within that file,
;; you have access to the (require-package ...) function defined here, so for example, you could have:
;; (require-package 'rainbow-delimiters)
;; This would be all that is needed for emacs to automatically download the Rainbow Delimiters package
;; from Melpa. Additional configs of any kind could be added to this user-customizations.el file.
;; If the file is ommitted, no problem, no customizations are run.


(when (file-exists-p (concat dotfiles-lisp-dir "user-customizations.el"))
  (load (concat dotfiles-lisp-dir "user-customizations.el")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Send backups and auto saves to direcotey
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups



                                        ; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq cljr-inject-dependencies-at-jack-in nil)

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  :config (load-theme 'spacemacs-dark t))
