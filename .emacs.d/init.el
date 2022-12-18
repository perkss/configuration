;;;  ___      __   __   __

;;; EMACS Config based on EXORDIUM: https://github.com/emacs-exordium/exordium

;; Reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB). This reduces
;; the startup time.

(setq dotfiles-lisp-dir
      (file-name-as-directory
       (concat (file-name-directory
                (or (buffer-file-name) load-file-name))
               "lisp")))
(add-to-list 'load-path dotfiles-lisp-dir)

;; Set the cmd as meta key
(setq ns-command-modifier (quote meta))

(setq gc-cons-threshold 99999999)

(let ((min-version "27.1"))
  (when (version< emacs-version min-version)
    (error "This config requires at least Emacs %s, but you're running %s"
           min-version emacs-version)))

(setq initial-scratch-message
      "EXORDIUM DID NOT LOAD CORRECTLY.
Check the warnings and messages buffers, or restart with --debug-init")

(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(if (file-exists-p "/usr/bin/hunspell")
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(defconst exordium-before-init "before-init.el"
  "name of the before init file")

(defconst exordium-prefs "prefs.el"
  "name of the prefs file")

(defconst exordium-after-init "after-init.el"
  "name of the after init file")

(defconst exordium-custom "emacs-custom.el"
  "name of the customization file")

;; Use this file for HTTP proxy settings if needed for packages.  Also add
;; additional packages to exordium-extra-packages for packages to be
;; automatically pulled from the elpa archives

(defconst exordium-before-init-file (locate-user-emacs-file exordium-before-init)
  "location of the master before init file")

(defconst exordium-modules-dir (locate-user-emacs-file "modules")
  "location of the modules directory")
(defconst exordium-themes-dir (locate-user-emacs-file "themes")
  "location of the themes directory")
(defconst exordium-extensions-dir (locate-user-emacs-file "extensions")
  "location of the extensions directory")
(defconst exordium-local-dir (locate-user-emacs-file "local")
  "location of the local directory")

(defconst exordium-prefs-file (locate-user-emacs-file exordium-prefs)
  "location of the master prefs file")

(defconst exordium-after-init-file (locate-user-emacs-file exordium-after-init)
  "location of the master after init file")

(defconst exordium-custom-file (locate-user-emacs-file exordium-custom)
  "location of the customization file")

;; Save any custom set variable in exordium-custom-file rather than at the end of init.el:
(setq custom-file exordium-custom-file)

(defcustom exordium-extra-packages ()
  "Additional packages to auto load from elpa repositories"
    :group 'exordium
    :type  'list)

(defcustom exordium-extra-pinned ()
  "Additional packages locations to pin to"
  :group 'exordium
  :type  'list)

;; Taps definition of before and after files. These are loaded
;; after master 'before', 'after', and 'prefs' files

(defconst exordium-taps-root (locate-user-emacs-file "taps")
  "location of the tapped directories")

(defconst exordium-tapped-before-init-files ()
  "all tapped before init files, including master")

(defconst exordium-tapped-prefs-files ()
  "all tapped prefs files, including master")

(defconst exordium-tapped-after-init-files ()
  "all tapped after init files, including master")

(defconst exordium-melpa-package-repo "http://melpa.org/packages/"
  "URL for packages repository")

(defconst exordium-pinned-melpa-package-repo "http://melpa.org/packages/"
  "URL for pinned default packages. Set to stable melpa.org if you want stable")

(defconst exordium-gnu-package-repo "http://elpa.gnu.org/packages/"
  "URL for the GNU package repository")

(when (file-accessible-directory-p exordium-taps-root)
  (dolist (tap (nreverse (directory-files exordium-taps-root t "^[^\.][^\.]?*+")))
    (when (file-accessible-directory-p tap)
      (let ((tapped (concat (file-name-as-directory tap) exordium-before-init)))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-before-init-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-prefs))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-prefs-files tapped))
        (setq tapped (concat (file-name-as-directory tap) exordium-after-init))
        (when (file-readable-p tapped)
          (add-to-list 'exordium-tapped-after-init-files tapped))))))

(when (file-readable-p exordium-before-init-file)
  (add-to-list 'exordium-tapped-before-init-files exordium-before-init-file))

(when (file-readable-p exordium-prefs-file)
  (add-to-list 'exordium-tapped-prefs-files exordium-prefs-file))

(when (file-readable-p exordium-after-init-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-after-init-file))

(when (file-readable-p exordium-custom-file)
  (add-to-list 'exordium-tapped-after-init-files exordium-custom-file))

;; Load before init files
(dolist (tapped-file exordium-tapped-before-init-files)
  (load tapped-file))


;;; Packages from Melpa
;; Use M-x `package-refresh-contents' to update the cache.
;; Use M-x `package-list-package' to load and display the list of packages,
;; then press I to mark for installation and X to execute (it's like dired).

;; Initialize the package system
(require 'package)

(add-to-list 'package-archives
             (cons "melpa" exordium-melpa-package-repo) t)
(add-to-list 'package-archives
             (cons "melpa-pinned" exordium-pinned-melpa-package-repo) t)
(add-to-list 'package-archives
             (cons "gnu" exordium-gnu-package-repo) t)

(setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))

(package-initialize)

;; Load the packages we need if they are not installed already
(let ((package-pinned-packages (append
                                '((use-package             . "melpa-pinned")
                                  (diminish                . "melpa-pinned")
                                  (bind-key                . "melpa-pinned"))
                                exordium-extra-pinned))
      (has-refreshed nil))

  (defun update-package (p  has-refreshed)
    (unless (package-installed-p p)
      (unless has-refreshed
        (message "Refreshing package database...")
        (package-refresh-contents)
        (setq has-refreshed t)
        (message "Done."))
      (package-install p)))

  (dolist (pkg package-pinned-packages)
    (let ((p (car pkg)))
      (update-package p has-refreshed)))

  (dolist (pkg exordium-extra-packages)
    (update-package pkg has-refreshed)))


;;; Path for "require"

(add-to-list 'load-path exordium-modules-dir)

(defun add-directory-tree-to-load-path (dir &optional ignore-if-absent)
  "Add DIR and all its subdirs to the load path."
  (cond ((file-directory-p dir)
         (add-to-list 'load-path dir)
         (let ((default-directory dir))
           (normal-top-level-add-subdirs-to-load-path)))
        ((not ignore-if-absent)
         (warn "Missing directory: %s" dir))))

(add-directory-tree-to-load-path exordium-extensions-dir)
(add-directory-tree-to-load-path exordium-themes-dir)
(add-directory-tree-to-load-path exordium-local-dir t)

(setq custom-theme-directory exordium-themes-dir)


;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;;; Load Modules
(use-package bytecomp :ensure nil)
(defun recompile-modules ()
  "Recompile modules for which the .elc is older than the .el, if
the .elc exists. Also discard .elc without corresponding .el"
  (interactive)
  (dolist (dir (list exordium-modules-dir
                     exordium-themes-dir
                     exordium-extensions-dir
                     exordium-local-dir))
    (when (file-directory-p dir)
      ;; Recompile
      (dolist (el (directory-files dir t "\\.el$"))
        (let ((elc (byte-compile-dest-file el)))
          (when (and (file-exists-p elc)
                     (file-newer-than-file-p el elc))
            (byte-compile-file el))))
      ;; Discard .elc singletons
      (dolist (elc (directory-files dir t "\\.elc$"))
        (let ((el (concat (concat (file-name-sans-extension elc) ".el"))))
          (unless (file-exists-p el)
            (warn "Removing singleton .elc file: %s" elc)
            (delete-file elc)))))))
(recompile-modules)

(use-package init-lib :ensure nil)         ; utility functions - load this first
(use-package init-environment :ensure nil) ; environment variables

;;; Local preferences (fonts, frame size etc.)
(use-package init-prefs :ensure nil)       ; defines variables that prefs.el can override
(dolist (tapped-file exordium-tapped-prefs-files)
  (load tapped-file))

;;; Themes
;;; Note: use "export TERM=xterm-256color" for emacs -nw
(use-package init-progress-bar :ensure nil)
(when exordium-nw
  (set-face-background 'highlight nil))
(use-package init-themes :ensure nil :if exordium-theme)

;;; Desktop
(when exordium-desktop
  (use-package init-desktop :ensure nil))

;;; Look and feel
(use-package init-look-and-feel :ensure nil)   ; fonts, UI, keybindings, saving files etc.
(use-package init-font-lock :ensure nil)       ; enables/disables font-lock globally.
(use-package init-linum :ensure nil)           ; line numbers
(use-package init-smooth-scroll
  :ensure nil
  :if exordium-smooth-scroll
  :config (smooth-scroll-mode 1)) ; smooth
                                                                                                       ; scroll

(update-progress-bar)

;;; Usability
(use-package init-window-manager :ensure nil)  ; navigate between windows
(use-package init-util :ensure nil)            ; utilities like match paren, bookmarks...
(use-package init-ido :ensure nil)             ; supercharged completion engine
(use-package init-highlight :ensure nil)       ; highlighting current line, symbol under point
(use-package init-autocomplete :ensure nil
  :if (eq exordium-complete-mode :auto-complete))
(use-package init-company :ensure nil
  :if (eq exordium-complete-mode :company))

(use-package init-helm-projectile :ensure nil
  :if exordium-helm-projectile)
(use-package init-helm :ensure nil)            ; setup helm

(use-package init-help :ensure nil
  :if exordium-help-extensions)

(update-progress-bar)

(use-package init-dired :ensure nil)           ; enable dired+ and wdired permission editing
(use-package init-git :ensure nil)             ; Magit and git gutter
(use-package init-git-visit-diffs :ensure nil) ; visit diffs in successive narrowed buffers
(use-package init-forge :ensure nil)           ; Forge
(use-package init-flb-mode :ensure nil)        ; frame-local buffers

(update-progress-bar)

;;; Prog mode
(use-package init-prog-mode :ensure nil)

;;; Shell mode
(use-package init-shell :ensure nil)

;;; Major modes
(use-package init-markdown :ensure nil)
(use-package init-org :ensure nil)
(use-package init-xml :ensure nil)

;;; OS-specific things
(use-package init-osx :ensure nil :if exordium-osx)

;;; C++
(use-package init-cpp :ensure nil)
(use-package init-bde-style :ensure nil)
(use-package init-yasnippet :ensure nil :if exordium-yasnippet)
(use-package init-gdb :ensure nil)


;;; RTags
(use-package init-rtags :ensure nil)
(when (and (eq exordium-complete-mode :auto-complete)
       exordium-rtags-auto-complete)
  (rtags-auto-complete))
(use-package init-rtags-helm :ensure nil)
(use-package init-rtags-cmake :ensure nil)
(use-package init-rtags-cdb :ensure nil)

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))


(use-package rtags
  :ensure t
  ;; defer loading after 1 second to speed startup
  :defer 1
  :hook (c++-mode . rtags-start-process-unless-running)
  :config
  (message "Loaded rtags")
  (setq rtags-autostart-diagnostics t)
  ;; use standard C-c r <key> keybindings
  (rtags-enable-standard-keybindings))

(setq rtags-display-result-backend 'helm)



(use-package company-rtags
  :ensure t
  ;; load immediately after rtags
  :after rtags
  :init
  (setq rtags-completions-enabled t)
  :config
  ;; add to company backends only after package is loaded
  (add-to-list 'company-backends 'company-rtags))

(use-package ivy-rtags
  :ensure t
  ;; package is loaded automatically by rtags, no need to load manually
  :defer t
  :init
  ;; use ivy autocompletion interface for displaying rtags suggestions
  (setq rtags-display-result-backend 'ivy))
(update-progress-bar)

;;; JS
(use-package init-javascript :ensure nil)

;;; Python
(use-package init-python :ensure nil)

;;; Ruby
(use-package init-ruby :ensure nil)

;;; Lisp
(use-package init-elisp :ensure nil)

;;; Clojure
(use-package init-clojure :ensure nil :if exordium-clojure)

;;; Groovy
(use-package init-groovy :ensure nil)

;;; include-what-you-use
(use-package init-iwyu :ensure nil)

(update-progress-bar)

;;; Local extensions
(dolist (tapped-file exordium-tapped-after-init-files)
  (load tapped-file))

(use-package init-powerline :ensure nil
  :if (and exordium-theme exordium-enable-powerline))

;; Docker
(use-package init-docker :ensure nil)

;;; LSP
(use-package init-lsp :ensure nil :if exordium-lsp-mode-enable)

(update-progress-bar)

;;; Greetings
(setq initial-scratch-message
      (let ((current-user (split-string (user-full-name) " ")))
        (format ";; Happy hacking %s!

" (if current-user (car current-user) exordium-current-user))))

;;; End of file

;;; Perkss Changes
(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq ring-bell-function 'ignore)

(setq large-file-warning-threshold 100000000)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; highlight the current line
(global-hl-line-mode +1)

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
    (rjsx-mode pry robe cljr-helm clj-refactor feature-mode helm-git flycheck-haskell smart-parens magithub zenburn-theme yaml-mode which-key web-mode volatile-highlights use-package undo-tree toml-mode tagedit swiper-helm sql-indent spacemacs-theme smex smart-mode-line shell-pop scala-mode rust-mode rainbow-mode rainbow-delimiters pytest pt popwin neotree move-text markdown-mode magit-annex kibit-helper json-mode js2-mode jedi inf-ruby imenu-anywhere ido-ubiquitous hl-todo hl-sexp highlight-symbol highlight-parentheses helm-swoop helm-projectile helm-flycheck helm-dired-recent-dirs helm-dired-history helm-descbinds helm-company helm-clojuredocs helm-ag hardcore-mode go-mode github-browse-file git-timemachine git-rebase-mode git-commit-mode flyspell-lazy flycheck-tip flycheck-pos-tip flycheck-joker flycheck-cython flycheck-color-mode-line flycheck-clojure flycheck-cask fic-mode expand-region exec-path-from-shell enh-ruby-mode emoji-cheat-sheet-plus elisp-slime-nav easy-kill discover diff-hl cpputils-cmake counsel company-web company-jedi cmake-mode clojure-mode-extra-font-locking clojure-cheatsheet cljsbuild-mode cider-eval-sexp-fu cask-mode cask browse-at-remote better-defaults avy anzu aggressive-indent)))
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

(use-package diff-mode
  :commands diff-mode)

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

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

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))


(use-package rjsx-mode
  :ensure t)

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

(use-package hardcore-mode
  :ensure t
  :init
  (setq too-hardcore-backspace t)
  (setq too-hardcore-return t)
  :config
  (global-hardcore-mode))


;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))


;; don't use tabs for indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
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

(use-package paren
  :config
  (show-paren-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

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

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))



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

(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))


(use-package rjsx-mode
  :ensure t)

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

(use-package flycheck-tip
  :ensure t)

(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))


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

(update-progress-bar)
(show-paren-mode)

;; Magit for git
(global-set-key (kbd "C-x g") 'magit-status)

;; Send backups and auto saves to direcotey
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

 (setq backup-directory-alist
       `(("." . ,(concat user-emacs-directory "backups"))))


; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq cljr-inject-dependencies-at-jack-in nil)

(use-package spacemacs-common
  :ensure spacemacs-theme
  :init
  :config (load-theme 'spacemacs-dark t))

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))

(update-progress-bar)

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(use-package cmake-project)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p
             (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)
