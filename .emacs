;;Emacs config

(setq require-final-newline t) ; Makes sure the last line is terminated.
;; (If require-final-newline is not on, Emacs will too easily let you generate
;; text files where the last line has no terminating newline, which confuses
;; several tools).

(setq confirm-kill-emacs 'yes-or-no-p)

;;autosave
(defvar auto-save-interval 1000)

;;put backup files into a temporary directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ;;                 ("marmalade" . "https://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package ansi-color
  :ensure t)

(use-package prettier
  :ensure t)

(use-package indium
  :ensure t)

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(use-package exec-path-from-shell
  :ensure t)

;;Undo tree
(global-undo-tree-mode)

;; Projectile
(projectile-mode +1)

;;bookmark cycle in all buffers
(setq bm-cycle-all-buffers t)
(setq bm-highlight-style 'bm-highlight-line-and-fringe)

;;this will help remember TRAMP sessions
(setq desktop-files-not-to-save "^$")
(setq desktop-buffers-not-to-save "^$")

(use-package magit
  :ensure t)

(use-package helm
  :ensure t)

(use-package helm-git-grep
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package treemacs
  :ensure t)

(use-package solarized-theme
  :ensure t)

(load-theme 'solarized-light t)

;;my keybindings
(global-set-key (kbd "C-c a") 'ace-window)
(global-set-key (kbd "C-c b a") 'bm-show-all)
(global-set-key (kbd "C-c b c") 'bm-toggle-cycle-all-buffers)
(global-set-key (kbd "C-c b k") 'bm-remove-all-all-buffers)
(global-set-key (kbd "C-c b r") 'bm-remove-all-current-buffer)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)
(global-set-key (kbd "C-c c") 'company-complete)
(global-set-key (kbd "C-c d s") 'desktop-save)
(global-set-key (kbd "C-c d c") 'desktop-change-dir)
(global-set-key (kbd "C-c e l") 'list-flycheck-errors)
(global-set-key (kbd "C-c e f") 'eslint-fix)
(global-set-key (kbd "C-c e p") 'prettier-prettify)
(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g d") 'magit-diff-popup)
(global-set-key (kbd "C-c g f") 'magit-file-popup)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c i d") 'insert-debugger)
(global-set-key (kbd "C-c i l") 'insert-console-log)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-c l s") 'slack-start)
(global-set-key (kbd "C-c l c") 'slack-channel-select)
(global-set-key (kbd "C-c l i") 'slack-im-select)
(global-set-key (kbd "C-c m") 'mvn-compile)
(global-set-key (kbd "C-c m") 'mvn-compile)
(global-set-key (kbd "C-c n") 'narrow-split)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c s s") 'helm-git-grep)
(global-set-key (kbd "C-c s a") 'helm-git-grep-at-point)
;; not necessarily a good idea to put multiple things under C-c t
;; TODO think about keybindings
(global-set-key (kbd "C-c t r") 'treemacs)
(global-set-key (kbd "C-c t l") (lambda () (interactive) (load-theme 'solarized-light t)))
(global-set-key (kbd "C-c t d") (lambda () (interactive) (load-theme 'solarized-dark t)))
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c x") 'replace-regexp)

;;window splitting
(defun narrow-split()
  (interactive)
  (split-window-below -9))

;;maven compiler should not spit stuff
(with-eval-after-load "mvn"
  (defun mvn-compile ()
    (interactive)
    (mvn "-q compile")))

;; slack

(defun trim-final-newline (string)
  (let ((len (length string)))
    (cond
     ((and (> len 0) (eql (aref string (- len 1)) ?\n))
      (substring string 0 (- len 1)))
     (t string))))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;(el-get-bundle slack)
(use-package slack
  :ensure t
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "fusedesk"
   :default t
   :client-id "aaaaaaaaaaa.00000000000"
   :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   :token (trim-final-newline (get-string-from-file "~/.emacs.d/slack_token"))
   :subscribed-channels '()
   :full-and-display-names t)
  )

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(defun slack-user-status (_id _team) "")

;; (add-to-list
;;  'alert-user-configuration
;;  '(((:title . "\\(fusedesk-dev\\)")
;;     (:category . "slack"))
;;    libnotify nil))

;; (add-to-list
;;  'alert-user-configuration
;;  '(((:message . "@akovacs101386\\|Attila"))
;;    libnotify nil)
;; )

;;disable splash screen
(setq inhibit-startup-message t)

;;(use-package scroll-bar-mode
;;  :ensure t)

;;hide bars
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(scroll-bar-mode -1)

;;disable electric-indent-mode
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;;whitespace-mode
(use-package whitespace
  :ensure t)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(global-whitespace-mode t)

;;faster than the default scp
(setq tramp-default-method "ssh")

;;ttracker settings
(setq tramp-default-user "deploy")
(setq tramp-default-host "trucktracker.net")

;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck
  :ensure t)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-pos-tip-timeout 1)
(setq flycheck-display-errors-function nil)
(add-hook 'after-init-hook #'global-flycheck-mode)
;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; no paredit for now
;; (require 'paredit)
;; (add-hook 'prog-mode-hook #'enable-paredit-mode)

(use-package paren
  :ensure t)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

;;colored parentheses
(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;completion
(use-package company
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-dabbrev-downcase nil)

;;replace in rectangles
(cua-selection-mode 1)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;set scratch message
(setq initial-scratch-message "Far and away the best prize that life has to offer is the chance to work hard at work worth doing.")

;;line length

;;Turn on region uppercase and lowercase
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;sane handling of selected text
(delete-selection-mode 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)

;;smooth scrolling
(setq scroll-conservatively 101)

;;auto-revert (e.g. changing git branches)
(global-auto-revert-mode t)

;;add semi-slow scroll
(defun up-semi-slow () (interactive) (scroll-up 2))
(defun down-semi-slow () (interactive) (scroll-down 2))
(global-set-key "\M-N" 'up-semi-slow)
(global-set-key "\M-P" 'down-semi-slow)

;;display column number
(setq column-number-mode t)

;;Cycle through buffers (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;;line numbering
(global-linum-mode 1)

;;ediff characterwise
(setq-default ediff-forward-word-function 'forward-char)

;;IDO
(defvar ido-enable-flex-matching nil)
(defvar ido-enable-last-directory-history nil)
(defvar ido-record-commands nil)
(defvar ido-max-work-directory-list 0)
(defvar ido-max-work-file-list 0)
(ido-mode 'buffers)

;;open multiple files;
(declare-function dired-get-marked-files "dired")

(defun dired-find-marked-files ()
  (interactive)
  (dolist (f (dired-get-marked-files))
    (find-file f)))

;; Change overwrite-mode binding to C-Insert
(define-key global-map [(insert)] nil)
(define-key global-map [(control insert)] 'overwrite-mode)

;;ivy compilation
(use-package counsel
  :ensure t
  :bind (("C-c j" . counsel-imenu)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-X 4 b" . ivy-switch-buffer-other-window))
  :config (ivy-mode 1))

;;set emacs path from $PATH
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;load theme
(use-package dash
  :ensure t)
(use-package s
  :ensure t)

(-each
    (-map
     (lambda (item)
       (format "~/.emacs.d/elpa/%s" item))
     (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
  (lambda (item)
    (add-to-list 'custom-theme-load-path item)))

;; (defun my-haskell-hook ()
;;   (setq compile-command "stack build --fast --test --bench --no-run-tests --no-run-benchmarks"))


;; ;;haskell
;; (add-hook 'haskell-mode-hook #'hindent-mode)
;; (add-hook 'haskell-mode-hook #'my-haskell-hook)

(setq haskell-process-args-ghci
      '("-ferror-spans" "-fshow-loaded-modules"))

;; (setq haskell-process-args-cabal-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

;; (setq haskell-process-args-stack-ghci
;;       '("--ghci-options=-ferror-spans -fshow-loaded-modules"
;;         "--no-build" "--no-load"))

;; (setq haskell-process-args-cabal-new-repl
;;       '("--ghc-options=-ferror-spans -fshow-loaded-modules"))

;; (setq haskell-process-args-cabal-repl
;; '("--ghc-options -fshow-loaded-modules --ghc-options -ferror-spans"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0dd2666921bd4c651c7f8a724b3416e95228a13fca1aa27dc0022f4e023bf197" "653574dd35a64b45030075c99bb9e73f26d8abc7f21e145321e64fa2659fb6f5" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(fci-rule-color "#eee8d5")
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-compile-cabal-build-command
   "cd %s && cabal build --ghc-option=-ferror-spans --ghc-option=-threaded")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-tags-on-save t)
 '(helm-git-grep-candidate-number-limit nil)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(js-indent-level 4)
 '(js2-bounce-indent-p t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-startup-folded 'showeverything)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(dante indium prettier typescript-mode one-themes silkworm-theme plan9-theme xcscope counsel-etags magit yaml-mode flycheck-yamllint less-css-mode web-mode php-mode elm-mode slack bm undo-tree org-jira js-doc company-tern tern counsel eslint-fix ivy paredit buffer-move rjsx-mode sass-mode json-mode flx-ido helm-projectile projectile live-py-mode flycheck-pycheckers helm-git-grep company circe vimish-fold exec-path-from-shell mvn rainbow-delimiters hindent ghc ghc-imported-from ghci-completion haskell-mode scion treemacs use-package solarized-theme js2-refactor js2-closure helm flycheck dockerfile-mode))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 4)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(treemacs-space-between-root-nodes nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-enable-auto-indentation nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-interactive-bring)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;exec-path-from-shell should do this
;; (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;;   (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;;   (add-to-list 'exec-path my-cabal-path))



(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;clojure
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

;;scala
(add-hook 'scala-mode-hook 'ensime-mode)

;;cscope
;; (require 'xcscope)
;; (cscope-setup)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

                                        ;and html;
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;;javascript
(use-package rjsx-mode
  :ensure t)
(use-package web-mode
  :ensure t)

;;flycheck's syntax checking should be superior to js2-mode
(setq js2-mode-show-parse-errors nil)
(setq js2-mode-show-strict-warnings nil)
;;(global-set-key (kbd "C-c C-'") 'js2-display-error-list)

(setq-default js2-global-externs '("define"
                                   "module"
                                   "require"
                                   "buster"
                                   "sinon"
                                   "assert"
                                   "refute"
                                   "setTimeout"
                                   "clearTimeout"
                                   "setInterval"
                                   "clearInterval"
                                   "location"
                                   "__dirname"
                                   "console"
                                   "JSON"
                                   "google"
                                   "Audio"))

(setq-default js2-idle-timer-delay 0.1)
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;;(add-hook 'js2-mode-hook (lambda () (company-mode t)))
;;(add-hook 'js2-mode-hook (lambda () (js2-refactor-mode t)))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with rjsx-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; this checker sucks too
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(haskell-stack-ghc)))

;; use our own eslint
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;;error buffer hack
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;exec-path-from-shell should do this
;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))
;; (setq exec-path (append exec-path '("/home/attila/.cabal/bin")))

;;fold
(use-package vimish-fold
  :ensure t)
(vimish-fold-global-mode 1)

;; irc
(setq circe-reduce-lurker-spam t)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))


(defun my-c-hook ()
  (setq c-basic-offset 2)
  (use-package xcscope
    :ensure t)
  (cscope-setup))

(add-hook 'c-mode-hook #'my-c-hook)

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (defvar sgml-basic-offset)
            (set (make-local-variable 'sgml-basic-offset) 4)))

;; my macros
(fset 'insert-console-log
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 return up tab tab tab 99 111 110 115 111 108 101 46 108 111 103 40 41 59 left left] 0 "%d")) arg)))

(fset 'insert-debugger
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 return up tab tab 105 102 32 40 116 114 117 101 41 33554464 123 return tab tab 100 101 98 117 103 103 101 114 return tab 125 24 19 up up right] 0 "%d")) arg)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(slack-message-output-header ((t (:foreground "dark goldenrod" :weight bold :height 1.0 :family "Source Code Pro"))))
 '(slack-message-output-text ((t (:foreground "dark olive green" :weight bold :height 0.9 :family "Source Code Pro")))))
;;;;;;;;;;;;;;SUPPRESSED WARNINGS;;;;;;;;;;;;;;;;;
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(put 'narrow-to-region 'disabled nil)
