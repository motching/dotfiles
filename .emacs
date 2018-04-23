;;;Emacs config

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

(require 'ansi-color)

(defun helm-git-grep-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'helm-grep-do-git-grep))

;;my keybindings
(global-set-key (kbd "C-x a") 'ace-window)
(global-set-key (kbd "C-x c") 'company-complete)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x i") 'indent-region)
(global-set-key (kbd "C-x x") 'replace-regexp)
(global-set-key (kbd "C-x s") 'helm-git-grep-with-prefix-arg)
(global-set-key (kbd "C-x w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x f") 'multi-occur-in-matching-buffers)
(global-set-key (kbd "C-x t") 'treemacs-toggle)
(global-set-key (kbd "C-x e") 'list-flycheck-errors)

(global-set-key (kbd "C-x m") 'mvn-compile)

;;window splitting
(defun nars()
  "narrow split"
  (interactive)
  (split-window-below -9))

;;maven compiler should not spit stuff
(with-eval-after-load "mvn"
(defun mvn-compile ()
  (interactive)
  (mvn "-q compile")))

;;disable splash screen
(setq inhibit-startup-message t)

;;hide bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;disable electric-indent-mode
(electric-indent-mode -1)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;;whitespace-mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(global-whitespace-mode t)

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

;;load theme
(require 'dash)
(require 's)

(-each
   (-map
      (lambda (item)
      (format "~/.emacs.d/elpa/%s" item))
   (-filter
      (lambda (item) (s-contains? "theme" item))
      (directory-files "~/.emacs.d/elpa/")))
   (lambda (item)
     (add-to-list 'custom-theme-load-path item)))

  (load-theme 'solarized-light t)

;;haskell
(add-hook 'haskell-mode-hook #'hindent-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(fci-rule-color "#eee8d5")
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-compile-cabal-build-command
   "cd %s && cabal build --ghc-option=-ferror-spans --ghc-option=-threaded")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-startup-folded (quote showeverything))
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (buffer-move rjsx-mode magit-gh-pulls sass-mode json-mode flx-ido helm-projectile projectile live-py-mode flycheck-pycheckers helm-git-grep company circe vimish-fold exec-path-from-shell mvn rainbow-delimiters hindent ghc ghc-imported-from ghci-completion haskell-mode scion treemacs xcscope use-package solarized-theme magit js2-refactor js2-closure helm flycheck ensime dockerfile-mode)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 4)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
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
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
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

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))



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
(require 'xcscope)
(cscope-setup)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;;javascript
(global-set-key (kbd "C-c C-'") 'js2-display-error-list)
(setq-default js2-global-externs '("module"
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
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-to-list 'auto-mode-alist '("\\.js$" . rjsx-mode))
;;(add-hook 'js2-mode-hook (lambda () (company-mode t)))
;;(add-hook 'js2-mode-hook (lambda () (js2-refactor-mode t)))
(js2r-add-keybindings-with-prefix "C-c C-m")


;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq flycheck-pos-tip-timeout 1)
(setq flycheck-display-errors-function nil)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

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


;;error buffer hack
(setq split-height-threshold 0)
(setq split-width-threshold nil)

;;exec path
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/home/attila/.cabal/bin")))
;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (setq exec-path-from-shell-variables '("PATH"))
;;   (exec-path-from-shell-initialize))

;;fold
(require 'vimish-fold)
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

;;C-mode
(defvar c-mode-hook
      '(lambda ()
         (c-set-style "BSD")
         ;;
         ;; Change this you you want a different basic indentation step.
         ;;
         (require 'xcscope)
         (cscope-setup)
         (setq c-basic-offset 4)
         ;;
         ;; The next 2 calls invert the normal c-mode meanings
         ;; of LF and CR. They make CR automatically indent
         ;; the next line. Remove them if you do not like that.
         ;;
         (define-key c-mode-map "\r" 'newline-and-indent)
         (define-key c-mode-map "\n" 'newline) ; newline = C-j

         (setq case-fold-search nil) ; C is case-sensitive
         ))

;;C++-mode

(defvar c++-mode-hook
      '(lambda ()
         (c-set-style "BSD")
         (setq c-basic-offset 4)

         (define-key c++-mode-map "\r" 'newline-and-indent)
         (define-key c++-mode-map "\n" 'newline) ; newline = C-j

         (setq case-fold-search nil) ; C++ is case-sensitive
         ))

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
        (defvar sgml-basic-offset)
            (set (make-local-variable 'sgml-basic-offset) 4)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;SUPPRESSED WARNINGS;;;;;;;;;;;;;;;;;
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
