;;; package --- Summary
;; init.el file

;;; Commentary:

;;; Code:

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile (require 'use-package))
(require 'bind-key)
;; (require 'diminish)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)

;; elpy
(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-backend "jedi") ;; also ?? set in custom variables
  )
(add-hook 'python-mode-hook 'jedi:setup)
;; turn off pyvenv
(pyvenv-mode -1)
;; (when (require 'elpy nil t)
;;   (elpy-enable))
;; (add-hook 'python-mode-hook 'jedi:setup)
;;added per http://www.unknownerror.org/opensource/davidhalter/
;;jedi/q/stackoverflow/29809061/how-to-properly-setup-jedi-with-elpy-in-emacs
(setq elpy-rpc-backend "jedi")  
;; (elpy-use-ipython)

;; resolve company - yasnippet conflicts
;; https://github.com/jorgenschaefer/elpy/wiki/FAQ
(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

;; prevent autocomplete from starting automatically
;; (global-auto-complete-mode 0)

;; winner mode always
(winner-mode 1)

;; occur mode
(global-set-key (kbd "C-c o") 'occur)

;; cider code completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; column numbers
(setq column-number-mode t)

;; code folding
(defun fold ()
  (interactive)
  (if (not selective-display)
    (set-selective-display 2)
    (set-selective-display nil)))
(global-set-key (kbd "C-c f") 'fold)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(use-package web-mode :defer 8)

;; rainbow
(use-package rainbow-delimiters
  :init
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

;; expand region
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; window resizing
(use-package cycle-resize
  :defer 5
  :bind
  ("C-M-v" . cycle-resize-window-vertically)
  ("C-M-h" . cycle-resize-window-horizontally))

;; ace-window
(use-package ace-window
  :bind ("M-p" . ace-window))

;; magit
(use-package magit
  :bind ("C-x g" . magit-status))

;; multiple cursors
(use-package multiple-cursors
  :bind
  (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))

;; smex
(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ("C-c C-c M-x" . execute-extended-command))
;; smex keys
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; fill-column-indicator
(use-package fill-column-indicator
  :config
  (setq fci-rule-color "lightblue")
  (setq fci-rule-column 80)
)

;; smartparens
(use-package smartparens
  :config
  (defun sp-setup ()
    (progn
      (electric-pair-mode 0)
      (smartparens-strict-mode 1)
      (sp-use-smartparens-bindings))))

(add-hook 'clojure-mode-hook 'sp-setup)
(add-hook 'emacs-lisp-mode-hook 'sp-setup)
;;
;; typing replaces selection
(delete-selection-mode 1)
;;
;; ido mode
(use-package ido
  :config
  (progn
    (setq ido-enable-f-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)
    (ido-yes-or-no-mode)
    (setq ido-use-faces nil))
  :bind
  ("C-x o" . ido-select-window))

(use-package flx-ido)
     
;; code completion
(add-hook 'after-init-hook 'global-company-mode)
;;
;; yasnippets
;;
(use-package yasnippet
  :init
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  :config
  (yas-global-mode 1))

;; jslint (installed with npm -g install jslint)
(use-package flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; js completion
(use-package jquery-doc :defer 6)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(use-package clojure-snippets)
;; (yas-load-directory "~/.emacs.d/snippets")

;; avy
(use-package avy
  :defer t
  :config
  (avy-setup-default))

;; emacs as python IDE video

(use-package projectile
  :defer t
  :config
  (projectile-global-mode))

;; flycheck
(use-package flycheck
  :defer t
  :config
 (add-hook 'after-init-hook #'global-flycheck-mode))

;;
(use-package company-jedi
  :defer t
  :config
  (add-to-list 'company-backends 'company-jedi))

;; highlight indent 
(highlight-indentation-mode nil)
;; (set-face-background 'highlight-indentation-face "#d3d3d3")
;; (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")

;; git-gutter
(use-package git-gutter
  :defer t
  :config
  (global-git-gutter-mode t))
;; (git-gutter:linum-setup)
;; (custom-set-variables
;;  '(git-gutter:update-interval 2))
;;
;; warn long lines
(add-hook 'prog-mode-hook 'column-enforce-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(backup-by-copying t)
 '(backup-directory-alist
   (quote
    (("html$" . "bak")
     ("clj$" . "bak")
     ("js$" . "bak")
     ("org$" . "bak")
     ("py$" . "bak"))))
 '(custom-enabled-themes (quote (manoj-dark)))
 '(delete-old-versions t)
 '(desktop-save-mode t)
 '(electric-pair-mode t)
 '(elpy-rpc-backend "jedi")
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(flymake-log-level 2)
 '(flymake-start-syntax-check-on-find-file nil)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indent turn-on-font-lock turn-on-eldoc-mode turn-on-haskell-doc-mode turn-on-haskell-unicode-input-method)))
 '(package-selected-packages
   (quote
    (ag jquery-doc flymake-jslint web-mode ace-window smartparens-config zencoding zencoding-mode which-key use-package unbound swiper smex smartparens ranger rainbow-mode rainbow-delimiters projectile org multiple-cursors magit key-chord js2-mode idomenu ido-yes-or-no ido-vertical-mode ido-select-window ido-grid-mode ido-exit-target ido-describe-bindings git-gutter flycheck flx-ido fill-column-indicator eyebrowse expand-region exec-path-from-shell emmet-mode elpy cycbuf company-jedi column-enforce-mode clojure-snippets cider avy-zap anaconda-mode)))
 '(save-place t nil (saveplace))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;; (eyebrowse-mode t)
;;

;;
;; key mappings
(global-set-key (kbd "C-c n") 'linum-mode)
;; (global-set-key (kbd "<f5>") 'cider-eval-last-sexp)
;;(global-set-key (kbd "<f7>") 'kill-buffer)
;;(global-set-key (kbd "<f8>") 'cider-jack-in)
(global-set-key (kbd "<f9>") 'completion-at-point)
(global-set-key (kbd "M-z") 'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") 'avy-zap-up-to-char-dwim)
(global-set-key (kbd "C-?") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-x 8 d") 'ranger)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c x") 'kill-whole-line)
;; (global-set-key (kbd "M-g f") 'avy-goto-line)
;; (global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g") 'goto-line)

;; yas-expand alternative for modes where something else bound to tab
(global-set-key (kbd "<f5>") 'yas-expand)

;; which-key--current-key-list
(which-key-mode)

;; copy line
(defun quick-copy-line ()
      "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
      (interactive)
      (let ((beg (line-beginning-position 1))
            (end (line-beginning-position 2)))
        (if (eq last-command 'quick-copy-line)
            (kill-append (buffer-substring beg end) (< end beg))
          (kill-new (buffer-substring beg end))))
      (beginning-of-line 2))

;; Behave like vi's o command
(defvar newline-and-indent)
(defun open-next-line (arg)
  "Move to the next line and then opens a line (ARG repeats).
See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one (ARG repeats). 
See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "M-o") 'open-previous-line)
;;
;; move lines up or down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; more setup needed, see andrew werner on github.com/wernerdrew/jedi-starter

;;aliases from ergoemacs
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
;; make frequently used commands short
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

; major modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'clj 'clojure-mode)

; minor modes
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'glm 'global-linum-mode)
(defalias 'hsk 'helm-show-kill-ring)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
)

;; keychord
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xf" 'ido-find-file)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "xb" 'ido-switch-buffer)
(key-chord-define-global "zx" 'repeat)
(key-chord-define-global "xc" 'quick-copy-line)

;; for os x specific stuff
(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'super)
      (setq ns-function-modifier 'hyper)))

;; for note taking in org mode
(fset 'next-note
      [?\C-a ?\C-  ?\M-f right ?\M-w ?\C-e return ?\C-y ?\C-e ?\C-p])
(global-set-key (kbd "s-.") 'next-note)

;; my own utility functions
(load-file "~/.emacs.d/utility.el")
(global-set-key (kbd "s-r") 'rotate-windows)
(global-set-key (kbd "s-c") 'cleanup-buffer)

(provide 'init)
;;; init.el ends here
