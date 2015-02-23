;
; .emacs (-*-Emacs-Lisp-*-)
;
; Brad Merrill
; mailto:brad_merrill@hotmail.com
; Last updated: 19-Feb-2015
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs look and feel
 
;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
;(setq frame-title-format "Emacs: %b %+%+ %f")
(setq frame-title-format "%b%+ %f")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Key Bindings
 
;
; most of these strange key bindings are from the original Tops-20 Emacs
; my fingers are still wired the old way
;

(global-set-key "\C-u" 'kill-to-beg-of-line)                       ; old ^U behavior
(global-set-key "\C-w" 'backward-kill-word)                        ; old ^W behavior
(define-key esc-map "w" 'kill-region)                              ; old meta-w

;(defun kill-to-beg-of-line () (interactive) (kill-line 0))
(fset 'kill-to-beg-of-line "\C-[0\C-K")


;(global-unset-key "\C-x\C-c")                                          ; no exiting accidently
(setq kill-emacs-query-functions
     (cons (lambda () (yes-or-no-p "Really kill Emacs? "))
                    kill-emacs-query-functions))
 
(define-key esc-map "?" 'command-apropos)
(define-key esc-map "s" 'center-line)
(define-key esc-map "." 'set-mark-command)
 
(define-key ctl-x-map "s" 'save-buffer)
(define-key ctl-x-map "w" 'copy-region-as-kill)
(define-key ctl-x-map "c" 'compile)
(define-key ctl-x-map "g" 'goto-line)
 
(global-set-key "\C-h" 'quoted-insert)    
 
(define-key isearch-mode-map "\C-h" 'isearch-quote-char)
(define-key isearch-mode-map "\C-\\" 'isearch-repeat-forward)

(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-\\" 'isearch-forward)
 
;(global-unset-key "\C-q")                     ; unbind xoff
;(global-unset-key "\C-s")                     ; unbind xon
 
;(global-unset-key "\C-x\C-q")                 ; unbind xoff
;(global-unset-key "\C-x\C-s")                 ; unbind xon
(put 'downcase-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc
;
(setq max-specpdl-size 1000)
(setq auto-save-interval 200)
(setq-default case-fold-search t)
(setq-default comment-column 40)
(setq completion-auto-help nil)
(setq enable-recursive-minibuffers t)
(setq-default fill-column 64)
(setq inhibit-startup-message t)
(setq insert-default-directory nil)
(show-paren-mode t)  ;; always turn parentheses mode on.

(defconst --batch-mode 
  (or noninteractive (member "--batch-mode" command-line-args))
  "True when running in batch-mode (--batch-mode command-line switch set).")

(unless (or --batch-mode (not window-system))
  (require 'server)
  (when (and (= emacs-major-version 23)
         (= emacs-minor-version 1)
         (equal window-system 'w32))
    ;; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows.
    (defun server-ensure-safe-dir (dir) "Noop" t))
  (condition-case nil
      (server-start)
    (error
     (let* ((server-dir (if server-use-tcp server-auth-dir server-socket-dir)))
       (when (and server-use-tcp
          (not (file-accessible-directory-p server-dir)))
     (display-warning
      'server (format "Creating %S" server-dir) :warning)
     (make-directory server-dir t)
     (server-start))))
    )
  )

;; load sd helper
;;(load-library "sd")
;;(sd-set-sd-executable "d:/tools/sd/sd.exe")
 
;;(setq explicit-shell-file-name "e:/sd_orcas/tools/razzle")
;;(setq explicit-razzle-args '( "debug" ))
(setq explicit-shell-file-name "cmdproxy")
(setq shell-file-name "cmdproxy")
;;(setq explicit-cmdproxy-args '("/c"))
 
;;;;;
;; Font mode settings
(defun my-recenter (&optional arg)
  "Centre point in window and run font-lock-fontify-block"
  (interactive "P")
  (recenter arg)
  (font-lock-fontify-block (window-height)))
 
(setq my-font-lock-face-attributes
      '((font-lock-comment-face        "Firebrick")
	(font-lock-string-face         "SpringGreen4")
	(font-lock-keyword-face        "RoyalBlue")
	(font-lock-function-name-face  "Blue")
	(font-lock-variable-name-face  "GoldenRod")
	(font-lock-type-face           "DarkGoldenRod")
	(font-lock-constant-face       "Purple")
	))
 
(defun my-font-lock-mode-hook ()
	     (substitute-key-definition
	      'recenter 'my-recenter (current-global-map)))

;;;;;;;;;;;;;
;; compile
(setq-default compile-command "nmake")

;;(setq-default compile-command "e:/sd_orcas/tools/razzle no_sdrefresh exec Build -cZ -x86")
;;(setq-default compile-command "e:/sd_lh/tools/razzle no_oacr exec Build -cZP")
;;(setq-default compile-command "d:/nt/wfnext/tools/razzle no_sdrefresh amd64fre no_opt exec Build -cZ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for editing Vs tab styles
;;
(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
                (counter 1)
                (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; modify coding default for shell scripts for windows
(if (equal window-system 'w32)
(progn
  (modify-coding-system-alist 'file "\\.sh\\'" 'iso-latin-1-unix)
  (modify-coding-system-alist 'file "\\.csproj\\'" 'utf-8-dos)
 ))

;;;;;;;;;;
;; put all requires together

(require 'cl)
(require 'package)
(require 'font-lock)

(setq jit-lock-stealth-time 12
      jit-lock-defer-contextually t
      font-lock-multiline t)
(add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)

;;;;;;;;;;;;;;;;
;; bootstrap use-package
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;
;; load remaining packages with use-package

(use-package icicles :config (icy-mode 1) :ensure t)

;;;;;;;;;;;;;
;; jekyll bindings
(use-package markdown-mode :ensure t)
(use-package jekyll-modes :ensure t
  :mode
  (("\\.md\\'" . jekyll-markdown-mode)
   ("\\.text\\'" . jekyll-markdown-mode)
   ("\\.markdown\\'" . jekyll-markdown-mode)
   ("\\.html\\'" . jekyll-html-mode))
)

;;;;;;;;;;
;; yml mode
(use-package yaml-mode :ensure t)

;;;;;;;;;;
;; json mode
(use-package json-mode :ensure t :mode "\\.json\\'")

;;;;;;;;;;;;;;;;
;; flymake syntax checker
(use-package flymake :ensure t)
(use-package flymake-yaml :ensure t)
(use-package flymake-shell :ensure t)

;;;;;;;;;;;;;;;;
;; typescript checker
(use-package tss :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              C# Mode support
;;;
(defconst my-csharp-style
  '(
    (c-comment-continuation-stars . "/// ")
    (c-hanging-comment-starter-p . "///<summary>")
    (c-hanging-comment-ender-p . "///</summary>")
    (c-basic-offset . 2)
    (c-comment-only-line-offset . (0 . 0))
    (c-offsets-alist . (
      (inclass                 . 0)
      (namespace-open          . 0)
      (namespace-close         . 0)
      (innamespace             . 0)
      (class-open              . 0)
      (class-close             . 0)
      (inclass                 . 0)
      (defun-open              . +)
      (defun-block-intro       . 0)
      (inline-open             . +)
      (statement-block-intro   . 0)
      (brace-list-intro        . +)
      (else-clause             . -)
      ))
    ))

(defun my-csharp-mode-hook ()
   (cond (window-system
     (require 'cc-mode)
     (c-set-style "my-csharp-style")
     (setq compilation-error-regexp-alist '(
;C# Compiler
;t.cs(6,18): error SC1006: Name of constructor must match name of class
;
      ("^\\(.+\\.cs\\)(\\([0-9]+\\)[,]\\([0-9]+\\))\\s ?: \\(error\\|warning\\) CS[0-9]+:" 1 2 3)))
     )))

;;;;
;; have a java style too
(defconst my-java-style
  '(
  (c-basic-offset . 2)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (inclass               . 0)
    (namespace-open        . 0)
    (namespace-close       . 0)
    (innamespace           . 0)
    (class-open            . 0)
    (class-close           . 0)
    (inclass               . 0)
    (defun-open            . +)
    (defun-block-intro     . 0)
    (inline-open           . +)
    (statement-block-intro . 0)
    (brace-list-intro      . +)
    (else-clause           . -)
    ))
  ))
 
(defun my-java-mode-hook ()
  (cond (window-system
	 (c-set-style "my-java-style")
	 )))
 
;;;;;;;;;;;;;;;;;;;;;;;;
;; add a jscript style
(defconst my-jscript-style
  '(
  (c-basic-offset . 2)
  (c-comment-only-line-offset . (0 . 0))
  (c-offsets-alist . (
    (c                     . c-lineup-C-comments)
    (inclass               . 0)
    (namespace-open        . 0)
    (namespace-close       . 0)
    (innamespace           . 0)
    (class-open            . 0)
    (class-close           . 0)
    (inclass               . 0)
    (defun-open            . +)
    (defun-block-intro     . 0)
    (inline-open           . +)
    (statement-block-intro . 0)
    (brace-list-intro      . +)
    (else-clause           . -)
    ))
  ))
 
(defun my-jscript-mode-hook ()
  (cond (window-system
	 (c-set-style "my-jscript-style")
	 )))

(use-package cc-mode
  :config
  (progn
    (c-add-style "my-java-style" my-java-style)
    (c-add-style "my-jscript-style" my-jscript-style)
    (add-hook 'java-mode-hook 'my-java-mode-hook)
    (add-hook 'jscript-mode-hook 'my-jscript-mode-hook)
    ))

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :config
  (progn
    (c-add-style "my-csharp-style" my-csharp-style)
    (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
    ))
