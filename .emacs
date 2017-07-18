;
; .emacs (-*-Emacs-Lisp-*-)
;
; Brad Merrill
; mailto:brad_merrill@hotmail.com
; Last updated: 11-May-2017
;

;;;;;;;
;; os settings (usually paths)
;;


;;; c:\python3\Scripts\pip install jedi flake8 importmagic autopep8
(if (eq system-type 'windows-nt)
    (progn
;;      (setq python-environment-bin "c:/python3/python.exe")
;;      (setq python-environment-directory "cd:/python3/default")
;;      (setq python-environment-virtualenv "c:/python3/default")
;;      (setq python-shell-interpreter "c:/python3/python.exe")
;;      (setq python-shell-interpreter-args "-m")
;;      (add-to-list 'exec-path "c:/msys64/usr/bin")
;;      (add-to-list 'exec-path "c:/python3")
;;      (add-to-list 'exec-path "c:/python3/Scripts")
      (setq my-path-list '(
                      "c:/emacs/bin"
                      "c:/Program Files/Git/bin"
                      "c:/Program Files/nodejs"
                      "c:/msys64/mingw64/bin"
                      "c:/msys64/usr/bin"
                      "c:/python27"
                      "c:/python27/Scripts"
                      ;; "c:/python3"
                      ;; "c:/python3/Scripts"
                      ))
      (setq my-path (mapconcat 'identity my-path-list ";"))
      (setq exec-path (append exec-path my-path-list))
      (setenv "PATH" (concat (getenv "PATH") ";" my-path))


      (setq explicit-shell-file-name "c:/msys64/usr/bin/bash.exe")
      (setq shell-file-name explicit-shell-file-name)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; modify coding default for shell scripts for windows
      (setq-default prefer-coding-system 'utf-8)
      (setq-default buffer-file-coding-system 'utf-8)
      (modify-coding-system-alist 'file "\\.sh\\'" 'utf-8-unix)
      (modify-coding-system-alist 'file "\\.csproj\\'" 'utf-8-dos)
      ))

;;;;;;;;;
;; capture if batch
(defconst --batch-mode 
  (or noninteractive (member "--batch-mode" command-line-args))
  "True when running in batch-mode (--batch-mode command-line switch set).")

;;;;;;
;; skip rebindings and display changes if batch mode

(unless --batch-mode

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Emacs look and feel

  ;; Set the frame's title. %b is the name of the buffer. %+ indicates
  ;; the state of the buffer: * if modified, % if read only, or -
  ;; otherwise. Two of them to emulate the mode line. %f for the file
  ;; name. Incredibly useful!
  ;(setq frame-title-format "Emacs: %b %+%+ %f")

  (setq default-frame-alist '(
                              (font . "Inconsolata-14.5")
                              (height . 40)
                              (width . 100)
                              (title-format . "%b%+ %f")
                              ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; key bindings (no package dependencies)

  ;;
  ;; most of these strange key bindings are from the original Tops-20 Emacs
  ;; my fingers are still wired the old way
  ;;

  (defun kill-0 ()
    (interactive)
    (kill-line '(0)))

  (global-set-key "\C-u" 'kill-0)	      ; old ^U behavior
  (global-set-key "\C-w" 'backward-kill-word) ; old ^W behavior
  (define-key esc-map "w" 'kill-region)	      ; old meta-w
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

  (put 'downcase-region 'disabled nil)

  ;;;;;;;;;;;;;
  ;; special for non-window, eg console (unset xon/xoff)

  (if (not window-system)
      (progn
	  (global-unset-key "\C-q")                          ; unbind xoff
	  (global-unset-key "\C-s")                          ; unbind xon

	  (global-unset-key "\C-x\C-q")                      ; unbind xoff
	  (global-unset-key "\C-x\C-s")                      ; unbind xon

	  ))

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

  (tool-bar-mode -1)
  ;  (menu-bar-mode -1)
  (setq vc-handled-backends nil)
  (define-key menu-bar-tools-menu [vc] nil)      ; Remove VC
  (define-key menu-bar-tools-menu [games] nil)   ; Remove games menu
  (setq confirm-kill-emacs 'yes-or-no-p)	 ; confirm quit
;;  (setq-default indent-tabs-mode nil)	         ; always use spaces

  ;;;;;;;;;;;;;;;;;;;
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
  ;;(setq-default compile-command "nmake")

  ;;(setq-default compile-command "e:/sd_orcas/tools/razzle no_sdrefresh exec Build -cZ -x86")
  ;;(setq-default compile-command "e:/sd_lh/tools/razzle no_oacr exec Build -cZP")
  ;;(setq-default compile-command "d:/nt/wfnext/tools/razzle no_sdrefresh amd64fre no_opt exec Build -cZ")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; for editing Vs tab styles
  ;;
;;  (defun my-build-tab-stop-list (width)
;;    (let ((num-tab-stops (/ 80 width))
;;                  (counter 1)
;;                  (ls nil))
;;      (while (<= counter num-tab-stops)
;;        (setq ls (cons (* width counter) ls))
;;        (setq counter (1+ counter)))
;;      (set (make-local-variable 'tab-stop-list) (nreverse ls))))
  (setq default-tab-width 4)
)

(unless (or --batch-mode (not window-system))
  (require 'server)
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

;;;;;;;;;;;;;;;;;;;;;;;
;; load sd helper (source depot, older microsoft source control tool)
;;
;;(load-library "sd")
;;(sd-set-sd-executable "d:/tools/sd/sd.exe")

;;(setq explicit-shell-file-name "e:/sd_orcas/tools/razzle")
;;(setq explicit-razzle-args '( "debug" ))

;;(setq explicit-shell-file-name "cmdproxy")
;;(setq shell-file-name "cmdproxy")
;; C:\msys64\usr\bin\mintty.exe

;;(setq explicit-shell-file-name "C:/msys64/usr/bin/mintty.exe")
;;(setq shell-file-name "mintty")

;;;;;;;;;;;;;
;; useful if you want to set additional start options (env vars, etc)
;;(setq explicit-cmdproxy-args '("/c"))

(unless --batch-mode 
  (require 'font-lock)

  (setq jit-lock-stealth-time 12
	jit-lock-defer-contextually t
	font-lock-multiline t)
  (add-hook 'font-lock-mode-hook 'my-font-lock-mode-hook)
)

;;;;;;;;;;
;; put all requires together

(require 'cl)
(require 'package)

;;;;;;;;;;;;;;;;
;; bootstrap use-package
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;;;;;;;;;;;;
;; load remaining packages with use-package

(unless --batch-mode
  (use-package icicles :config (icy-mode 1) :ensure t))

(use-package cc-mode
  :defer t
  :config (progn
    (c-add-style "my-java-style" 'my-java-style)
    (c-add-style "my-jscript-style" 'my-jscript-style)
    (add-hook 'java-mode-hook 'my-java-mode-hook)
    (add-hook 'jscript-mode-hook 'my-jscript-mode-hook)
    )
)

;;;;;;;;;;;;;;
;; trying out magit for git

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; Restore windows after exiting magit
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit
  :ensure t
  :defer 2
  :diminish magit-auto-revert-mode
  :bind (("C-c s" . magit-status))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (bind-key "q" 'magit-quit-session magit-status-mode-map)
    (setq git-committer-email "brad_merrill@hotmail.com")
    (setq git-committer-name "Brad Merrill")
    )
  )

;;;;;;;;;;;;;
;; gfm mode
(use-package markdown-mode 
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
        ("\\.html\\'" . gfm-mode))
  )

(use-package yaml-mode :ensure t
  :mode "\\.yaml\\'"
  )

;;;;;;;;;;;;;;;;
;; typescript checker
(use-package tss :ensure t
  :mode "\\.ts\\'"
  :defer t
  )
;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

;;;;;;;;;;;;;;;;;;
;; react jsx mode
(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" "\\.json\\'" "\\.jsx\\'")
  :defer t
  :config
  (progn
    (setq indent-tabs-mode t)
    (setq sgml-basic-offset 4)
    (setq js-basic-offset 4)
    (setq js-indent-level 4)
    (setq js2-basic-offset 4)
    (setq js2-bounce-indent-p t)
    )
  )

;;;;;;;;;;;;;;;
;; css mode
(use-package css-mode
  :ensure t
  :defer t
  :mode ("\\.scss\\'" "\\.sass\\'")
  )

;;;;;;;;;;;;;;;
;; python
;(use-package python
;  :ensure t
;  :defer t
;  :mode ("\\.py\\'" . python-mode)
;  )

(use-package elpy
  :ensure t
  :config
  (progn
    (setq elpy-rpc-python-command "c:/python3/python")
    (setq elpy-rpc-backend "jedi")
    (add-hook 'python-mode-hook 'jedi:setup)
    (elpy-enable)
    ))

(use-package jedi
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc)))

;; (use-package python
;;   :ensure t
;;   :mode ("\\.py" . python-mode)
;;   :config
;;   (use-package elpy
;;     :ensure t
;;     :commands elpy-enable
;;     :config
;;     (setq elpy-rpc-python-command "python3"
;; 	  elpy-modules (dolist (elem '(elpy-module-highlight-indentation
;; 				       elpy-module-yasnippet))
;; 			 (remove elem elpy-modules)))
;;     (elpy-use-ipython))
;;   (elpy-enable)
;;   (add-hook 'python-mode-hook #'smartparens-strict-mode))

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

(use-package csharp-mode
  :ensure t
  :mode "\\.cs\\'"
  :config
  (progn
    (c-add-style "my-csharp-style" my-csharp-style)
    (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
    ))

