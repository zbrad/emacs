;;; ghp.el --- Emacs support for GitHub Pages

;; Copyright (c) 2015 Brad Merrill

;; Author: Brad Merrill (brad_merrill@hotmail.com)
;; Homepage: https://github.com/zbrad/emacs/ghp
;; Created: 1-Mar-2015
;; Keywords: github blog mode
;; Package-Version: 1.0
;; Package-Requires: ((emacs "24") (polymode "1.0") (markdown-mode "2.0") (cc-mode "5.32.5") (csharp-mode "0.8.8"))

;;; Commentary:

;; This packages provides both edit modes for github formats of
;; markdown and html, as well as some basic post management.

;; Here are the default key bindings:
;; C-c g l - List all posts
;; C-c g n - Create new post
;;

;; Requirements:
(require 'polymode)
(require 'markdown-mode)
(require 'yaml-mode)

(require 'ghp-defs)
(require 'ghp-liquid-mode)
(require 'ghp-modes)
(require 'ghp-blog)
(require 'ghp-hook)

;; Custom variables

(provide 'ghp)
;;; ghp.el ends here
