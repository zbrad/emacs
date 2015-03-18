
;;;###autoload
(defcustom ghp-hook nil "*Hook called by `ghp'."
  :require 'ghp
  :type 'hook
  :group 'ghp)

;;;###autoload
(defun ghp-init () "initialize ghp"
    (dir-locals-set-class-variables
     'ghp-folder-class
     '((nil . (
               (ghp-folder . default-directory)
               (eval . (message "hello"))
               )))
     )
    (dolist (folder ghp-folders)
      (dir-locals-set-directory-class folder 'ghp-folder-class))
    )

(defun ghp-hook-default () "default hook"
   (local-set-key (kbd "C-c g l") 'ghp-list)
   (local-set-key (kbd "C-c g n") 'ghp-post)
   (add-to-list 'auto-mode-alist '("\\.md\\'" . ghp-markdown-mode))
   (add-to-list 'auto-mode-alist '("\\.html\\'" . ghp-html-mode))
   )

(defun ghp-run-hook () "run hook"
       (if (equal ghp-hook nil)
           (ghp-hook-default)
         (ghp-hook)))

(provide 'ghp-hook)
