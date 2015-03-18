;;
;; common definitions
;;
(defvar ghp-folders nil)

(defvar ghp-path nil "Path to GitHub Pages blog")

(defvar ghp-template
    "---\ntitle: %s\n---\n\n"
    "Default template for GitHub posts. %s will be replace by the post title.")

(provide 'ghp-defs)
