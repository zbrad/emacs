;;;;;;;;;;;;;;;;;;;;;
;; functions for managing blog content

(defun ghp-slug (s) "Turn a string into a slug."
    (replace-regexp-in-string " " "-" 
      (downcase 
        (replace-regexp-in-string "[^A-Za-z0-9 ]" "" s))))

(defun ghp-yaml-escape (s) "Escape a string for YAML."
    (if (or (string-match ":" s) (string-match "\"" s))
        (concat
           "\""
           (replace-regexp-in-string "\"" "\\\\\"" s)
           "\"")
        s
        ))

;;;###autoload
(defun ghp-post (title) "Create GitHub Pages blog post"
    (interactive "sPost Title: ")
    (let (
          (post-file (concat default-directory
                            "/_posts/"
                            (format-time-string "%Y-%m-%d-")
                            (ghp-slug title)
                            ".md"))
          )
         (find-file post-file)
         (insert (format ghp-template (ghp-yaml-escape title)))
         ))

;;;###autoload
(defun ghp-list () "Get list of posts"
    (interactive)
    (find-file (concat default-directory "_posts/")))

(provide 'ghp-blog)
