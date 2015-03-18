(setq ghp-liquid-keywords
    '(
;;; tag delimiters
      ("{%\\|%}\\|{{\\|}}" . font-lock-comment-face) 

      ("{%\s*\\(assign\\|capture\\|endcapture\\|for\\|forloop\\|endfor\\|if\\|endif\\|comment\\|endcomment\\|else\\|elsif\\|unless\\|endunless\\|case\\|when\\|endcase\\|cycle\\)" (1 font-lock-keyword-face))

;;; if/else operators (keyword)
      ("{%\s*\\(?:if\\|unless\\)\s+\\(?:[\\w._]+\\)\s+\\(contains\\|>\\|<\\|==\\|!=\\)" (2 font-lock-variable-name-face) (3 font-lock-keyword-face))

;;; loop stuff

;;; the 'in' keyword: "for temp in collection"
      ("{%\s*for\s+\\w+\s+\\(in\\)" (1 font-lock-keyword-face))

;;; the 'collection' in "for temp in collection"
      ("{%\s*for\s+\\w+\s+in\s+\\(?:[\\w._]+\\)" (1 font-lock-variable-name-face))

      ("forloop.\\(length\\|index0\\|index\\|rindex0\\|rindex\\|first\\|last\\)" (1 font-lock-function-name-face))

      ("{%\s*\\(?:assign\\|capture\\|for\\|if\\|unless\\|case\\|when\\)\s+\\(?:[\\w._]+\\)" (2 font-lock-variable-name-face))

;;; variable after assign|capture|for|if
      ("{{\s*\\(\\(?:[\\w.]\\)+\\)" (1 font-lock-variable-name-face))
      ("{{\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\(\\w+\\)" (1 font-lock-variable-name-face))
      ("{{\s*\\(?:\\w\\|\\.\\)+\s+|\s+\\w+\s+|\s+\\(\\w+\\)" (1 font-lock-variable-name-face))

      ))

;;;###autoload
(define-derived-mode ghp-liquid-mode html-mode
  "mode for GitHub Pages Liquid Tags"
  (setq font-lock-defaults '(ghp-liquid-keywords))
  (setq mode-name "liquid mode")
  )

(provide 'ghp-liquid-mode)


