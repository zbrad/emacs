(require 'polymode)

(defcustom ghp-host/html
  (pm-bchunkmode "html"
                 :mode 'html-mode)
  "HTML host chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom ghp-host/yaml
  (pm-bchunkmode "YAML"
                 :mode 'yaml-mode)
  "YAML chunkmode"
  :group 'hostmodes
  :type 'object)

(defcustom ghp-host/markdown
  (pm-bchunkmode "Markdown"
               :mode 'markdown-mode)
  "Markdown host chunkmode"
  :group 'hostmodes
  :type 'object)

;;
;; define the inner modes
;;
(defcustom ghp-inner/yfront
  (pm-hbtchunkmode "yaml"
                   :mode 'yaml-mode
                   :head-reg "\\`---\\s-*\n"
                   :tail-reg "\n---\\s-*\n"
                   )
  "yaml front matter chunk"
  :group 'innermodes
  :type 'object)

(defcustom ghp-inner/liquid-tag
  (pm-hbtchunkmode "liquid"
                   :mode 'ghp-liquid-mode
                   :head-reg "\n\\s-*{%\\s-+"
                   :tail-reg "\\s-+%}\\s-*\n"
                   )
  "liquid tag"
  :group 'innermodes
  :type 'object)

(defcustom ghp-inner/liquid-expression
  (pm-hbtchunkmode "liquid"
                   :mode 'ghp-liquid-mode
                   :head-reg "{{\\s-+"
                   :tail-reg "\\s-+}}"
                   )
  "Liquid expression"
  :group 'innermodes
  :type 'object)

;;
;; create poly modes for code segments
;;
(defcustom ghp-inner/highlight
  (pm-hbtchunkmode-auto "highlight"
      :head-reg "\n{%\s+highlight\s+\\(\w.*\\)?\s+\\(linenos\\)?.*%}\s*\n"
      :tail-reg "\n{%\s+endhighlight\s+%}\n"
      :head-mode 'ghp-markdown-mode
      :tail-mode 'ghp-markdown-mode
      :retriever-regexp "\n{%\s+highlight\s+\\.*%}\s*\n\\(.+\\)\n{%\s-+endhighlight\s+%}\n"
      :font-lock-narrow t)
  "highlighter chunk"
  :group 'innermodes
  :type 'object)

(defcustom ghp-inner/codefence
  (pm-hbtchunkmode-auto "codefence"
    :head-reg "\n```(\w.*)?\s*\n"
    :tail-reg "\n```\s*\n"
    :head-mode 'ghp-markdown-mode
    :tail-mode 'ghp-markdown-mode
    :retriever-regexp "\n```.*\n\\(.+)\n```\s*\n"
    :font-lock-narrow t)
  "code chunk"
  :group 'innermodes
  :type 'object)

;;
;; create polymode for markdown file
;;
(defcustom ghp-poly/markdown
  (pm-polymode-multi-auto "markdown"
      :hostmode 'ghp-host/markdown
;      :auto-innermodes '(ghp-inner/highlight ghp-inner/codefence)
      :auto-innermode 'ghp-inner/highlight
      :innermodes '(
                    ghp-inner/yfront
                    ghp-inner/liquid-expression
                    ghp-inner/liquid-tag
                    )
      )
  "Markdown with Yaml and Liquid tags support."
  :group 'polymodes
  :type 'object
  )

(defcustom ghp-poly/html
  (pm-polymode-multi "html"
      :hostmode 'ghp-host/html
;      :auto-innermodes '(ghp-inner/highlight ghp-inner/codefence)
      :innermodes '(
                    ghp-inner/yfront
                    ghp-inner/liquid-expression
                    ghp-inner/liquid-tag
                    )
      )
  "Markdown with Yaml and Liquid tags support."
  :group 'polymodes
  :type 'object
  )


;;;###autoload (autoload 'ghp-markdown-mode "GitHub Pages Markdown mode")
(define-polymode ghp-markdown-mode ghp-poly/markdown)

;;;###autoload (autoload 'ghp-html-mode "GitHub Pages Html mode")
(define-polymode ghp-html-mode ghp-poly/html)

(provide 'ghp-modes)
