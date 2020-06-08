;;; init-md.el --- markdown
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; `md-mode'
;;----------------------------------------------------------------------------
(use-package markdown-mode
  :delight "μ "
  :mode ("\\.markdown\\'" "\\.md\\'")
  :custom (markdown-command "/usr/local/bin/pandoc"))
;; -END

;;----------------------------------------------------------------------------
;; `md-preview'
;;----------------------------------------------------------------------------
(use-package markdown-preview-mode
  :after markdown-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))
;; -END


(provide 'init-md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-md.el ends here
