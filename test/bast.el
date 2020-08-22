(require 'ox-html)
(require 'ox-rss)
(require 'ox-publish)
(setq org-publish-project-alist
      '(("blog"
         :components ("blog-content" "blog-static" "blog-rss"))
        ("blog-content"
         :base-directory "~/Projects/blog/posts"
         :html-extension "html"
         :base-extension "org"
         :publishing-directory "~/Projects/blog/publish"
         :publishing-function (org-html-publish-to-html)
         :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Archive"
         :sitemap-sort-files anti-chronologically
         :sitemap-style list
         :makeindex t
         :recursive t
         :section-numbers nil
         :with-toc nil
         :with-latex t
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-head-extra
         "<link rel=\"alternate\" type=\"appliation/rss+xml\"
                href=\"http://bastibe.de/rss.xml\"
                title=\"RSS feed for bastibe.de\">
          <link href='http://fonts.googleapis.com/css?family=Roboto&subset=latin' rel='stylesheet' type='text/css'>
          <link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono' rel='stylesheet' type='text/css'>
          <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
          <title>Basti's Scratchpad on the Internet</title>
          <meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
          <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">"
         :html-preamble
         "<div class=\"header\">
              <a href=\"http://bastibe.de\">Basti's Scratchpad on the Internet</a>
              <div class=\"sitelinks\">
                  <a href=\"http://alpha.app.net/bastibe\">alpha.app.net</a>  | <a href=\"http://github.com/bastibe\">Github</a>
              </div>
          </div>"
         :html-postamble
         (lambda (info)
           "Do not show disqus for Archive and Recent Posts"
           (cond ((string= (car (plist-get info :title)) "Archive") "")
                 ((string= (car (plist-get info :title)) "Recent Posts")
                  "<div id=\"archive\"><a href=\"archive.html\">Other posts</a></div>")
                 (t
									"<div id=\"archive\"><a href=\"archive.html\">Other posts</a></div>
              <div id=\"disqus_thread\"></div>
              <script type=\"text/javascript\">
              /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
              var disqus_shortname = 'bastibe';
              /* * * DON'T EDIT BELOW THIS LINE * * */
              (function() {
                var dsq = document.createElement('script');
                dsq.type = 'text/javascript';
                dsq.async = true;
                dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
                (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                  })();
              </script>
              <noscript>Please enable JavaScript to view the
                  <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
              <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>")))
         :exclude "rss.org\\|archive.org\\|theindex.org")
        ("blog-rss"
         :base-directory "~/Projects/blog/posts"
         :base-extension "org"
         :publishing-directory "~/Projects/blog/publish"
         :publishing-function (org-rss-publish-to-rss)
         :html-link-home "http://bastibe.de/"
         :html-link-use-abs-url t
         :exclude ".*"
         :include ("rss.org")
         :with-toc nil
         :section-numbers nil
         :title "Bastis Scratchpad on the Internet")
        ("blog-static"
         :base-directory "~/Projects/blog/static"
         :base-extension "png\\|jpg\\|css"
         :publishing-directory "~/Projects/blog/publish/static"
         :recursive t
         :publishing-function org-publish-attachment)))
