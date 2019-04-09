(require 'ox-publish)

;; Timestamps can be used to avoid rebuilding everything.
;; This is useful locally for testing.
;; It won't work on Gitlab when stored in ./: the timestamps file should
;; probably be put inside the public/ directory.  It's not so useful there
;; however since generation is fast enough.
(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./"
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-email t
      org-export-with-date t
      org-export-with-tags 'not-in-toc
      org-export-with-toc t)

;; Get rid of index.html~ and the like that pop up during generation.
(setq make-backup-files nil)

(defun pdn/preamble (info)
  "Return preamble as a string."
  (let* ((file (plist-get info :input-file))
         (prefix (file-relative-name (expand-file-name "source" (expand-file-name "./"))
                                     (file-name-directory file))))
    (format
     "<a href=\"%1$s/index.html\">About</a1>
<a href=\"%1$s/articles.html\">Articles</a2>
<a href=\"%1$s/projects/index.html\">Projects</a3>
<a href=\"%1$s/links/index.html\">Links</a4>
<a href=\"%1$s/power-apps/index.html\">Apps</a>
<a href=\"%1$s/atom.xml\">Feed</a>" prefix)))

(setq org-html-divs '((preamble "header" "top")
                      (content "main" "content")
                      (postamble "footer" "postamble"))
      ;; org-html-postamble t
      ;; Use custom preamble function to compute relative links.
      ;; org-html-container-element "section"
      org-html-metadata-timestamp-format "%Y-%m-%d"
      org-html-checkbox-type 'html
      org-html-html5-fancy t
      ;; Use custom .css.  This removes the dependency on `htmlize', but then we
      ;; don't get colored code snippets.
      org-html-htmlize-output-type nil
      org-html-validation-link nil
      org-html-doctype "html5")

(setq org-publish-project-alist
      '(("orgfiles"
         :base-directory "./articles/"
         :recursive t
         :base-extension "org"
         :publishing-directory "./public"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"../pdn.css\" type=\"text/css\"/>"
         :html-preamble pdn/preamble
         ;; :auto-sitemap t
         ;; :sitemap-title "Articles"
         :html-head-include-default-style nil
         )

        ("images"
         :base-directory "./images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "./public/images/"
         :publishing-function org-publish-attachment)

        ("styles"
         :base-directory "./styles/"
         :base-extension "css\\|el"
         :publishing-directory "./public"
         :publishing-function org-publish-attachment)
        ))
