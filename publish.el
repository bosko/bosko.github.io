(require 'ox-publish)

;; Get rid of index.html~ and the like that pop up during generation.
(setq make-backup-files nil)
(setq pdn/root (file-name-directory (buffer-file-name)))

(setq org-html-divs '((content "main" "content")
                      (postamble "footer" "postamble"))
      org-html-postamble nil
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

;; Extract string from string property is done with
;; (substring-no-properties (car (plist-get article-env ':title)))
;; (org-export-get-date argicle-env "%Y-%m-%d")
(defun pdn/article-env (article-file)
  "Returns Org publish environment"
  (with-temp-buffer
    (insert-file-contents article-file)
    (org-export-get-environment)))

(defun pdn/get-article-date (article-file)
  "Returns date of article"
  (with-temp-buffer
    (insert-file-contents article-file)
    (plist-get (org-export-get-environment) ':date)))

(defun pdn/article-date (props)
  (plist-get (car (cdr (car (plist-get (car (cdr props)) :date)))) :year-start)
  )

;; Praviti HTML strukturu sa @@html:<b>@@bold text@@html:</b>@@
(defun pdn/create-index-page ()
    "Returns all subdirectories of articles directory"
    (let* ((content (directory-files (concat pdn/root "articles") t))
           (only-subfolders (seq-filter (lambda(name)
                                          (and
                                           (file-directory-p name)
                                           (not (equal name (concat pdn/root "articles/.")))
                                           (not (equal name (concat pdn/root "articles/.."))))) content))
           (properties (mapcar
                        (lambda(article-dir)
                          `(,article-dir . ,(list (pdn/article-env (concat article-dir "/index.org")))))
                        only-subfolders))
           (by-year (seq-group-by #'pdn/article-date properties)))
      by-year
      ))

;; Timestamps can be used to avoid rebuilding everything.
;; This is useful locally for testing.
;; It won't work on Gitlab when stored in ./: the timestamps file should
;; probably be put inside the public/ directory.  It's not so useful there
;; however since generation is fast enough.
(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./"
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-author nil
      org-export-with-email nil
      org-export-with-date t
      org-export-with-title nil
      org-export-with-tags 'not-in-toc
      org-export-with-toc t)

(defun pdn/preamble (info)
  "Return preamble as a string."
  (let* ((file (plist-get info :input-file))
         (spec (org-html-format-spec info))
         (date (cdr (assq ?T spec)))
         (title (substring-no-properties (car (plist-get info :title))))
         (prefix (file-relative-name (expand-file-name "articles" (expand-file-name pdn/root))
                                     (file-name-directory file))))
    (concat
     (format
      "<nav id=\"sidenav\" <a href=\"%1$s/index.html\">About</a1>
<a href=\"%1$s/articles.html\">Articles</a2>
<a href=\"%1$s/projects/index.html\">Projects</a3>
<a href=\"%1$s/links/index.html\">Links</a4>
<a href=\"%1$s/power-apps/index.html\">Apps</a>
<a href=\"%1$s/atom.xml\">Feed</a></nav>
<h1 class=\"title\">%2$s</h1>
<p class=\"subtitle\">%3$s</p>" prefix title date))))

(defun pdn/publish ()
  "Publishes all articles and creates index page"
  (org-publish-all)
  )
