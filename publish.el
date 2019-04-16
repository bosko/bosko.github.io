(require 'ox-publish)
(require 'seq)

(setq navigation-data '())

;; Get rid of index.html~ and the like that pop up during generation.
(setq make-backup-files nil)
(setq pdn/root (expand-file-name default-directory))

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
;; (org-export-get-date article-env "%Y-%m-%d")
(defun pdn/article-env (article-file)
  "Returns Org publish environment"
  (with-temp-buffer
    (insert-file-contents article-file)
    (org-export-get-environment)))

(defun pdn/article-year (props)
  (plist-get (car (cdr (car (plist-get (car (cdr props)) :date)))) :year-start)
  )

(defun pdn/compare-by-article-date (first second)
  (string> (org-export-get-date (car (cdr first)) "%Y-%m-%d")
      (org-export-get-date (car (cdr second)) "%Y-%m-%d")))

(defun pdn/month-day-as-html (data)
  (format "%s%s%s"
          "@@html:<span class=\"article-date\">@@"
          (org-export-get-date (car data) "%b %d")
          "@@html:</span>@@"))

(defun pdn/append-year-org-links-to-index (article-props)
  (goto-char (point-max))
  (newline)
  (insert "@@html:<div class=\"timeline\">@@\n")
  (insert (format "* %s\n" (car article-props)))
  (setq sorted (sort (cdr article-props) 'pdn/compare-by-article-date))

  (dolist (article sorted)
    (let* ((art-dir (car article))
           (art-date (plist-get (car (cdr article)) :date))
           (art-year (plist-get (car (cdr (car art-date))) :year-start))
           (art-title (substring-no-properties (car (plist-get (car (cdr article)) :title))))
           (link-data `(dir ,art-dir year ,art-year title ,art-title)))
      (if (= (length navigation-data) 0)
          (setq navigation-data `(,link-data))
        (add-to-list 'navigation-data link-data)
          ))
    (insert (format "+ [[./%s/index.org][%s %s]]\n"
                    (file-name-nondirectory (car article))
                    (pdn/month-day-as-html (cdr article))
                    (substring-no-properties (car (plist-get (car (cdr article)) :title))))))
  (insert "@@html:</div>@@\n"))

(defun pdn/compare-by-year (first second)
  (>= (car first) (car second)))

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
           (by-year (sort (seq-group-by 'pdn/article-year properties) 'pdn/compare-by-year)))

      (set-buffer (generate-new-buffer "index-page.org"))
      (erase-buffer)
      (insert-file-contents (concat pdn/root "/index.org"))

      (setq navigation-data '())

      (dolist (curr-year by-year)
        (pdn/append-year-org-links-to-index curr-year))

      (setq navigation-data (reverse navigation-data))

      (write-file (concat pdn/root "articles/index.org"))
      (kill-buffer)
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
  (pdn/create-index-page)
  (org-publish-all))
