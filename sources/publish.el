(require 'ox-publish)
(require 'seq)

(setq navigation-data '())

;; Get rid of index.html~ and the like that pop up during generation.
(setq make-backup-files nil)
(setq pdn/root (expand-file-name default-directory))

(setq org-html-divs '((preamble "div" "preamble")
                      (content "main" "content")
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
         :publishing-directory "../"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head "<script async src=\"https://www.googletagmanager.com/gtag/js?id=UA-4023697-2\"></script>
<script type=\"text/javascript\" src=\"../google.js\"></script>
<link rel=\"stylesheet\" href=\"../pdn.css\" type=\"text/css\"/>
<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.8.2/css/all.css' integrity='sha384-oS3vJWv+0UjzBfQzYUhtDYW+Pj2yciDJxpsK1OYPAYjqT085Qq/1cq5FLXAZQ7Ay' crossorigin='anonymous'>"
         :html-preamble pdn/preamble
         ;; :auto-sitemap t
         ;; :sitemap-title "Articles"
         :html-head-include-default-style nil
         )

        ("images"
         :base-directory "./images/"
         :recursive t
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "../images/"
         :publishing-function org-publish-attachment)

        ("fonts"
         :base-directory "./fonts/"
         :base-extension "eot\\|svg\\|ttf\\|woff\\|woff2\\|otf"
         :publishing-directory "../fonts/"
         :publishing-function org-publish-attachment)

        ("styles"
         :base-directory "./styles/"
         :base-extension "css\\|el"
         :publishing-directory "../"
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
          "@@html:<span class='article-date'>@@"
          (org-export-get-date (car data) "%b %d")
          "@@html:</span>@@"))

(defun pdn/append-year-org-links-to-index (article-props)
  (goto-char (point-max))
  (newline)

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
  )

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

      (goto-char (point-max))
      (newline)
      (insert "@@html:<div class='archive'><div id='timeline' class='timeline'>@@\n")
      (dolist (curr-year by-year)
        (pdn/append-year-org-links-to-index curr-year))
      (insert "@@html:</div></div>@@\n")

      (setq navigation-data (reverse navigation-data))

      (write-file (concat pdn/root "articles/index.org"))
      (kill-buffer)
      ))

(setq org-publish-use-timestamps-flag t
      org-publish-timestamp-directory "./"
      org-export-with-section-numbers nil
      org-export-with-smart-quotes t
      org-export-with-author nil
      org-export-with-email nil
      org-export-with-date t
      org-export-with-title t
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
      "<div class='cover'>
<div class='cover-card'>
<div class='author-name'>Boško Ivanišević</div>
<div class='author-job'>Developer, Theoretical Physicist</div>
<div class='author-bio mbm'>Ruby, JavaScript, Elixir, C++ and few more</div>
<nav id='sidenav' clas='nav'>
<ul class='nav-list'>
<li class='nav-item'><a href='%1$s/index.html'>Home</a1> <span>/</span></li>
<li class='nav-item'><a href='%1$s/about.html'>About</a> <span>/</span></li>
</ul>
</nav>
<div class='social-links'>
<ul>
<li><a href='https://twitter.com/boskoivanisevic' class='social-link-item' target='_blank'>
<i class='fab fa-twitter'></i>
</a></li>
<li><a href='https://linkedin.com/in/boskoivanisevic' class='social-link-item' target='_blank'>
<i class='fab fa-linkedin'></i>
</a></li>
<li><a href='https://github.com/bosko' class='social-link-item' target='_blank'>
<i class='fab fa-github'></i>
</a></li>
<li><a href='https://stackoverflow.com/users/1665470/boško-ivanišević' class='social-link-item' target='_blank'>
<i class='fab fa-stack-overflow'></i>
</a></li>
<li><a href='https://dev.to/bosko' class='social-link-item' target='_blank'>
  <i class='fab fa-dev' title='bosko\'s DEV Profile'></i>
</a></li>
</ul>
</div>
</div>
</div>" prefix))))

(defun pdn/publish ()
  "Publishes all articles and creates index page"
  (pdn/create-index-page)
  (org-publish-all))
