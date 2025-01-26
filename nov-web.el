;;; nov-web.el --- nov-web - read epub in web -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Damon Chan

;; Author: Damon Chan <elecming@gmail.com>
;; URL: https://github.com/chenyanming/nov-web
;; Keywords: hypermedia, multimedia, epub
;; Created: 25 Jan 2025
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (nov "0.4.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nov-web - read epub in web by injecting the javascript and css into the epub file.

;;; Code:

(require 'nov)
(require 'shr)
(require 'cl-lib)
(require 'evil-core nil 'noerror)

(defcustom nov-web-script-file
  (expand-file-name "nov-web.js" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the JavaScript file containing nov-web functionality."
  :group 'nov-web
  :type 'file)

(defun nov-web-script ()
  "JavaScript scripts used to run in the epub file."
  (with-temp-buffer
    (insert-file-contents nov-web-script-file)
    (buffer-string)))


(defcustom nov-web-style-light (format "
    body {
        writing-mode: horizontal-tb;
        // background: %s !important;
        font-size: 18px !important;
        text-align: left !important;
        width: 90%% !important;
        height: 50%% !important;
        position: absolute !important;
        left: 49%% !important;
        top: 30%% !important;
        transform: translate(-50%%, -55%%) !important;
    }
    p {
        font-size: 1em !important;
        text-align: left !important;
        line-height: 1.3 !important;
        margin-bottom: 25px !important;
    }
    pre, tr, td, div.warning {
        font-size: 1em;
        background: #d8dee9;
    }
    th {
        font-size: 1em;
    }
    span {
        font-size: 18px;
    }
    /* Same font for all tags */
    a, em, caption, th, pre, tr, td, code, h1, h2, h3, h4, h5, h6, p, body {
        font-family: \"Fira Code\", Georgia,Cambria,\"Times New Roman\",Times,serif !important;
    }
    h1 {
        font-size: 2em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    h2 {
        font-size: 1.5em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    h3 {
        font-size: 1.3em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    h4 {
        font-size: 1.2em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    h5 {
        font-size: 1.1em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    h6 {
        font-size: 1em !important;
        color: #2e3440 !important;
        margin-bottom: 10px !important;
    }
    code {
        font-size: 1em !important;
    }
    :root {
        color-scheme: light; /* both supported */
    }

    body img {
        max-width: 100%% !important;
    }
    .programlisting {
        font-size: 20px;
    }
" (face-attribute 'default :background))
  "Light mode CSS style used to render the epub file."
  :group 'nov-web
  :type 'string)


(defcustom nov-web-style-dark (format "
    body {
        writing-mode: horizontal-tb;
        // background: %s !important;
        color: #eee !important;
        font-size: 18px !important;
        text-align: left !important;
        width: 90%% !important;
        height: 50%% !important;
        position: absolute !important;
        left: 49%% !important;
        top: 30%% !important;
        transform: translate(-50%%, -55%%) !important;
    }
    p {
        text-align: left !important;
        line-height: 1.3 !important;
        margin-bottom: 25px !important;
    }
    h1, h2, h3, h4, h5, h6 {
        /*color: #eee !important;*/
        border-bottom: 0px solid #eee !important;
    }
    pre, tr, td, div.warning {
        font-size: 1em;
        background: #272c35;
    }
    th {
        font-size: 1em;
        color: #eee !important;
    }

    span {
        font-size: 18px;
        color: #eee !important;
    }
    h1 {
        color: #ffaf69 !important;
    }
    h2 {
        color: #3fc6b7 !important;
    }
    h3 {
        color: #88d498 !important;
    }
    h4 {
        color: #80c3f0 !important;
    }
    h5 {
        color: #cccccc !important;
    }
    h6 {
        color: #cccccc !important;
    }

    /* Same font for all tags */
    a, em, caption, th, pre, tr, td, code, h1, h2, h3, h4, h5, h6, p, body {
        font-family: \"Fira Code\", Georgia,Cambria,\"Times New Roman\",Times,serif !important;
    }
    code {
        font-size: 1em !important;
    }
    :root {
        color-scheme: dark; /* both supported */
    }

    body, p.title  {
        color: #eee !important;
    }

    body a{
        color: #809fff !important;
    }

    body img {
        max-width: 100%% !important;
        filter: brightness(.8) contrast(1.2);
    }
    .programlisting {
        font-size: 20px;
    }
" (face-attribute 'default :background))
  "Dark mode CSS style used to render the epub file."
  :group 'nov-web
  :type 'string)

(defcustom nov-web-browser-function 'browse-url
  "browser function."
  :group 'nov-web
  :type browse-url--browser-defcustom-type)

(defcustom nov-web-inject-output-dir
  (expand-file-name (concat temporary-file-directory "nov-web/"))
  "The nov-web injected output html directory."
  :group 'nov-web
  :type 'directory)

(defcustom nov-web-unzip-program "unzip"
  "Program used to unzip epub files."
  :group 'nov-web
  :type 'string)

(defcustom nov-web-unzip-args '("-o" filename "-d" directory)
  "Arguments passed to `nov-web-unzip-program'.
The symbols 'filename' and 'directory' are replaced with their
actual values."
  :group 'nov-web
  :type '(repeat (choice string symbol)))

(defvar nov-web-current-file nil)

(defun nov-web-unnest-directory (directory child)
  "Move contents of CHILD into DIRECTORY, then delete CHILD."
  ;; FIXME: this will most certainly fail for con/con
  (dolist (item (nov-directory-files child))
    (rename-file item directory))
  (delete-directory child))

(defun nov-web-contains-nested-directory-p (directory)
  "Non-nil if DIRECTORY contains exactly one directory."
  (let* ((files (nov-directory-files directory))
         (file (car files)))
    (and (= (length files) 1)
         (file-directory-p file)
         file)))


(defun nov-web--fix-permissions (file-or-directory mode)
  (let* ((modes (file-modes file-or-directory))
         (fixed-mode (file-modes-symbolic-to-number mode modes)))
    (set-file-modes file-or-directory fixed-mode)))

(defun nov-web-fix-permissions (directory)
  "Iterate recursively through DIRECTORY to fix its files."
  (nov--fix-permissions directory "+rx")
  (dolist (file (nov-directory-files directory))
    (if (file-directory-p file)
        (nov-web-fix-permissions file)
      (nov-web--fix-permissions file "+r"))))


(defun nov-web-unzip-epub (directory filename)
  "Extract FILENAME into DIRECTORY.
Unnecessary nesting is removed with `nov-web-unnest-directory'."
  (let* ((status (apply #'call-process nov-web-unzip-program nil "*nov-web unzip*" t
                        (mapcar (lambda (arg)
                                  (cond
                                   ((eq arg 'directory) directory)
                                   ((eq arg 'filename) filename)
                                   (t arg)))
                                nov-web-unzip-args)))
         child)
    (while (setq child (nov-web-contains-nested-directory-p directory))
      (nov-web-unnest-directory directory child))
    ;; HACK: unzip preserves file permissions, no matter how silly they
    ;; are, so ensure files and directories are readable
    (nov-web-fix-permissions directory)
    status))

(defun nov-web-fix-file-path (file)
  "Fix the FILE path."
  (format "%s%s.%s"
          (or (file-name-directory file) "")
          (file-name-base file)
          (file-name-extension file)))

(defun nov-web-inject (file &optional callback)
  "Inject `nov-web-script', `nov-web-style-light', or `nov-web-style-dark' into FILE.
Call CALLBACK on the final injected dom.
Input FILE should be  htm/html/xhtml
Output a new html file prefix by _."
  ;; create the nov-web-inject-output-dir if not exists
  (unless (file-exists-p nov-web-inject-output-dir)
    (make-directory nov-web-inject-output-dir t))
  (let* ((native-path file)
         ;; get new path of the final html file
         (output-path (string-replace (concat nov-work-dir "/") nov-web-inject-output-dir file))
         ;; create the html if not esists, insert the `nov-web-script' as the html script
         (dom (with-temp-buffer
                (insert-file-contents native-path)
                (libxml-parse-html-region (point-min) (point-max))))
         (new-dom (let ((dom dom))
                    ;; fix all href and point to the new html file
                    (cl-map 'list (lambda(x)
                                    (let* ((href (dom-attr x 'href))
                                           (new-href (nov-web-fix-file-path href)))
                                      (dom-set-attribute x 'href new-href)))
                            ;; all elements that not start with http or https,
                            ;; but matches htm.*
                            (cl-remove-if
                             (lambda(x)
                               (string-match-p "https?.*"
                                               (dom-attr x 'href)))
                             (dom-elements dom 'href ".*htm.*")))
                    (dom-append-child
                     (dom-by-tag dom 'head)
                     '(meta ((charset . "utf-8"))))
                    (dom-append-child
                     (dom-by-tag dom 'head)
                     `(style nil ,(pcase (frame-parameter nil 'background-mode)
                                    ('light nov-web-style-light)
                                    ('dark nov-web-style-dark)
                                    (_ nov-web-style-light))))
                    (dom-append-child
                     (dom-by-tag dom 'head)
                     `(script nil ,(nov-web-script)))
                    dom)))
    (if callback
        (funcall callback new-dom))
    (with-temp-file output-path
      (shr-dom-print new-dom)
      ;; (encode-coding-region (point-min) (point-max) 'utf-8)
      output-path)))

(defun nov-web-inject-all-files()
  "Inject `nov-web-style-dark', `nov-web-style-light', or
`nov-web-script' to all files in `nov-documents'. It should
be run once after the epub file is opened, so that it can fix all
the href and generate new injected-htmls beforehand. You could
also run it after modifing `nov-web-style-dark',
`nov-web-style-light', or `nov-web-script'."
  (interactive)
  (if nov-documents
      (dolist (document (append nov-documents nil))
        ;; inject all files
        (nov-web-inject (cdr document))
        ;; fix the path
        ;; (setf (cdr document) (nov-web-fix-file-path (cdr document)))
        )))

;;;###autoload
(defun nov-web-find-file (file)
  "Open and prepare FILE for viewing in nov-web.
If FILE is an EPUB, unzip it to `nov-web-inject-output-dir' and go to TOC."
  (interactive "fFile to open: ")
  (when (string-equal (file-name-extension file) "epub")
    (let ((epub-file file))
      (when epub-file
        (find-file epub-file))
      ;; Delete the temp directory
      (delete-directory nov-web-inject-output-dir t)
      ;; Unzip to output directory
      (nov-web-unzip-epub nov-web-inject-output-dir (expand-file-name epub-file))
      ;; inject all files
      (nov-web-inject-all-files)
      ;; Set file to the first HTML file in the unzipped directory
      (setq file (car (directory-files nov-web-inject-output-dir t "\\.html?$"))
            nov-web-current-file file)
      ;; Go to TOC
      (nov-web-goto-toc)
      (kill-current-buffer)))
  file)

(defun nov-web-find-page (file &optional arg new-session)
  "Open a FILE with nov-web."
  (interactive
   (list
    (pcase major-mode
      ('nov-mode
       (cdr (aref nov-documents nov-documents-index)))
      (_
       (read-file-name "Webkit find file: ")))
    current-prefix-arg))
  ;; Prepare file (unzip if EPUB)
  (setq file (nov-web-find-file file))
  ;; every time to open a file, force inject, so that the scripts are reloaded
  (let* ((file (nov-web-inject file)) ;; Inject when open
         ;; get web url of the file
         (path (replace-regexp-in-string
                " "
                "%20"
                (concat
                 "file:///"
                 file)))
         (final-path (if (string-equal (file-name-extension file) "ncx")
                         "about:blank"
                       path)))
    ;; workaround to view in windows
    ;; TODO it is able to support to browse in external browser
    ;; after supporting more advance html/style/scripts
    (funcall nov-web-browser-function final-path)))

(defun nov-web-find-source-file ()
  "Open the source file."
  (interactive)
  (find-file (cdr (aref nov-documents nov-documents-index))))

(defun nov-web-view ()
  "View the current document in web."
  (interactive)
  (let* ((docs nov-documents)
         (index nov-documents-index)
         (toc nov-toc-id)
         (epub nov-epub-version)
         (metadata nov-metadata)
         (file (cdr (aref docs index))))

    ;; open the html file
    (nov-web-find-file file nil t)))

(defun nov-web-next-document ()
  "Go to the next document and render it."
  (interactive)
  (when (< nov-documents-index (1- (length nov-documents)))
    (let* ((docs nov-documents)
           (index (1+ nov-documents-index))
           (toc nov-toc-id)
           (epub nov-epub-version)
           (metadata nov-metadata)
           (path (cdr (aref docs index))))
      (nov-web-find-file path)
      (with-current-buffer (buffer-name)
        (setq-local nov-documents docs)
        (setq-local nov-documents-index index)
        (setq-local nov-toc-id toc)
        (setq-local nov-metadata metadata)
        (setq-local nov-epub-version epub)))))

(defun nov-web-previous-document ()
  "Go to the previous document and render it."
  (interactive)
  (when (> nov-documents-index 0)
    (let* ((docs nov-documents)
           (index (1- nov-documents-index))
           (toc nov-toc-id)
           (epub nov-epub-version)
           (metadata nov-metadata)
           (path (cdr (aref docs index))))
      (if (string-equal (file-name-extension path) "ncx")
          (nov-web-goto-toc)
        (nov-web-find-file path)
        (with-current-buffer (buffer-name)
          (setq-local nov-documents docs)
          (setq-local nov-documents-index index)
          (setq-local nov-toc-id toc)
          (setq-local nov-metadata metadata)
          (setq-local nov-epub-version epub))))))

(defun nov-web-goto-toc ()
  "Go to the TOC index and render the TOC document."
  (interactive)
  (let* ((docs nov-documents)
         (epub nov-epub-version)
         (ncxp (version< nov-epub-version "3.0"))
         (index (nov-find-document (lambda (doc) (eq (car doc) nov-toc-id))))
         (toc nov-toc-id)
         (path (cdr (aref docs index)))
         (html-path (expand-file-name "toc.html" (file-name-directory (string-replace (concat nov-work-dir "/") nov-web-inject-output-dir path))))
         (html (if (file-exists-p html-path)
                   (with-temp-buffer (insert-file-contents html-path) (buffer-string))
                 ;; it could be empty sting
                 (nov-ncx-to-html path)))
         (dom (with-temp-buffer
                (if ncxp
                    (insert html)
                  (insert-file-contents path))
                (libxml-parse-html-region (point-min) (point-max))))
         (new-dom (let ((dom dom))
                    (if dom
                        (dom-add-child-before
                         dom
                         `(head nil
                                (meta ((charset . "utf-8")))
                                (title nil "TOC")
                                (style nil ,(pcase (frame-parameter nil 'background-mode)
                                              ('light nov-web-style-light)
                                              ('dark nov-web-style-dark)
                                              (_ nov-web-style-light)))
                                (script nil ,(nov-web-script)))) )
                    dom))
         (file (with-temp-file html-path
                 (shr-dom-print new-dom)
                 html-path)))
    (when (not index)
      (error "Couldn't locate TOC"))
    (nov-web-find-page file)))

(provide 'nov-web)
;;; nov-web.el ends here
