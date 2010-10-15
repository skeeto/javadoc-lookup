;;; java-docs.el --- Java documentation Emacs integration

;; Copyright (C) 2010 Christopher Wellons <mosquitopsu@gmail.com>

;; This file is NOT part of GNU Emacs.

;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; This package provides a quick way to look up any Javadoc
;; documentation from Emacs, using your browser to display the
;; information.

;; To install, just drop somewhere on your `load-path' and put this in
;; your .emacs,

;; (require 'java-docs)

;; Give `java-docs' your root Java documentation directories. It will
;; scan and index those directories, exposing them to
;; `java-docs-lookup'. You can provide multiple directories at once,
;; for example,

;;   (java-docs "/usr/share/doc/openjdk-6-jdk/api" "~/src/project/doc")

;; For your convenience, lookups can then be done with C-h j.

;; When viewing docs, a browser is fired off with `browse-url'. You
;; may have to set your `browse-url-browser-function' to make this
;; work properly. For example,

;;   (setq browse-url-browser-function 'browse-url-firefox)

;; I strongly recommend using ido-mode in conjunction with
;; java-docs. It becomes really easy to type in what you're looking
;; for. To set this up properly, make sure ido-mode is enabled before
;; loading java-docs.

;; If you're using a recent enough version of emacs, the indexes will
;; be cached in the directory specified in `java-docs-cache-dir', for
;; the purpose of faster loading. If the documentation is updated,
;; just delete the cache directory and it will be recreated.

;;; Code:

(provide 'java-docs)

(require 'cl)
(require 'ido)

(global-set-key "\C-hj" 'java-docs-lookup)

(defvar java-docs-enable-cache (featurep 'hashtable-print-readable)
  "Enable caching for faster loads.")

(defvar java-docs-compress-cache (executable-find "gzip")
  "Compress the cache using gzip.")

(defvar java-docs-cache-dir "~/.java-docs"
  "Location to store index information.")

(defvar java-docs-index (make-hash-table :test 'equal)
  "Index of documentation for quick lookups.")

(defvar java-docs-class-list nil
  "List of classes in the index.")

(defvar java-docs-completing-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "Function used when performing a minibuffer read.")

(defvar java-docs-full-class nil
  "Use the fully qualified class name when indexing.")

(defvar java-docs-current-root nil
  "Current root being indexed. Used to determine full class name.")

(defun java-docs (&rest dirs)
  "Set the Javadoc search path to DIRS and index them."
  (dolist (java-docs-current-root dirs)
    (java-docs-add java-docs-current-root)))

(defun java-docs-add (dir)
  "Add directory to directory list and either index or fetch the cache."
  (let ((cache-name (concat (md5 dir) (if java-docs-compress-cache ".gz" "")))
	(hash (make-hash-table :test 'equal)))
    (if (and java-docs-enable-cache
	     (file-exists-p (concat java-docs-cache-dir "/" cache-name)))
	(java-docs-load-cache cache-name)
      (java-docs-index dir hash)
      (java-docs-save-cache cache-name hash)
      (java-docs-add-hash hash))))

(defun java-docs-load-cache (cache-name)
  "Load a cache from disk."
  (let ((file (concat java-docs-cache-dir "/" cache-name)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (java-docs-add-hash (read (current-buffer)))
      (kill-buffer))))

(defun java-docs-save-cache (cache-name hash)
  "Save a cache to the disk."
  (when java-docs-enable-cache
    (if (not (file-exists-p java-docs-cache-dir))
	(make-directory java-docs-cache-dir))
    (with-temp-buffer
      (insert (prin1-to-string hash))
      (write-file (concat java-docs-cache-dir "/" cache-name)))))

(defun java-docs-add-hash (hash)
  "Combine HASH into the main index hash."
  (maphash (lambda (key val)
	     (puthash key val java-docs-index)
	     (setq java-docs-class-list (cons key java-docs-class-list)))
	   hash))

(defun java-docs-update-list ()
  "Update the completion list to match the index."
  (setq java-docs-class-list nil)
  (maphash (lambda (key val)
	     (setq java-docs-class-list (cons key java-docs-class-list)))
	   java-docs-index))

(defun java-docs-index (dir hash)
  "Index the documentation in DIR into HASH."
  (let* ((list (directory-files dir t "^[^.]"))
	 (files (remove-if 'file-directory-p list))
	 (dirs (remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (java-docs-add-file file hash))
    (dolist (dir dirs)
      (if (not (string-equal "class-use" (file-name-nondirectory dir)))
	  (java-docs-index dir hash)))))

(defun java-docs-add-file (fullfile hash)
  "Add a file to the index if it looks like a class."
  (let* ((file (file-name-nondirectory fullfile))
	 (ext (file-name-extension fullfile))
	 (class (file-name-sans-extension file))
	 (rel (substring fullfile (1+ (length java-docs-current-root))))
	 (fullclass (substitute ?. ?/ (file-name-sans-extension rel)))
	 (case-fold-search nil))
    (when (and (string-equal ext "html")
	       (string-match "^[A-Z].+" class))
      (puthash class fullfile hash)
      (if java-docs-full-class
	  (puthash fullclass fullfile hash)))))

(defun java-docs-lookup (name)
  "Lookup based on class name."
  (interactive (list (funcall java-docs-completing-function
			      "Class: " java-docs-class-list)))
  (let ((file (gethash name java-docs-index)))
    (if file
	(browse-url file))))
