;(java-docs-dirs "~/src/chess/doc/javadoc" "/usr/share/doc/openjdk-6-jdk/api")

(setq java-docs-enable-cache nil)

;;; Code

(provide 'java-docs)

(global-set-key "\C-hj" 'java-docs-lookup)

(defvar java-docs-enable-cache t
  "Enable caching for faster loads.")

(defvar java-docs-cache-dir "~/.java-docs"
  "Location to store index information.")

(defvar java-docs-dirs nil
  "List of directories containing documentation. These
directories are indexed, so do not edit this list directly.")

(defvar java-docs-index (make-hash-table :test 'equal)
  "Index of documentation for quick lookups.")

(defvar java-docs-class-list nil
  "List of classes in the index.")

(defun java-docs-dirs (&rest dirs)
  "Set the Javadoc search path to DIRS and index them."
  (dolist (dir dirs)
    (java-docs-add dir)))

(defun java-docs-add (dir)
  "Add directory to directory list and either index or fetch the cache."
  (let ((cache-name (md5 dir))
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
  (if (not (file-exists-p java-docs-cache-dir))
      (make-directory java-docs-cache-dir))
  (with-temp-buffer
    (insert (prin1-to-string hash))
    (write-file (concat java-docs-cache-dir "/" cache-name))))

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
	 (case-fold-search nil))
    (if (and (string-equal ext "html")
	     (string-match "^[A-Z].+" class))
	(puthash class fullfile hash))))

(defun java-docs-lookup (name)
  "Lookup based on class name."
  (interactive (list (ido-completing-read "Class: " java-docs-class-list)))
  (let ((file (gethash name java-docs-index)))
    (if file
	(browse-url file))))
