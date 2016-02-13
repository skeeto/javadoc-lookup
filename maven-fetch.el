;;; maven-fetch.el --- Fetch Javadoc artifacts from the Maven repository

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This code adds support to javadoc-lookup by automatically fetching
;; and indexing documentation artifacts from the Maven repository. It
;; requires Maven and the command-line unzip utility installed on your
;; system. If these are not in your $PATH, the variables
;; `maven-program-name' and `unzip-program-name' will need to be set.

;; An artifact is specified by a sequence (list or vector) of three
;; strings: [groupId artifactId version]. For example,

;;     ["org.apache.commons" "commons-math3" "3.0"]

;; In your Emacs initialization file, call `javadoc-add-artifact' with
;; your desired artifacts. They are only downloaded, unzipped, and
;; indexed -- a slow process -- the very first time. Each startup
;; after that, Emacs will work entirely from a cache (fast).

;;; Code:

(require 'cl-lib)
(require 'javadoc-lookup)

(defcustom maven-program-name "mvn"
  "Path to the Maven executable."
  :group 'external)

(defcustom unzip-program-name "unzip"
  "Path to the unzip executable."
  :group 'external)

(defvar maven-fetch-command
  "org.apache.maven.plugins:maven-dependency-plugin:2.6:get"
  "Long command name for the Maven dependency fetch plugin.")

(defun maven-fetch-artifact-jar (artifact)
  "Return the cache filename for ARTIFACT."
  (let ((file (format "%s-%s-javadoc.jar" (elt artifact 1) (elt artifact 2))))
    (expand-file-name file (jdl/dir-truename javadoc-lookup-cache-dir))))

(defun maven-fetch-unpacked-path (artifact)
  "Return the unpacked directory name for ARTIFACT."
  (expand-file-name (format "%s/%s" (elt artifact 0) (elt artifact 1))
                    (jdl/dir-truename javadoc-lookup-cache-dir)))

(cl-defun maven-fetch (artifact)
  "Use maven to fetch ARTIFACT into the cache directory,
returning true on success."
  (let ((jarfile (maven-fetch-artifact-jar artifact)))
    (if (file-exists-p jarfile)
        (format "Artifact %s already downloaded" artifact)
      (let ((artifact-arg "-Dartifact=%s:%s:%s:javadoc"))
        (message "Maven is fetching %s ..." artifact)
        (zerop
         (call-process maven-program-name nil nil nil
                       maven-fetch-command
                       (format "-Ddest=%s" jarfile)
                       (apply #'format artifact-arg
                              (cl-coerce artifact 'list))))))))

(defun maven-fetch-unpack (artifact)
  "Unpack an artifact in the javadoc-lookup cache directory,
returning the destination directory. Throws an error on any failure."
  (let* ((jarfile (maven-fetch-artifact-jar artifact))
         (destdir (maven-fetch-unpacked-path artifact)))
    (if (not (file-exists-p jarfile))
        (error "Could not find artifact jar file: %s." jarfile)
      (if (file-exists-p destdir)
          destdir
        (message "Unpacking %s ..." (file-name-nondirectory jarfile))
        (mkdir destdir t)
        (if (call-process unzip-program-name nil nil nil jarfile "-d" destdir)
            destdir
          (delete-directory destdir t)
          (error "Failed to unpack %s" jarfile))))))

;;;###autoload
(defun javadoc-add-artifacts (&rest artifacts)
  "Add Maven repository artifacts to the javadoc-lookup index.
An artifact is specified by a sequence of three strings:
 [groupId artifactId version]."
  (dolist (artifact artifacts)
    (if (maven-fetch artifact)
        (javadoc-add-roots (maven-fetch-unpack artifact))
      (error "Failed to fetch %s" artifact))))

(provide 'maven-fetch)

;;; maven-fetch.el ends here
