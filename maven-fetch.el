(require 'cl)
(require 'javadoc-lookup)

(defvar maven-program-name "mvn"
  "Path to the Maven executable.")

(defvar unzip-program-name "unzip"
  "Path to the unzip executable.")

(defvar maven-fetch-command
  "org.apache.maven.plugins:maven-dependency-plugin:2.6:get"
  "Long command name for the Maven dependency fetch plugin.")

(defun maven-fetch-artifact-jar (artifact)
  "Return the cache filename for ARTIFACT."
  (expand-file-name (apply #'format "%s-%s-javadoc.jar" (cdr artifact))
                    (jdl/dir-truename javadoc-lookup-cache-dir)))

(defun maven-fetch-unpacked-path (artifact)
  "Return the unpacked directory name for ARTIFACT."
  (expand-file-name (apply #'format "%s/%s" (butlast artifact))
                    (jdl/dir-truename javadoc-lookup-cache-dir)))

(defun* maven-fetch (artifact)
  "Use maven to fetch ARTIFACT into the cache directory,
returning true on success. An artifact is a list of three
strings: (groupId artifactId version)."
  (let ((jarfile (maven-fetch-artifact-jar artifact)))
    (if (file-exists-p jarfile)
        (message "Artifact %S already downloaded" artifact)
      (let ((artifact-arg "-Dartifact=%s:%s:%s:javadoc"))
        (message "Maven is fetching %S ..." artifact)
        (zerop
         (call-process maven-program-name nil nil nil
                       maven-fetch-command
                       (format "-Ddest=%s" jarfile)
                       (apply #'format artifact-arg artifact)))))))

(defun maven-fetch-unpack (artifact)
  "Unpack an artifact in the javadoc-lookip cache directory,
returning the destination directory. See `maven-fetch'."
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

(defun javadoc-lookup-maven-add (&rest artifacts)
  (dolist (artifact artifacts)
    (if (maven-fetch artifact)
        (javadoc-add (maven-fetch-unpack artifact))
      (error "Failed to fetch %S" artifact))))

;(javadoc-lookup-maven-add '("org.xerial" "sqlite-jdbc" "3.7.2"))
