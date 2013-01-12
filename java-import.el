;;; java-import.el --- quickly add import statements in Java

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Provides the functions `add-java-import' and `sort-java-imports'.

;;; Code:

(require 'javadoc-lookup)

(defvar jdl/import-regexp "^import "
  "Regular expression for finding import statements.")

(defvar jdl/package-regexp "^package "
  "Regular expression for finding package statements.")

(defun jdl/in-package ()
  "Return t if this source has a package statement."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward-regexp jdl/package-regexp nil t) t)))

(defun jdl/has-import ()
  "Return t if this source has at least one import statement."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward-regexp jdl/import-regexp nil t) t)))

(defun jdl/goto-first-import ()
  "Move cursor to the first import statement."
  (goto-char (point-min))
  (search-forward-regexp jdl/import-regexp)
  (move-beginning-of-line nil)
  (point))

(defun jdl/goto-last-import ()
  "Move cursor to the first import statement."
  (goto-char (point-max))
  (search-backward-regexp jdl/import-regexp)
  (move-end-of-line nil)
  (forward-char)
  (point))

;;;###autoload
(defun sort-java-imports ()
  "Sort the imports in the import section in proper order."
  (interactive)
  (when (jdl/has-import)
    (save-excursion
      (sort-lines nil (jdl/goto-first-import) (jdl/goto-last-import)))))

;;;###autoload
(defun add-java-import ()
  "Insert an import statement at import section at the top of the file."
  (interactive)
  (let ((class (jdl/completing-read)))
    (save-excursion
      (if (jdl/has-import)
          (progn
            (jdl/goto-first-import)
            (insert "import " class ";\n")
            (sort-java-imports))
        (progn
          (goto-char (point-min))
          (when (jdl/in-package)
            (search-forward-regexp jdl/package-regexp)
            (move-end-of-line nil)
            (forward-char)
            (insert "\n"))
          (insert "import " class ";\n"))))))

(provide 'java-import)

;;; java-import.el ends here
