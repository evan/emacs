;; manifest.el --- install package.el packages from a manifest file

;; Usage:

;; Manifest will install packages via package.el from a manifest file.
;; The manifest file format is a single alist. The keys specify
;; package names, and the values specify the target version. Since
;; package.el only serves the latest package version (currently
;; unused).

;; The default manifest path is ~/.emacs.d/package-manifest.el, but
;; can be customized by overriding `manifest-file`.

;; once loaded, call `manifest-install-missing-packages` after
;; `package-initialize`. If any manifest packages are missing, they
;; will be installed.

(require 'cl)
(require 'package)

(defvar manifest-file "~/.emacs.d/package-manifest.el"
  "Path to package manifest elisp file. Format is ((package-name . package-func) ... )")

(defvar manifest-package-alist nil
  "Package manifest alist")

(defvar manifest-missing-packages nil
  "Packages from the manifest that are not installed.")

(defvar manifest-existing-packages nil
  "Packages from the manifest that are installed.")

(defvar manifest-untracked-packages nil
  "Installed packages that do not appear in the manifest")

(defun manifest--set-partition (a b)
  (loop with right = b
        for i in a
          if (member i b) collect i into union
          else collect i into left
          do (setq right (remove i right))
          finally return (list left union right)))

(defun manifest-load ()
  (package-load-all-descriptors)
  (manifest-load-manifest-file)
  (let ((parts (manifest--set-partition
                (mapcar 'car manifest-package-alist)
                (mapcar 'car package-alist))))
    (setq manifest-missing-packages (car parts))
    (setq manifest-existing-packages (cadr parts))
    (setq manifest-untracked-packages (caddr parts))))

(defun manifest-load-manifest-file ()
  (let ((file (expand-file-name manifest-file))
        (manifest ())
        buf)

    (when (file-readable-p file)
      (setq buf (get-buffer-create " *manifest session*"))

      (unwind-protect
          (with-current-buffer buf
            (erase-buffer)
            (insert-file-contents file)
            (condition-case nil
                (setq manifest (read (current-buffer)))
              (error nil)))
        (kill-buffer buf)))

    (when (null manifest)
      (if (file-readable-p file)
        (message "Failed to load manifest. Invalid format.")
        (message (format "Failed to load manifest. '%s' is not readable." manifest-file))))

    (setq manifest-package-alist manifest)))

(defun manifest-install-packages (packages)
  "Installs the given list of packages."
  (unless (null packages)
    (package-refresh-contents)
    (dolist (package packages)
      (package-install package))))

(defun manifest-install-missing-packages ()
  "Installs missing packages if necessary."
  (interactive)
  (manifest-load)
  (message "Checking and/or installing missing packages...")
  (manifest-install-packages manifest-missing-packages)
  (message "Missing packages installed."))

(defun manifest-update-packages ()
  "Installs all packages."
  (interactive)
  (manifest-load)
  (message "Updating manifest packages...")
  (manifest-install-packages (mapcar 'car manifest-package-alist))
  (message "Packages updated."))
