;; manifest.el --- install package.el packages from a manifest file

;; Usage:

;; Manifest will install packages via package.el from a manifest file.
;; The manifest file format is a single alist. The keys specify
;; package names, and the values should each be a function provided by
;; the named package.

;; The default manifest path is ~/.emacs.d/package-manifest.el, but
;; can be customized by overriding `manifest-file`.

;; once loaded, call `manifest-install-missing-packages` after
;; `package-initialize`. If any manifest packages are missing, they
;; will be installed.

(require 'package)

(defvar manifest-file "~/.emacs.d/package-manifest.el"
  "Path to package manifest elisp file. Format is ((package-name . package-func) ... )")

(defun foldcar (f acc l)
  (if (null l)
    acc
    (foldcar f (funcall f acc (car l)) (cdr l))))

(defun flatmap (f l)
  (apply 'append (mapcar f l)))

(defun manifest-missing-packages (manifest)
  (flatmap
   (lambda (spec)
     (let ((name (car spec))
           (func (cdr spec)))
       (if (fboundp func) () (list name))))
   manifest))

(defun manifest-all-packages (manifest)
  (mapcar 'car manifest))

(defun manifest-load-manifest-file (manifest-file)
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

    manifest))

(defun manifest-load-manifest ()
  (manifest-load-manifest-file manifest-file))

(defun manifest-install-packages (packages)
  "Installs the given list of packages."
  (unless (null packages)
    (package-refresh-contents)
    (dolist (package packages)
      (package-install package))))

(defun manifest-install-missing-packages ()
  "Installs missing packages if necessary."
  (interactive)
  (message "Checking and/or installing missing packages...")
  (manifest-install-packages (manifest-missing-packages (manifest-load-manifest))))

(defun manifest-update-packages ()
  "Installs all packages."
  (interactive)
  (message "Updating manifest packages...")
  (manifest-install-packages (manifest-all-packages (manifest-load-manifest))))
