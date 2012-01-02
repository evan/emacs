;; hello, world.

(add-to-list 'default-frame-alist
;;           '(font . "-apple-Helvetica_Neue-medium-normal-normal-*-13-*-*-*-p-0-iso10646-1"))
             '(font . "-apple-Menlo-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1"))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(package-initialize)


;; package manifest

(load (concat dotfiles-dir "manifest-init.el"))
(manifest-install-missing-packages)

;; common libraries

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)


;; If this is Cocoa emacs, gimme back the menu bar.
(if (and (fboundp 'menu-bar-mode) (eq window-system 'ns)) (menu-bar-mode 1))

;; local config bootstrap

(defun concat-path (&rest elems)
  "Concats path segments in a system agnostic manner"
  (if (= 1 (length elems))
    (car elems)
    (concat (file-name-as-directory (car elems)) (apply 'concat-path (cdr elems)))))

(setq local-extras-dir (concat-path dotfiles-dir "local"))

(if (file-exists-p local-extras-dir)
    (mapc (lambda (extra) (load (concat-path local-extras-dir extra)))
          (directory-files local-extras-dir nil ".*el$")))

(load custom-file 'noerror)
