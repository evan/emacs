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

;; If this is Cocoa emacs, gimme back the menu bar.
(if (and (fboundp 'menu-bar-mode) (eq window-system 'ns))
    (menu-bar-mode 1))

;; common libraries

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)


;; local config bootstrap

(setq local-extras-dir (concat dotfiles-dir "local"))
(add-to-list 'load-path local-extras-dir)

(if (file-exists-p local-extras-dir)
    (mapc #'load (directory-files local-extras-dir nil ".*el$")))

(load custom-file 'noerror)
