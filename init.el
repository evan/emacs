(message "hello, world.")

;; basic initial styling

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(add-to-list 'default-frame-alist
;;           '(font . "-apple-Verdana-medium-normal-normal-*-12-*-*-*-p-0-iso10646-1"))
             '(font . "-apple-Verdana-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist '(left-fringe . 1))
(add-to-list 'default-frame-alist '(right-fringe . 1))

;; basic helper functions and definitions

(defun concat-path (&rest elems)
  "Concats path segments in a system agnostic manner"
  (if (= 1 (length elems))
      (car elems)
    (concat (file-name-as-directory (car elems)) (apply 'concat-path (cdr elems)))))

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq local-lib-dir (concat-path dotfiles-dir "lib"))
(setq local-extras-dir (concat-path dotfiles-dir "local"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; load path etc.

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path local-lib-dir)

;(when (file-exists-p local-extras-dir) (add-to-list 'load-path local-extras-dir))

;; packages and manifest

(require 'package)
(require 'manifest)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(package-initialize)
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

;; If we are not in a shell, extract the PATH. (Here's looking at you, OS X)
(if (not (getenv "TERM_PROGRAM"))
    (let ((shell-path (shell-command-to-string "$SHELL -i -c 'printf $PATH'")))
      (setenv "PATH" shell-path)
      (setq exec-path (delete-dups (append (split-string shell-path path-separator) exec-path)))))

;; do not indicate empty lines
(setq-default indicate-empty-lines nil)

;; Create a minor mode for custom global bindings. Groups these in one
;; place and prevents them from being taken over otherwise.
(defvar m-global-keys-map (make-keymap) "Custom bindings minor mode")

(defun m-global-set-key (key def) (define-key m-global-keys-map key def))

;; local config bootstrap

(when (file-exists-p local-extras-dir)
  (mapc (lambda (extra) (load (concat-path local-extras-dir extra)))
        (directory-files local-extras-dir nil ".*el$")))

(load custom-file 'noerror)

;; Enable global bindings

(define-minor-mode m-global-keys-minor-mode
  "Minor mode that groups all custom global bindings."
  :init-value t
  :lighter ""
  :keymap m-global-keys-map)

(defun m-enable-global-keys () (interactive) (m-global-keys-minor-mode 1))
(defun m-disable-global-keys () (interactive) (m-global-keys-minor-mode 0))

(defun m-prioritize-global-keys ()
  "Attempt to ensure that m-global-keys always has priority."
  (interactive)
  (if (not (eq (car (car minor-mode-map-alist)) 'm-global-keys-minor-mode))
      (let ((mykeys (assq 'm-global-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'm-global-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'minibuffer-setup-hook 'm-disable-global-keys)

(m-enable-global-keys)

(defadvice load (after m-prioritize-global-keys activate)
  (m-prioritize-global-keys))

(put 'narrow-to-region 'disabled nil)

(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
