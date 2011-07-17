;; extra random settings
(setq initial-scratch-message nil)
(setq ring-bell-function (lambda ()))

;; scratch does not start out as elisp
(setq initial-major-mode 'fundamental-mode)

;; tab sanity
(setq standard-indent 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)

;; no trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq show-paren-style 'expression)

;; put autosaves in the same directory as backups
(setq auto-save-list-file-prefix (concat-path dotfiles-dir (file-name-as-directory "backups")))
(setq auto-save-file-name-transforms `((".*" ,(concat-path dotfiles-dir (file-name-as-directory "backups")) t)))

(defun m-ido-complete-or-next ()
  "if there is only one ido match, use it, otherwise select the next"
  (interactive)
  (if (and ido-matches (> (length ido-matches) 1))
      (ido-next-match)
    (ido-complete)))

(add-hook 'ido-setup-hook
	  (lambda ()
	    (define-key ido-completion-map [tab] 'm-ido-complete-or-next)
	    (define-key ido-completion-map [(shift tab)] 'ido-prev-match)))

(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (flet ((one-window-p (&optional nomini all-frames) t)) ad-do-it))
