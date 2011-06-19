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
(setq auto-save-list-file-prefix (concat dotfiles-dir "backups/"))
(setq auto-save-file-name-transforms `((".*" ,(concat dotfiles-dir "backups/") t)))

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

;; speedbar same frame
(require 'speedbar)

(defconst my-speedbar-buffer-name "speedbar")

(defun my-speedbar-no-separate-frame ()
  (interactive)
  (when (not (buffer-live-p speedbar-buffer))
    (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
	  speedbar-frame (selected-frame)
	  dframe-attached-frame (selected-frame)
	  speedbar-select-frame-method 'attached
	  speedbar-verbosity-level 0
	  speedbar-last-selected-file nil)
    (set-buffer speedbar-buffer)
    (speedbar-mode)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer 1)
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
	      (lambda () (when (eq (current-buffer) speedbar-buffer)
			   (setq speedbar-frame nil
				 dframe-attached-frame nil
				 speedbar-buffer nil)
			   (speedbar-set-timer nil)))))
  (set-window-buffer (selected-window)
		     (get-buffer my-speedbar-buffer-name)))
