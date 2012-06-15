;; echo keystrokes more quickly than after 1 second
(setq echo-keystrokes 0.2)

;; swap command and option (to meta and super)
;; use option key for all mac shortcuts, e.g. ⌘Q -> ⌥Q
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)
(setq ns-function-modifier 'control)

;; CUA
(cua-mode t)
(setq cua-keep-region-after-copy t)
(transient-mark-mode 1)

;; Creating a minor mode that owns global bindings prevents these keys
;; from being taken over by others
(defvar m-global-keys-mode (make-keymap) "Custom bindings minor mode")

;; C-g clears the current region, but typing C-c C-g in quick
;; succession doesn't work. Define C-c C-g to work correctly
(defun local--copy-region-and-deselect ()
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end))
  (keyboard-quit))
(m-global-set-key (kbd "C-c C-g") 'local--copy-region-and-deselect)

;; because I stupidly hit ⌘-C (meta-c in my setup) all the time, which
;; default is bound to capitalize-word, which is the source of all
;; sorts of wonderful typo bugs
(global-unset-key [(meta c)])
(m-global-set-key [(meta C)] 'capitalize-word)

;; Control tab instead of C-x b
(m-global-set-key [(control tab)]  'ido-switch-buffer)

;; Bind text-scale-increase to C-= to have parity with
;; text-scale-decrease
(m-global-set-key [(control =)] 'text-scale-increase)

;; make meta forward-delete an alias for meta d
(m-global-set-key [(meta kp-delete)] [(meta d)])

;; moving windows
(m-global-set-key [(meta down)] 'windmove-down)
(m-global-set-key [(meta up)] 'windmove-up)
(m-global-set-key [(meta left)] 'windmove-left)
(m-global-set-key [(meta right)] 'windmove-right)
;;(m-global-set-key [(meta \`)] 'other-frame)
(m-global-set-key [(meta \`)] 'next-multiframe-window)
(m-global-set-key [(super \`)] 'next-multiframe-window)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (message "ran")
            (local-set-key [(meta down)] 'windmove-down)
            (local-set-key [(meta up)] 'windmove-up)))

;; widow proportions
(m-global-set-key [(shift meta down)] 'shrink-window)
(m-global-set-key [(shift meta up)] 'enlarge-window)
(m-global-set-key [(shift meta left)] 'shrink-window-horizontally)
(m-global-set-key [(shift meta right)] 'enlarge-window-horizontally)

;; mouse scroll behavior
(setq mouse-wheel-scroll-amount '(2 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; fullscreen mode
(if (fboundp 'ns-toggle-fullscreen)
    (m-global-set-key [(shift super f)] 'ns-toggle-fullscreen))
