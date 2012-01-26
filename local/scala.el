(add-to-list 'load-path (concat-path local-extras-dir "scala-ensime/dist/elisp"))

(require 'scala-mode)
(require 'ensime)

(add-hook 'scala-mode-hook (lambda () (local-unset-key [(control tab)])))
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(define-key scala-mode-map (kbd "M-q") 'fill-paragraph)

(defun scala-block-indentation ()
  (+ (current-indentation) scala-mode-indent:step))
