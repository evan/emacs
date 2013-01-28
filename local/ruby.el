(defun ruby-beautify-buffer ()
  (interactive)
  (let (rb)
    (setq rb (buffer-string))

    (with-temp-buffer
      (insert rb)
      (call-process-region (point-min) (point-max) "rbeautify" t t)
      (setq rb (buffer-string)))

    (erase-buffer)
    (insert rb)))

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-c C-v f") 'ruby-beautify-buffer))
