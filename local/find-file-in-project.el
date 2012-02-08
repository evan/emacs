(require 'find-file-in-project)

(add-to-list 'ffip-patterns "*.scala")

(global-set-key (kbd "C-c C-f") 'find-file-in-project)
