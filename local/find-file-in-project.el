(require 'find-file-in-project)

(add-to-list 'ffip-patterns "*.scala")
(add-to-list 'ffip-patterns "*.thrift")

(global-set-key (kbd "C-c C-f") 'find-file-in-project)
