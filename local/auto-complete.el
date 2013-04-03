(require 'auto-complete)

(add-to-list 'ac-dictionary-directories (concat-path dotfiles-dir "auto-complete.dict"))

(require 'auto-complete-config)
(ac-config-default)
