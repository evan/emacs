(require 'smex)
(smex-initialize)

(m-global-set-key (kbd "M-x") 'smex)
(m-global-set-key (kbd "M-X") 'smex-major-mode-commands)
(m-global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
