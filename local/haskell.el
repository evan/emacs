(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Three indentation alternatives. pick one.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; (global-set-key [(control meta down-mouse-3)] 'imenu)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
