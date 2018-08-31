;; emacs-running-in-terminal specific crap

(menu-bar-mode -1)

(use-package pbcopy
  :demand
  :config (turn-on-pbcopy))

(provide 'terminal-config)
