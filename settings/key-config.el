;; keys
(bind-keys
 ("C-x C-g" . keyboard-quit)
 ("C-c C-g" . keyboard-quit)
 ("C-c C-c" . keyboard-quit)
 ("C-;"     . other-window)
 ("C-<end>" . other-window)
 ("C-M-;"   . other-frame)
 ("C-z"     . nil)
 ("C-x a r" . align-regexp)
 ;; ("C-c h" .   'my/copy-whole-buffer)
 ("C-c s"   . my/swap-windows)
 ([remap dabbrev-expand] . hippie-expand))

(provide 'key-config)
