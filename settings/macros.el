;; swagger macroes
(fset 'macro-swagger-keys
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("[1;5Fname\" \"w[1;5F:description: [1;5Fdoc\" w[1;5F''type: string		" 0 "%3d")) arg)))
(fset 'macro-swagger-enum
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("[1;5F\" \"w[1;5F- [1;5F[1;5F" 0 "%3d")) arg)))

(provide 'macros)
