(setq use-package-keywords
      (use-package-list-insert :hook* use-package-keywords :hook))

(defun use-package-normalize/:hook* (name keyword args)
  (let* ((elem  (car args))
         (pairs (if (listp (cdr elem)) elem
                  (list elem)))
         (out   (mapc (lambda (x)
                        (setcar x (symbol-value (car x))))
                      pairs)))
    (use-package-normalize/:hook name :hook out)))

(defalias 'use-package-handler/:hook* 'use-package-handler/:hook)

(provide 'use-package-hook-star)
