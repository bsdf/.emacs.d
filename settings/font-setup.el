(provide 'font-setup)

;; 日本語 😄 😱 😸 👸 👽 🙋
(defun my/set-fontsets ()
  (mapc
   (lambda (x)
     (if (fontset-name-p x)
         (set-fontset-font "fontset-default"  nil "Noto Sans")))
   '("fontset-startup" "fontset-default" "fontset-standard"))

  (set-fontset-font t 'unicode "Noto Color Emoji")
  (set-fontset-font t 'unicode "Noto Sans" nil 'append)
  (set-fontset-font t 'unicode "Noto Sans Symbols" nil 'append)
  (set-fontset-font t 'unicode "Noto Sans Symbols2" nil 'append)
  (set-fontset-font t 'unicode "Quivira" nil 'append)
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-fontset-font t 'unicode "Unifont" nil 'append)

  (set-fontset-font t 'han "Source Han Mono")
  (set-fontset-font t 'kana "Source Han Mono")
  (set-fontset-font t 'hangul "Source Han Mono")
  (set-fontset-font t 'cjk-misc "Source Han Mono")
  (set-fontset-font t 'egyptian "Noto Sans Egyptian Hieroglyphs"))

(my/set-fontsets)
(add-hook 'server-after-make-frame-hook #'my/set-fontsets)
