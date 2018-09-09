(require 'spaceline-config)

(spaceline-compile
  'mine
  '(((buffer-modified
      buffer-size)
     :face highlight-face
     :priority 100)
    ((buffer-id remote-host)
     :priority 98)
    (major-mode :priority 79)
    (process :when active)
    ((flycheck-error flycheck-warning flycheck-info)
     :when active
     :priority 89)
    (minor-modes :when active
                 :priority 9))
  '((selection-info :priority 95)
    input-method
    ((buffer-encoding-abbrev
      point-position
      line-column)
     :separator " | "
     :priority 96)
    (global :when active)
    (buffer-position :priority 99)
    (hud :priority 99)))

(setq-default mode-line-format '("%e" (:eval (spaceline-ml-mine))))

(provide 'my-spaceline-config)
