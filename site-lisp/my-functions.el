(defun insert-current-time()
  "insert the current time"
  (interactive "*")
  (insert (format-time-string "%l.%M %p")))

(defmacro my/add-to-hooks (mode &rest hooks)
  "adds the function `mode` to all of the following hooks"
  `(dolist (hook (quote ,hooks))
     (add-hook hook (function ,mode))))

(defun my/copy-whole-buffer ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "buffer copied to clipboard"))

;; byte compile init dir and hook to remove stale elc files
(defun my/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun my/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'my/remove-elc-on-save)

(defun my/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))


;; PROFILING SHITE

(defvar the-test-counter 0)
(defvar the-test-buffer nil)

(defun insert-thing ()
  (when (equal 0 the-test-counter)
    (profiler-start 'cpu+mem))
  (cl-incf the-test-counter)
  (if (< the-test-counter 100)
      (progn
        (run-at-time 0.01 nil #'insert-thing)
        (insert "x"))
    (profiler-report)
    (profiler-stop)))

(defun do-the-test ()
  "Test font lock speed"
  (interactive)
  (setq the-test-counter 0)
  (setq the-test-buffer (current-buffer))
  (run-at-time 0.01 nil #'insert-thing))


;; sudo

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun my/add-to-exec-path (path)
  (let ((expanded (expand-file-name path)))
    (add-to-list 'exec-path expanded)
    (setenv "PATH" (concat (getenv "PATH") ":" expanded))))

(provide 'my-functions)
