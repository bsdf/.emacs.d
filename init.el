;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; set up load path
(dolist (p '(site-lisp-dir settings-dir))
  (add-to-list 'load-path (symbol-value p)))

;; keep emacs custom settings in separate file
(setq custom-file (expand-file-name (format "env/%s.el" (system-name))
                                    user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; setup package archives
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; and install needed packages if necessary
(unless package-archive-contents
  (package-refresh-contents)
  (mapc #'package-install
        '(use-package bind-key diminish)))

(require 'sane-defaults)
(require 'my-functions)
(require 'package-config)
(require 'key-config)
(require 'macros)
