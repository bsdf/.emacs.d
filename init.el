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
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path settings-dir)

;; keep emacs custom settings in separate file
(setq custom-file (expand-file-name (concat "env/" (system-name) ".el")
                                    user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; install use-package if we don't have it
(package-initialize)

(unless package-archive-contents
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
  (mapc #'package-install
        '(use-package bind-key diminish)))

(require 'sane-defaults)
(require 'my-functions)
(require 'package-config)
(require 'key-config)
(require 'macros)
