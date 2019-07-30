;;; -*- lexical-binding: t -*-

(eval-when-compile
  (when (boundp 'use-package-compute-statistics)
    (setq use-package-compute-statistics nil))
  (require 'use-package)
  (require 'use-package-hook-star))

(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t)

;; uncomment to enable profiling
;; (progn
;;   (setq use-package-compute-statistics t)
;;   (require 'use-package)
;;   (require 'use-package-hook-star))

;; default emac packages

(use-package dired
  :ensure nil
  :diminish dired-omit-mode
  :bind (("C-c C-j" . dired-jump))
  :hook ((dired-load . (lambda () (load "dired-x")))
         (dired-mode . (lambda () (dired-omit-mode t))))
  :init (require 'dired-x)
  :config
  (setq dired-omit-verbose nil))

(use-package eshell
  :commands eshell
  :functions eshell-cmpl-initialize
  :hook (eshell-mode . my-eshell-setup)
  :init
  (defun my-eshell-setup ()
    (require 'em-tramp)
    (add-to-list 'eshell-modules-list 'eshell-tramp)
    (eshell-cmpl-initialize)

    (bind-keys :map eshell-mode-map
               ([remap eshell-pcomplete] . completion-at-point)
               ("C-c M-O" . (lambda () (interactive)
                              (eshell/clear t)
                              (eshell-reset))))
    (setq-local ivy-display-functions-alist
                (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                      ivy-display-functions-alist)))

  :config
  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode))

  (setq password-cache               t
        password-cache-expiry        3600
        ping-program-options         '("-c" "4")
        eshell-prefer-lisp-functions t
        eshell-prefer-lisp-variables t
        eshell-hist-ignoredups       t
        eshell-save-history-on-exit  t
        Man-notify-method            'pushy))

(use-package display-line-numbers
  :hook ((prog-mode nxml-mode) . display-line-numbers-mode)
  :config
  (global-display-line-numbers-mode -1))

(use-package gud
  :commands gdb
  :config
  (setq gdb-show-main t
        gdb-display-io-nopopup t))

(use-package tramp
  :config
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  (defun tramp-clean ()
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections))

  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (setq tramp-default-method        "ssh"
        tramp-persistency-file-name "~/.elpa/tramp"
        password-cache-expiry       600))

(use-package hexl
  :commands (hexl-mode hexl-find-file)
  :config
  (defadvice hexl-save-buffer (around hexl-save-point
                                      activate compile)
    "save the point position when saving in hexl"
    (let ((p (point)))
      ad-do-it
      (goto-char p))))

;; third party packages

(use-package color-theme-sanityinc-tomorrow
  :demand
  :config
  (load-theme 'sanityinc-tomorrow-eighties))

(use-package magit
  :bind (("C-x g" . magit-status))
  :defer 2
  :config
  (global-magit-file-mode +1)
  (setq magit-log-arguments '("--decorate")
        magit-auto-revert-mode t
        git-commit-finish-query-functions nil)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-unpulled-from-upstream)

  (use-package magit-gitflow
    :diminish magit-gitflow-mode
    :hook (magit-mode . turn-on-magit-gitflow)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)))

(use-package ivy
  :demand
  :diminish ivy-mode
  :functions ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume))
  :config
  (ivy-mode 1)
  (setq
   ivy-use-virtual-buffers          t
   enable-recursive-minibuffers     t
   projectile-completion-system     'ivy
   projectile-switch-project-action #'projectile-find-file
   magit-completing-read-function   'ivy-completing-read
   ivy-extra-directories            '()))

(use-package counsel
  :after ivy
  :demand
  :diminish counsel-mode
  :bind (("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c k"   . counsel-ag)
         ("C-x l"   . counsel-locate)
         :map minibuffer-local-map
         ("C-r"     . counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package prescient
  :after (ivy company)
  :config
  (setq prescient-save-file "~/.elpa/prescient-save.el")
  (prescient-persist-mode +1)
  (use-package ivy-prescient
    :config
    (ivy-prescient-mode +1))
  (use-package company-prescient
    :config
    (company-prescient-mode +1)))

;; (use-package helm
;;   :demand
;;   :bind (("C-c h"   . helm-command-prefix)
;;          ("C-x C-j" . helm-M-x)
;;          ("M-x"     . helm-M-x)
;;          ("C-x C-f" . helm-find-files)
;;          ("C-x b"   . helm-mini)
;;          ("C-c k"   . helm-descbinds)

;;          :map helm-map
;;          ("<tab>"   . helm-execute-persistent-action))

;;   :config
;;   (use-package helm-projectile
;;     :bind (("C-c p h" . helm-projectile)))

;;   (use-package helm-ag)
;;   (use-package helm-descbinds)
;;   (use-package helm-flx
;;     :config
;;     (helm-flx-mode +1)
;;     (setq helm-flx-for-helm-find-files t
;;           helm-flx-for-helm-locate     t))

;;   (require 'helm-config)

;;   (setq helm-split-window-inside-p  t
;;         helm-completion-mode-string ""
;;         helm-M-x-fuzzy-match        t
;;         helm-buffers-fuzzy-matching t
;;         helm-recentf-fuzzy-match    t
;;         helm-autoresize-mode        0
;;         helm-adaptive-mode          1
;;         helm-adaptive-history-file  "~/.elpa/helm-adaptive-history"
;;         helm-show-completion-display-function #'helm-default-display-buffer)

;;   ;; advice which doesn't select ./.. by default in find file
;;   (advice-add #'helm-ff-move-to-first-real-candidate :after
;;               (lambda ()
;;                 (let ((it (helm-get-selection)))
;;                   (unless (or (not (stringp it))
;;                               (and (string-match helm-tramp-file-name-regexp it)
;;                                    (not (file-remote-p it nil t)))
;;                               (and (file-exists-p it)
;;                                    (null (helm-ff-dot-file-p it))))
;;                     (while (and (not (helm-end-of-source-p))
;;                                 (helm-ff-dot-file-p (helm-get-selection)))
;;                       (helm-next-line))))))

;;   (helm-mode 1))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq
   projectile-enable-caching    nil
   projectile-mode-line         '(:eval (format " [%s]" (projectile-project-name)))

   ;; projectile-completion-system 'helm
   ;; projectile-switch-project-action 'helm-projectile
   projectile-known-projects-file "~/.elpa/projectile-bookmarks.eld"
   projectile-cache-file          "~/.elpa/projectile.cache")

  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-project-root-files ".projectile")
  (projectile-mode +1))

(use-package ag
  :if (executable-find "ag")
  :defer 5
  ;; :bind (:map projectile-command-map
  ;;        ("a"   . helm-projectile-ag)
  ;;        ("s s" . helm-projectile-ag))
  )

(use-package web-mode
  :mode ("\\.jsx\\'"
         "\\.eex\\'")
  :config
  (setq web-mode-content-types-alist  '(("jsx" . "\\.js[x]?\\'"))
        web-mode-engines-alist        '(("elixir" . "\\.eex\\'"))
        web-mode-code-indent-offset   2
        web-mode-css-indent-offset    2
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset    2
        web-mode-enable-auto-quoting  nil))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode ("\\.eslintrc\\'"
         "\\.json\\'"
         "\\.avsc\\'"
         "\\.avro\\'")
  :config
  (setq json-reformat:indent-width 2
        js-indent-level            2))

(use-package spaceline
  :demand
  :config
  (setq powerline-default-separator   'wave
        spaceline-highlight-face-func #'spaceline-highlight-face-default)
  (require 'my-spaceline-config))

(setq my-lisp-modes
      '(clojure-mode
        emacs-lisp-mode
        lisp-mode
        lisp-interaction-mode
        cider-repl-mode
        sly-mrepl-mode
        sly-mode
        racket-mode
        racket-repl-mode
        ielm-mode))

(use-package smartparens
  :diminish smartparens-mode
  :bind (:map smartparens-mode-map
         ("C-M-k"   . sp-kill-sexp)
         ("C-M-SPC" . sp-mark-sexp))
  :hook* (my-lisp-modes . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (setq-default sp-autoskip-closing-pair 'always))

(use-package rainbow-delimiters
  :hook* (my-lisp-modes . rainbow-delimiters-mode))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :defer 2
  :config
  (use-package yasnippet-snippets
    :after yasnippet)
  (setq yas-verbosity 2)
  (yas-global-mode 1))

(use-package exec-path-from-shell
  :unless (eq system-type 'windows-nt)
  :defer 5
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; (use-package ace-jump-mode
;;   :bind (("C-c SPC" . ace-jump-mode)))

;; (use-package ace-window
;;   :bind (("M-p" . ace-window)))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand)))

(use-package org
  :ensure org-plus-contrib
  :diminish org-indent-mode
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :config
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  (require 'ox-md nil t)
  (require 'ox-gfm nil t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (emacs-lisp . t)))

  (setq org-babel-results-keyword "results")

  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when org-inline-image-overlays
                (org-redisplay-inline-images))))

  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

  (add-hook 'org-mode-hook (lambda ()
                             (add-pcomplete-to-capf)
                             (setq word-wrap t)))


  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/org/gtd/inbox.org" "Tasks")
           "* TODO %i%?")
          ("T" "Tickler" entry
           (file+headline "~/org/gtd/tickler.org" "Tickler")
           "* %i%? \n %U")
          ("P" "process-soon" entry
           (file+headline "todo.org" "Todo")
           "* TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")))

  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '(("~/org/gtd/gtd.org"     :maxlevel . 2)
                             ("~/org/gtd/someday.org" :maxlevel . 2)
                             ("~/org/gtd/tickler.org" :maxlevel . 2)))

  (setq org-agenda-files '("~/org/gtd/gtd.org"
                           "~/org/gtd/inbox.org"
                           "~/org/gtd/tickler.org"))

  (setq org-agenda-custom-commands
        '(;; Contexts
          ("o" "At the office" tags-todo "@office"
           ((org-agenda-overriding-header "Office")))
          ("h" "At home" tags-todo "@home"
           ((org-agenda-overriding-header "Home")))
          ("p" "On the phone" tags-todo "@phone"
           ((org-agenda-overriding-header "Phone Calls")))
          ("s" "Shopping to do" tags-todo "@shopping"
           ((org-agenda-overriding-header "Shopping")))
          ("e" "While running errands" tags-todo "@errands"
           ((org-agenda-overriding-header "Errands")))

          ;; TODO States
          ("w" "Waiting for items" todo "WAITING"
           ((org-agenda-overriding-header "Waiting")))
          ;; Misc
          ))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-startup-indented     t
        org-startup-truncated    nil
        org-special-ctrl-a/e     t
        org-src-fontify-natively t

        org-agenda-block-separator nil

        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        ))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package async
  :defer 2
  :diminish dired-async-mode
  :config
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  (setq async-bytecomp-allowed-packages '(all)))

(use-package editorconfig
  :diminish editorconfig-mode
  :defer 5
  :config
  (editorconfig-mode 1))

(use-package sly
  :commands sly
  :defines sly-mrepl-mode-map
  :functions (sly-mrepl-mode sly-mrepl-clear-repl)
  :config
  (setq
   sly-complete-symbol-function #'sly-simple-completions
   ;; sly-complete-symbol-function #'sly-flex-completions
   ;; inferior-lisp-program "sbcl"
   )

  (load (expand-file-name "~/.roswell/helper.el"))

  (with-eval-after-load 'sly-mrepl
    (bind-key "C-c M-O" #'sly-mrepl-clear-repl sly-mrepl-mode-map)
    (sp-local-pair #'sly-mrepl-mode "'" nil :actions nil)))

(use-package company
  :demand
  :diminish company-mode
  :diminish global-company-mode
  :bind (("C-\\" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :config
  (setq company-backends '(company-elisp company-files company-capf)
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil)
  (global-company-mode))

(use-package flycheck
  :commands flycheck-mode
  :diminish flycheck-mode)

(use-package go-mode
  :mode "\\.go\\'")

(use-package scala-mode
  :mode "\\.scala\\'"
  :config
  (use-package ensime
    :hook (scala-mode . ensime-mode)
    :commands (ensime ensime-mode)
    :config
    (setq scala-indent:indent-value-expression t)))

(use-package qml-mode
  :mode "\\.qml\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (use-package company-ghc
    :init
    (add-to-list 'company-backends 'company-ghc))

  (use-package ghc
    :hook (haskell-mode . ghc-init))

  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t))

(use-package elm-mode
  :mode "\\.elm\\'"
  :config
  (use-package flycheck-elm
    :hook (flycheck-mode . flycheck-elm-setup))

  (add-to-list 'company-backends 'company-elm))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" . cmake-mode))

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :preface
  (defun erlang-shell/send-C-g ()
    (interactive)
    (funcall comint-input-sender
             (get-buffer-process (current-buffer))
             (kbd "C-g")))

  :bind (:map erlang-shell-mode-map
         ("C-c C-g" . erlang-shell/send-C-g))
  :config
  (use-package edts
    :defines ac-mode-map
    :bind (:map ac-mode-map
           ("C-\\" . auto-complete))

    :diminish auto-complete-mode
    :diminish auto-highlight-symbol-mode
    :diminish eproject-mode

    :config
    (add-hook 'erlang-mode-hook
              (lambda ()
                (require 'edts-start)
                (edts-mode)
                ;; (auto-complete-mode)
                ;; (edts-complete-setup)
                (company-mode 0))))

  (let ((man-dir "~/.elpa/edts/doc/20.0"))
    (setq erlang-root-dir man-dir
          edts-man-root   man-dir))

  (setq erlang-electric-commands '(erlang-electric-comma erlang-electric-semicolon erlang-electric-newline)))

;; (use-package meghanada
;;   :config
;;   (add-hook 'java-mode-hook (lambda ()
;;                               (setq c-basic-offset 2)
;;                               (meghanada-mode t))))

(use-package fsharp-mode
  :mode "\\.fs\\'")

(use-package elixir-mode
  :mode ("\\.ex\\'" . elixir-mode)
  :config
  (use-package alchemist
    :hook (elixir-mode . alchemist-mode)
    :config
    (setq alchemist-goto-elixir-source-dir "~/code/elixir/"))

  (use-package flycheck-dogma
    :hook (elixir-mode . flycheck-mode)
    :config
    (eval-after-load 'flycheck '(flycheck-dogma-setup))))

(use-package cider
  :bind (:map cider-repl-mode-map
         ("C-c M-O" . cider-repl-clear-buffer))
  :config
  (setq cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (setq cider-repl-display-help-banner nil)

  ;; (use-package clj-refactor
  ;;   :pin melpa-stable
  ;;   :diminish clj-refactor-mode
  ;;   :config
  ;;   (cljr-add-keybindings-with-prefix "C-c RET")
  ;;   (add-hook 'clojure-mode-hook #'clj-refactor-mode))
  ;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
  )

(use-package lua-mode
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 2))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :hook (purescript-mode . turn-on-purescript-indentation)
  :diminish purescript-indentation-mode
  :config
  (use-package psci
    :diminish inferior-psci-mode
    :hook (purescript-mode . inferior-psci-mode)))

(use-package groovy-mode
  :mode "\\.groovy\\'")

(use-package anaconda-mode
  :diminish 'anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends 'company-anaconda))

  (setq anaconda-mode-installation-directory "~/.elpa/anaconda-mode"))

(use-package tuareg
  :mode ("\\.ml[iylp]?\\'" . tuareg-mode)
  :config
  (electric-indent-mode 0)

  (use-package merlin
    :diminish merlin-mode
    :hook ((tuareg-mode reason-mode) . merlin-mode)
    :config (setq merlin-ac-setup t))

  (use-package utop
    :diminish 'utop-minor-mode
    :hook ((tuareg-mode reason-mode) . utop-minor-mode)
    :config
    (setq utop-edit-command t
          utop-command "opam config exec -- utop -emacs")

    ;; remove buggy-ass utop-company-backend
    (my/add-to-hooks (lambda ()
                       (setq company-backends
                             (remove #'utop-company-backend company-backends)))
                     utop-mode-hook
                     utop-minor-mode))

  (use-package reason-mode
    :commands reason-mode
    :config
    (add-hook 'before-save-hook 'refmt-before-save)
    ;; (add-hook 'reason-mode-hook
    ;;           (lambda ()
    ;;             (add-hook 'before-save-hook 'refmt-before-save)
    ;;             (utop-minor-mode)
    ;;             (merlin-mode)))
    )

  ;; (add-hook 'tuareg-mode-hook
  ;;           (lambda ()
  ;;             (merlin-mode)
  ;;             (utop-minor-mode)
  ;;             (electric-indent-mode 0)))
  )

(use-package rtags
  :hook ((c-mode c++-mode) . rtags-start-process-unless-running)
  :if (executable-find "rdm")
  :config
  (use-package flycheck-rtags
    :after rtags
    :config
    (require 'flycheck-rtags)
    (flycheck-mode))
  (use-package company-rtags
    :after rtags
    :config
    (push 'company-rtags company-backends))

  ;; (use-package modern-cpp-font-lock
  ;;   :diminish modern-c++-font-lock-mode
  ;;   :hook (c++-mode . modern-c++-font-lock-mode))

  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics nil
        rtags-use-bookmarks nil
        rtags-completions-enabled t))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package neotree
  :bind (("<f10>" . neotree-toggle))
  :hook (neotree-mode . (lambda ()
                          (display-line-numbers-mode -1)
                          (visual-line-mode -1)
                          (toggle-truncate-lines 1)))
  :config
  (use-package all-the-icons)
  (setq neo-theme      'icons
        neo-smart-open t))

(use-package unkillable-scratch
  :defer 2
  :config
  (setq unkillable-scratch-behavior 'bury)
  (unkillable-scratch))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2)))

(use-package vlf
  :defer 5
  :commands vlf
  :config
  (require 'vlf-setup))

(use-package lsp-java
  :hook (java-mode . lsp-java-enable)
  :config
  (use-package lsp-ui
    :hook ((lsp-mode  . lsp-ui-mode)
           (java-mode . flycheck-mode))
    :config
    (setq lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable nil
          lsp-ui-doc-enable      nil))
  (setq lsp-inhibit-message t))

(use-package eglot
  :commands eglot)

(use-package diff-hl
  :unless (eq system-type 'windows-nt)
  :config
  (global-diff-hl-mode  +1)
  (diff-hl-flydiff-mode +1))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (let ((cargo-bin (expand-file-name "~/.cargo/bin/")))
    (add-to-list 'exec-path cargo-bin)
    (setenv "PATH" (concat (getenv "PATH") ":" cargo-bin)))

  (use-package racer
    :diminish racer-mode
    :hook ((rust-mode . racer-mode)
           (rust-mode . eldoc-mode))
    :config
    ;; (setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
    )
  (use-package flycheck-rust
    :hook (rust-mode . (lambda ()
                         (flycheck-mode 1)
                         (flycheck-rust-setup)))))

(use-package mmm-mode
  :defer 2
  :config
  (mmm-add-classes
   '((jenkinsfile-yaml
      :submode yaml-mode
      :delimiter-mode nil
      :front "yml\\s-*=\\s-*\"\"\""
      :back "\"\"\"")))
  (mmm-add-mode-ext-class 'groovy-mode "jenkinsfile\\'" 'jenkinsfile-yaml)

  (setq mmm-global-mode 'buffers-with-submode-classes
        mmm-submode-decoration-level 2))

(use-package ripgrep
  :if (executable-find "rg")
  :commands (ripgrep-regexp projectile-ripgrep))

(use-package page-break-lines
  :ensure
  :defer 2
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(provide 'package-config)
