;;; -*- lexical-binding: t -*-

(eval-when-compile
  (when (boundp 'use-package-compute-statistics)
    (setq use-package-compute-statistics nil))
  (require 'use-package)
  (require 'use-package-hook-star))

(require 'diminish)
(require 'bind-key)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package quelpa-use-package)

;; uncomment to enable profiling
;; (progn
;;   (setq use-package-compute-statistics t)
;;   (require 'use-package)
;;   (require 'use-package-hook-star))

;; default emac packages

(use-package dired
  :ensure nil
  :diminish dired-omit-mode
  :bind (("C-c C-j" . dired-jump)
         :map dired-mode-map
         ("C-c C-w" . wdired-change-to-wdired-mode))
  :hook (dired-mode . (lambda () (dired-omit-mode nil)))
  :init 
  :config
  (require 'dired-x)
  (setq dired-omit-verbose nil
        dired-listing-switches "-alh"))

(use-package eshell
  :commands eshell
  :functions (eshell-cmpl-initialize eshell-reset eshell/clear)
  :hook (eshell-mode . my-eshell-setup)
  :bind (:map minibuffer-local-map
              ([remap eshell-pcomplete] . completion-at-point)
              :map eshell-mode-map
              ("C-c M-O" . (lambda () (interactive)
                             (eshell/clear t)
                             (eshell-reset))))
  :init
  (defun my-eshell-setup ()
    (require 'em-tramp)

    ;; (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)
    
    (add-to-list 'eshell-modules-list 'eshell-tramp)
    ;; (eshell-cmpl-initialize)
    (setq-local pcomplete-ignore-case t)
    ;; (setq-local ivy-display-functions-alist
    ;;             (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
    ;;                   ivy-display-functions-alist))
    )

  :config
  (setq password-cache               t
        password-cache-expiry        3600
        ping-program-options         '("-c" "4")
        eshell-prefer-lisp-functions t
        eshell-prefer-lisp-variables t
        eshell-hist-ignoredups       t
        eshell-save-history-on-exit  t))

(use-package man
  :config
  (setq Man-notify-method 'pushy
        Man-width-max      100))

;; (use-package esh-autosuggest
;;   :hook (eshell-mode . esh-autosuggest-mode))

(use-package display-line-numbers
  :hook ((prog-mode nxml-mode conf-mode) . display-line-numbers-mode)
  :config
  (global-display-line-numbers-mode -1))

(use-package gud
  :commands gdb
  :bind (:map gud-mode-map
              ("<tab>" . company-complete))
  :config
  (setq gdb-show-main t
        gdb-display-io-nopopup t))

(use-package tramp
  :defer 5
  :config
  ;; save tramp backups locally
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "~/.elpa/backup"))

  ;; but not su/sudo files
  (setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not
              (let ((method (file-remote-p name 'method)))
                (when (stringp method)
                  (member method '("su" "sudo"))))))))

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

(use-package conf-mode
  :config (unbind-key "C-c C-j" conf-mode-map))

(use-package lisp-interaction-mode
  :ensure nil
  :hook (lisp-interaction-mode . eldoc-mode))

(use-package cc-mode
  :ensure nil
  :bind (:map c++-mode-map
              ("C-c C-o" . ff-find-other-file))
  :config
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-cont-nonempty '++))

(use-package windmove
  :config
  (setq windmove-wrap-around nil)
  (windmove-default-keybindings))

(use-package uniquify
  :defer 1
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

(use-package midnight :defer 5)

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
                          'magit-insert-unpulled-from-upstream))

(use-package magit-gitflow
  :after magit
  :diminish magit-gitflow-mode
  :hook (magit-mode . turn-on-magit-gitflow))

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
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil))

(use-package prescient
  :after (ivy company)
  :config
  (setq prescient-save-file "~/.elpa/prescient-save.el")
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after prescient
  :config (ivy-prescient-mode +1))

;; (use-package company-prescient
;;   :disable
;;   :after prescient
;;   :config (company-prescient-mode +1))

(use-package counsel-projectile
  :after (ivy counsel projectile)
  :config
  (setq counsel-projectile-sort-files t)
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-projectile-find-file . ivy-prescient-sort-function)))

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq
   projectile-enable-caching    nil
   projectile-mode-line         '(:eval (format " [%s]" (projectile-project-name)))
   projectile-sort-order        'recently-active

   projectile-known-projects-file "~/.elpa/projectile-bookmarks.eld"
   projectile-cache-file          "~/.elpa/projectile.cache")

  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-project-root-files ".projectile")
  (projectile-mode +1))

(use-package ag
  :if (executable-find "ag")
  :defer 5)

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
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

(use-package prettier-js
  :bind (:map js2-mode-map
              ("C-c s" . prettier-js))
  :after js2-mode)

(use-package indium
  :commands (indium-launch))

(use-package tern
  :commands (tern-mode)
  :after js2-mode)

;; (use-package company-tern
;;   :ensure nil
;;   :quelpa (company-tern :fetcher github :repo "emacsmirror/company-tern")
;;   :hook (tern-mode
;;          . (lambda ()(add-to-list 'company-backends 'company-tern))))

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
  (setq yas-verbosity 2)
  (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

(use-package exec-path-from-shell
  :unless (eq system-type 'windows-nt)
  :defer 5
  :demand t
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

(use-package doct
  :commands (doct))

(use-package org
  :ensure org-plus-contrib
  :diminish org-indent-mode
  :functions (org-redisplay-inline-images)
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

        org-hidden-keywords '(title)
        org-babel-results-keyword "results"
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

(use-package sly
  :commands sly
  :defines sly-mrepl-mode-map
  :functions (sly-mrepl-mode sly-mrepl-clear-repl sly-simple-completions)
  :config
  (if (executable-find "rosconfig")
      (let ((helper (expand-file-name "~/.roswell/helper.el")))
        (when (file-exists-p helper) (load helper))))
  
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
  :ensure nil
  :commands flycheck-mode
  :diminish flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(python-pylint c/c++-clang c/c++-cppcheck c/c++-gcc)))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.20))))

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (my/add-to-exec-path "~/go/bin"))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :hook (scala-mode . lsp))

(use-package qml-mode
  :mode "\\.qml\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t))

(use-package company-ghc
  :after haskell-mode
  :init
  (add-to-list 'company-backends 'company-ghc))

(use-package ghc
  :hook (haskell-mode . ghc-init))

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

(use-package fsharp-mode
  :mode "\\.fs\\'")

(use-package elixir-mode
  :mode ("\\.ex\\'" . elixir-mode))

(use-package cider
  :bind (:map cider-repl-mode-map
         ("C-c M-O" . cider-repl-clear-buffer))
  :config
  (setq cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (setq cider-repl-display-help-banner nil))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (setq alchemist-goto-elixir-source-dir "~/code/elixir/"))

(use-package flycheck-dogma
  :hook (elixir-mode . flycheck-mode)
  :config
  (eval-after-load 'flycheck '(flycheck-dogma-setup)))

(use-package lua-mode
  :mode ("\\.lua\\'"
         "\\.conkyrc\\'")
  :config
  (use-package company-lua
    :after lua-mode
    (push 'company-lua company-backends))
  (setq lua-indent-level 2
        lua-documentation-url "http://www.lua.org/manual/5.3/manual.html"))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :hook (purescript-mode . turn-on-purescript-indentation)
  :diminish purescript-indentation-mode)

(use-package psci
  :diminish inferior-psci-mode
  :hook (purescript-mode . inferior-psci-mode))

(use-package groovy-mode
  :mode "\\.groovy\\'")

(use-package python-mode
  :hook (python-mode . flycheck-mode)
  :mode "\\.py\\(de\\)?\\'")

;; (use-package pyenv-mode
;;   :hook python-mode
;;   :init
;;   (my/add-to-exec-path "~/.pyenv/bin"))

(use-package blacken
  :bind (:map python-mode-map
              ("C-c s" . blacken-buffer)))

(use-package tuareg
  :mode ("\\.ml[iyp]?\\'" . tuareg-mode)
  :hook (tuareg-mode . flycheck-mode)
  :bind (:map tuareg-mode-map
         ("C-c s" . ocamlformat))
  :custom-face
  (merlin-type-face ((t (:background "#515151"))))
  (tuareg-font-double-semicolon-face ((t (:inherit tuareg-font-lock-operator-face))))
  (tuareg-font-lock-extension-node-face ((t (:inherit 'tuareg-font-lock-attribute-face))))
  :init (require 'opam-user-setup)
  :config (require 'ocamlformat))

(use-package reason-mode
  :mode "\\.re\\'"
  :commands reason-mode
  :config
  (setq refmt-command 'opam)
  (setq utop-command "opam config exec -- rtop -emacs"))

(use-package merlin
  :hook ((reason-mode tuareg-mode) . merlin-mode)
  :diminish merlin-mode
  :custom-face
  (merlin-type-face ((t (:background "#515151"))))
  :config
  (setq merlin-command 'opam))

(use-package merlin-eldoc
  :hook ((reason-mode tuareg-mode) . merlin-eldoc-setup)
  :requires merlin
  :custom-face
  (merlin-eldoc-occurrences-face ((t (:background "#515151"))))
  :config
  (setq merlin-eldoc-type-verbosity 'min
        merlin-eldoc-doc nil
        merlin-eldoc-function-arguments t))

(use-package flycheck-ocaml
  :after flycheck
  :functions (flycheck-define-generic-checker flycheck-ocaml-merlin-start flycheck-verify-ocaml-merlin)
  :config
  (setq merlin-error-after-save nil)
  (flycheck-define-generic-checker 'ocaml-merlin-reason
    "A syntax checker for Reason using Merlin Mode."
    :start #'flycheck-ocaml-merlin-start
    :verify #'flycheck-verify-ocaml-merlin
    :modes '(caml-mode tuareg-mode reason-mode)
    :predicate (lambda ()
                 (and merlin-mode (not merlin-error-after-save))))
  (add-to-list 'flycheck-checkers 'ocaml-merlin-reason))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package neotree
  :bind (("<f12>" . neotree-toggle))
  :hook (neotree-mode . (lambda ()
                          (display-line-numbers-mode -1)
                          (visual-line-mode -1)
                          (toggle-truncate-lines 1)))
  :config
  (setq neo-theme      'icons
        neo-smart-open t))

(use-package all-the-icons)

(use-package unkillable-scratch
  :defer 2
  :config
  (setq unkillable-scratch-behavior 'bury)
  (unkillable-scratch))

(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  :config (setq cd2/region-command 'cd2/comment-or-uncomment-region))

(use-package vlf
  :defer 5
  :commands vlf
  :config
  (require 'vlf-setup))

(use-package lsp-java
  :hook (java-mode . lsp-java-enable)
  :config
  (setq lsp-inhibit-message t))

(use-package eglot
  :commands eglot)

(use-package diff-hl
  :defer 5
  :unless (eq system-type 'windows-nt)
  :config
  (global-diff-hl-mode  +1)
  (diff-hl-flydiff-mode +1))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (my/add-to-exec-path "~/.cargo/bin"))

(use-package racer
  :after rust-mode
  :diminish racer-mode
  :hook ((rust-mode . racer-mode)
         (rust-mode . eldoc-mode)))

(use-package flycheck-rust
  :after rust-mode
  :hook (rust-mode . (lambda ()
                       (flycheck-mode 1)
                       (flycheck-rust-setup))))

;; (use-package mmm-mode
;;   :diminish mmm-mode
;;   :defer 2
;;   :commands mmm-mode
;;   :config
;;   (mmm-add-classes
;;    '((jenkinsfile-yaml
;;       :submode yaml-mode
;;       :delimiter-mode nil
;;       :front "yml\\s-*=\\s-*\"\"\""
;;       :back "\"\"\"")

;;      (c++-glsl
;;       :submode glsl-mode
;;       :front "R\"glsl(\n?"
;;       :back ")glsl\""
;;       )))

;;   (add-to-list 'mmm-c-derived-modes 'glsl-mode)

;;   (mmm-add-mode-ext-class 'groovy-mode "jenkinsfile\\'" 'jenkinsfile-yaml)
;;   (mmm-add-mode-ext-class 'c++-mode nil 'c++-glsl)

;;   (setq mmm-global-mode 'buffers-with-submode-classes
;;         mmm-submode-decoration-level 2))

(use-package ripgrep
  :if (executable-find "rg")
  :commands (ripgrep-regexp projectile-ripgrep))

(use-package page-break-lines
  :defer 2
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package lsp-mode
  :commands lsp
  ;; :bind (:map c++-mode-map
  ;;             ("C-c C-l C-s" . lsp))
  :config
  (ggtags-mode -1)
  (setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook lsp-mode
  :bind (:map lsp-ui-mode-map
              ("M-." . lsp-ui-peek-find-definitions))
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-modeline-workspace-status-enable))

(use-package dap-mode
  :commands dap-mode
  :custom
  (dap-lldb-debug-program '("/usr/bin/lldb-vscode-9"))
  :config
  (require 'dap-lldb))

(use-package company-lsp
  :hook lsp-mode)

(use-package ccls
  :mode "\\.(c(pp|c)?|h(h|pp)?)\\'"
  :config
  ;; (setq lsp-disabled-clients '(ccls))
  (setq ccls-executable "ccls"))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package arduino-mode
  :mode "\\.ino\\'")

(use-package csharp-mode
  :mode "\\.cs\\'")

(use-package multiple-cursors
  :bind (("C-c m c" . mc/edit-lines)))

;; (use-package unicode-fonts
;;   :config
;;   ;; (setq unicode-fonts-block-font-mapping
;;   ;;       '(("Emoticons"
;;   ;;          ("Noto Color Emoji" "Symbola" "Quivira")))
;;   ;;       unicode-fonts-fontset-names '("fontset-default"))
;;   (unicode-fonts-setup))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; (use-package ggtags
;;   :hook (c-mode-common
;;          . (lambda ()
;;              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                (ggtags-mode 1)))))

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

(use-package sqlformat
  :if (executable-find "pg_format")
  :hook sql-mode
  :bind (:map sql-mode-map
              ("C-c C-f" . sqlformat))
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-f2")))

(use-package keychain-environment
  :demand t
  :config (keychain-refresh-environment))

(use-package xterm-color
  :after eshell
  :config
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  (setenv "TERM" "xterm-256color"))

(use-package tramp-theme
  :demand t
  :config (load-theme 'tramp))

(use-package fish-completion
  :config)

;; (use-package paradox
;;   :defer 1
;;   :custom
;;   (paradox-column-width-package 27)
;;   (paradox-column-width-version 13)
;;   (paradox-execute-asynchronously t)
;;   (paradox-hide-wiki-packages t)
;;   :config
;;   (paradox-enable))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))

;; (use-package selectrum
;;   :ensure t
;;   :demand t
;;   :bind (:map selectrum-minibuffer-map
;;               ("C-j"   . selectrum-insert-current-candidate)
;;               ("C-M-j" . selectrum-submit-exact-input))
;;   :custom-face
;;   (selectrum-current-candidate ((t (:background "#515151"))))
;;   :config
;;   (selectrum-mode +1)
;;   (setq projectile-completion-system 'default
;;         magit-completing-read-function 'selectrum-completing-read))

;; (use-package selectrum-prescient
;;   :after selectrum
;;   :config
;;   (selectrum-prescient-mode +1)
;;   (prescient-persist-mode +1))

(use-package gcmh
  :ensure t
  :demand t
  :diminish gcmh-mode
  :config (gcmh-mode 1))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package elpher
  :bind (:map elpher-mode-map
              ("n" . elpher-next-link)
              ("p" . elpher-prev-link)
              ("`" . elpher-prev-link)
              ("l" . recenter-top-bottom))
  :config
  (setq elpher-gemini-link-string "=> "
        elpher-gemini-bullet-string "*"))

(use-package gemini-mode
  :mode "\\.gmi\\'")

(provide 'package-config)
