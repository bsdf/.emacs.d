;; no splash screen
(setq inhibit-startup-message t)

;; navigate camelCase with word ops
(progn
  (global-subword-mode 1)
  (diminish 'global-subword-mode)
  (diminish 'subword-mode))

;; enable all commands
(setq disabled-command-function nil)

;; set time zone
(set-time-zone-rule "US/Central")

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; also refresh dired, but be quiet about it
;; (setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; but don't revert buffer list
;; (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; show keystrokes in progress
(setq echo-keystrokes 0.1)

;; delete by moving to trash
(setq delete-by-moving-to-trash t)

;; no shift select
(setq shift-select-mode nil)

;; open compressed files
(auto-compression-mode t)

;; answer y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; remove active region if inserting text
(delete-selection-mode 1)

;; lines should be 80 char instead of 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; never insert tabs
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; store all backups and autosave files in the tmp dir
(setq my-backup-dir "~/.elpa/backup")
(setq backup-directory-alist
      `((".*" . ,my-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-dir t)))
(setq auto-save-list-file-prefix "~/.elpa/auto-save-list/.saves-")

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; show nfo files correctly
(add-to-list 'auto-coding-alist '("\\.nfo\\'" . ibm437))

;; terminal mouse stuff
(xterm-mouse-mode t)
;; (global-set-key [mouse-4] '(lambda ()
;;                              (interactive)
;;                              (scroll-down 1)))
;; (global-set-key [mouse-5] '(lambda ()
;;                              (interactive)
;;                              (scroll-up 1)))
(setq ring-bell-function 'ignore)

;; set frame dimensions
(setq default-frame-alist
      (append default-frame-alist
              '((left   . 610)
                (top    . 75)
                (height . 50)
                (width  . 85))))

;; save recentf in a non-synced location
(setq recentf-save-file "~/.elpa/recentf")

;; diminish thangs
(diminish 'eldoc-mode)
(diminish 'visual-line-mode)
(diminish 'abbrev-mode)

;; set gc threshold higher
;; (setq gc-cons-threshold 20000000)
(setq gc-cons-threshold (* gc-cons-threshold 2))

;; use a less dumbass regex syntax
(setq reb-re-syntax 'string)

;; don't use vc.el on windows-nt
(when (eq system-type 'windows-nt)
  (setq vc-handled-backends nil))

;; don't suggest shorted M-x invocations
(setq extended-command-suggest-shorter nil)

;; apropos should search everything
(setq apropos-do-all t)

;; prefer newer versions of .el code
(setq load-prefer-newer t)

;; mu4e wants UTF-8
(set-language-environment "UTF-8")

;; don't create lockfiles to fuck with syncthing
(setq create-lockfiles nil)

;; unicode font setup
;; (when (member "Symbola" (font-family-list))
;;   (setq use-default-font-for-symbols nil)
;;   (set-fontset-font t 'unicode "Go Mono")
;;   (set-fontset-font t 'unicode "Symbola" nil 'append))

;; compile-command stuff
(setq compile-command "make"
      compilation-read-command nil)

(let ((dir (expand-file-name "~/opt/emacs/src")))
  (dolist (x '(source-directory find-function-C-source-directory))
    (set x dir)))

;; 😄 😱 😸 👸 👽 🙋
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")))


;; long line mitigation
(setq bidi-paragraph-direction 'left-to-right)
(global-so-long-mode 1)

(provide 'sane-defaults)
