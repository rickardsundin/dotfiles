;; No toolbar
(tool-bar-mode nil)

;; No beep
(setq visible-bell t)

;; Mac settings
(setq default-input-method "MacOSX"
      mac-command-modifier 'meta
      mac-option-modifier 'none
      x-select-enable-clipboard t
      mac-allow-anti-aliasing t)

;; Relocate backup files
(setq backup-directory-alist '(("." . "~/.emacs_backups")))

;; Enable Marmalade package archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Always install nREPL
(when (not (package-installed-p 'nrepl)) (package-install 'nrepl))

;; Prevent error buffer in REPL
(setq nrepl-popup-stacktrace nil)
