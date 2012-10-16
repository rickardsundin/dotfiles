(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ns-command-modifier (quote meta))
 '(tool-bar-mode nil)
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq visible-bell t)
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-allow-anti-aliasing t
      x-select-enable-clipboard t
      mac-command-key-is-meta t)

;; Enable Marmalade package archive
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Always install nREPL
(when (not (package-installed-p 'nrepl)) (package-install 'nrepl))

;; Prevent error buffer in REPL
(setq nrepl-popup-stacktrace nil)