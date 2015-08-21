;; No toolbar
(tool-bar-mode -1)

;; No menu
(menu-bar-mode -1)

;; No scrollbar
(scroll-bar-mode -1)

;; No beep
(setq visible-bell t)

;; Relocate backup files
(setq backup-directory-alist '(("." . "~/.emacs_backups")))

;; Journaling macros
(defun insert-time () (interactive) (insert (format-time-string "%Y-%m-%d %R")))
(defun journal () (interactive) (find-file "~/Documents/Journal.txt") (end-of-buffer) (insert "\n\n") (insert-time) (insert "\n================\n"))

;; Enable Emacs Lisp Package Archive (ELPA)
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Install packages
(defvar my-packages '(color-theme-solarized
		      evil
		      exec-path-from-shell
		      magit
		      cider))
(dolist (p my-packages) (unless (package-installed-p p) (package-install p)))

;; Set path from shell when Emacs is launched from GUI
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Color scheme
(set-frame-parameter nil 'background-mode 'light)
(set-terminal-parameter nil 'background-mode 'dark)
(load-theme 'solarized t)

;; Vim emulation
(require 'evil)
(evil-mode 1)

;; Find files with fuzzy matching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Calendar
(setq calendar-week-start-day 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
