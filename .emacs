;; No toolbar
(tool-bar-mode -1)

;; No menu
(menu-bar-mode -1)

;; No scrollbar
(scroll-bar-mode -1)

;; No beep
(setq visible-bell t)

;; No splash screen
(setq inhibit-startup-message t)

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
		      which-key
		      ido-vertical-mode
		      iedit
		      magit
		      cider
		      clj-refactor
		      restclient))
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

;; Display available keybindings in popup
(which-key-mode)

;; Find files with fuzzy matching
(require 'ido-vertical-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Calendar
(setq calendar-week-start-day 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Org-mode
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Clojure
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
