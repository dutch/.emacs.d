;;; init.el --- clamberson's configuration file

;; Remove all GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remove the default splash screen
(setq inhibit-startup-screen t)

;; Save backups in the system's temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Initialize package.el
(require 'package)
(dolist (source
         '(("melpa" . "http://melpa.milkbox.net/packages/"))
         (package-initialize))
  (add-to-list 'package-archives source t))

;; Ensure installation of my favorite packages
(defvar my-packages
  '(moe-theme
    nyan-mode
    autopair
    company
    flycheck
    flycheck-rust))

(defun install-packages ()
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)
    (setq inferior-lisp-program "sbcl")))

;; Use Nyan Cat in place of the scrollbar
(eval-after-load "nyan-mode-autoloads"
  '(nyan-mode t))

;; Enable autopair by default
(eval-after-load "autopair-autoloads"
  '(progn
     (require 'autopair)
     (autopair-global-mode t)))

(eval-after-load 'autopair
  '(setq autopair-autowrap t))

;; Theming configuration
(eval-after-load "moe-theme-autoloads"
  '(when (display-graphic-p)
     (require 'moe-theme)
     (load-theme 'moe-light t)
     (set-face-attribute 'default nil :font "Source Code Pro-18")))

;; Auto-completion with company
(eval-after-load "company-autoloads"
  '(require 'company))

(eval-after-load 'company
  '(add-hook 'after-init-hook 'global-company-mode))

;; Syntax checking with Flycheck
(eval-after-load "flycheck-autoloads"
  '(require 'flycheck))

(eval-after-load 'flycheck
  '(progn
     (add-hook 'after-init-hook 'global-flycheck-mode)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(provide 'init)
;;; init.el ends here
