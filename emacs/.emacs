;;; package repos
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;;; package management functions i stole from tsoding
(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))


;;; default
(setq inhibit-startup-message t)
(recentf-mode 1)
(setq backup-directory-alist '(("" . "~/.emacs.d/recovery")))


;;; theme
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(rc/require 'solarized-theme)
(load-theme 'solarized-dark t)


;;; ido
(rc/require 'smex 'ido-completing-read+)

(require 'ido-completing-read+)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; ide stuff
(rc/require 'company 'flycheck 'eglot)
(add-hook 'after-init-hook 'global-company-mode)

;; syntax checker flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
; error showing up console
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
;(setq flycheck-gcc-include-path '("/home/<user>/<project>"))
(setq flycheck-disabled-checkers '(c/c++-clang))
(setq flycheck-enabled-checkers '(c/c++-gcc))

