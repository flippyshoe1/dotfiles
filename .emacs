;;; Package management (and custom functions)
(when (>= emacs-major-version 24)
  (progn
    ;; load emacs 24's package system.
    (require 'package)
    ;; Add MELPA repository.
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
  (when (< emacs-major-version 27) (package-initialize)))

(defun pm/require-package (package)
  (when (not (package-installed-p package))
    (package-install package)))

(defun pm/require (&rest packages)
  (dolist (package packages)
    (pm/require-package package)))

;;; Basic UI changes
(setq inhibit-startup-message t)     ;; no startup message
(global-display-line-numbers-mode 1) ;; numbers on the side!!
;; no bloat on my machine!
(menu-bar-mode -1) 
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; check if the jetbrainsmono nerd font is downloaded and if it is then use it
;; get it from https://www.nerdfonts.com/font-downloads and install it at ~/.local/share/fonts
(if (find-font(font-spec :name "JetBrainsmono Nerd font"))
    (add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono Nerd font-12")))

;;; Handy settings
(recentf-mode 1)    ;; remembers the last files you edited in emacs (use M-x recentf-open-files to use)
(save-place-mode 1) ;; remembers where you've last been in a file after closing it
;; prevents "littering" by making the custom emacs settings go elsewhere :)
(setq custom-file(locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(global-auto-revert-mode 1) ;; autoupdate the file if it has been changed

;;; Backup?
(setq make-backup-files nil)   ;; creates a backup file
;(setq version-control t)      ;; make multiple versions of the backup
;(setq backup-by-copying t)    ;; preserve metadata when making backups
;(setq vc-make-backup-files t) ;; make backups even in version controlled dirs

;;; Ido and Smex
(require 'ido) ;; the ido part
(ido-mode t)
(ido-everywhere 1)

(pm/require 'smex) ;; the smex part
(global-set-key (kbd "M-x") 'smex)

;;; Theme
(pm/require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)
;(load-theme 'sanityinc-tomorrow-bright t)

;;; Electric Pair Mode (autoinsert matching delimiters)
;;; https://www.emacswiki.org/emacs/ElectricPair
(electric-pair-mode 1) ;; TODO: limit this to C source files?

;;; Company mode (completions)
;;; https://company-mode.github.io/
(pm/require 'company)
(global-company-mode)

;;; Tagging
;; TODO: look into GNU Global source tagging system?

;;; Hideshow
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hideshow.html
;;; emacs keeps giving me an annoying warning every time i launch it, so ill disable it for now
;(hs-minor-mode 1)

;;; C-Mode (or cc mode or whatever the fuck)
;;; https://cc-mode.sourceforge.net/html-manual/index.html
(setq c-basic-offset 2) ;; tab offset
;; indentation styles apparently?
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

;; forces comment commands to insert line comments on c source files rather than block comments
;; i.e. // instead of /* */
(add-hook 'c-mode-hook (lambda()
			 (interactive)
			 (c-toggle-comment-style -1)))
