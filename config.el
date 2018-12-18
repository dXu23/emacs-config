(setq inhibit-startup-message t)

(setq x-select-enable-clipboard-manager nil)

(setq make-backup-file nil)

(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

;; <use-package>
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; </use-package

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
;; (require 'ido)
;; (ido-mode t)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ([remap other-window] . switch-window))

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(when window-system (global-hl-line-mode t))

(when window-system (global-prettify-symbols-mode t))

(set-default-font "-*-terminus-*-*-*-*-18-*-*-*-*-*-*-*")

(unless (package-installed-p 'moe-theme)
  (package-refresh-contents)
  (package-install 'moe-theme))

(require 'moe-theme)
(moe-light)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
