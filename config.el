(setq inhibit-startup-message t)

(setq x-select-enable-clipboard-manager nil)

(setq make-backup-file nil)

(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'exec-path "/usr/local/bin")

(setq tramp-default-method "ssh")

(setq register-separator ?+)
(set-register register-separator "\n\n")

(global-set-key "\M-Z" 'zap-up-to-char)

(defun split-and-follow-horizontally ()
  "Splits a window horizontally and follows to opened window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1)
  )

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)


(defun split-and-follow-vertically ()
  "Splits a window vertically and follows to opened window"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1)
  )

(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun compile-based-on-extension (&optional args) 
    "Compile/run a file based on its extension"
    (interactive "P")
    (setq file-extension (file-name-extension buffer-file-name))
    (setq executable-name (file-name-base buffer-file-name))
    (cond ((string= file-extension "c")
	  (compile (concat "cc -o " executable-name " " buffer-file-name " && ./" executable-name)))
	  ((string= file-extension "cpp")
	   (compile (concat "g++ -o " executable-name " " buffer-file-name " && ./" executable-name)))
	  ((string= file-extension "java")
	  (compile (concat "javac " buffer-file-name " && java " executable-name)))
    )
)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)
;; (require 'ido)
;; (ido-mode t)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

(setq org-log-done t)

(setq org-agenda-files (list "~/gtd/inbox.org"
			     "~/gtd/gtd.org"
			     "~/gtd/tickler.org"))

(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

(setq org-priority-faces '((?A . (:foreground "#DC143C" :weight bold))
			   (?B . (:foreground "#FFA500"))
			   (?C . (:foreground "#48D1CC"))))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/gtd/inbox.org" "Tasks")
			       "* TODO [#A] %i%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
			      ("T" "Tickler" entry
			       (file+headline "~/gtd/tickler.org" "Tickler")
			       "* %i%? \n %U")))

(setq org-agenda-window-setup (quote current-window))

(setq org-deadline-warning-days 7)

(setq org-agenda-span (quote fortnight))

(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))

(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
	(todo priority-down category-keep)
	(tags priority-down category-keep)
	(search category-keep))))

(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "PROJ(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-goto-auto-isearch nil)

(setq org-list-indent-offset 2)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-src-fontify-natively t)

(setq org-list-description-max-indent 5)

(setq org-adapt-indentation nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (C . t)
   (python . t)
   (R . t)
   (ditaa . t)
   (gnuplot . t)
   ))

;; <use-package>
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("org" . "http://orgmode.org/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; </use-package

(use-package avy
  :ensure t)

(defun avy-goto-char-n (&optional n arg beg end &rest chars)
  (interactive (append '((prefix-numeric-value current-prefix-arg) nil nil nil)
		     (let ((count 1)
			   charList)
		       (while (<= count (prefix-numeric-value current-prefix-arg))
			 (push (read-char (format "char %d: " count) t) charList)
			 (setq count (1+ count))
			 )
		       (reverse charList))
		     )
	       )
  (mapcar (lambda (char) (when (eq char ?) (setq char ?\n))) chars)
  (avy-with avy-goto-char-n
    (avy--generic-jump
     (regexp-quote (concat chars))
     arg
     avy-style
     beg end)))

(global-set-key (kbd "C-:") 'avy-goto-char-n)

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package cider
  :ensure t)

(use-package helm
 :ensure t
 :bind
 ("M-x" . 'helm-M-x)
 ;;("C-x r b" 'helm-filtered-bookmarks)
 ("C-x C-f" . 'helm-find-files)
 ("C-x C-b" . 'helm-buffers-list)
 ;; ("C-i" . 'helm-execute-persistent-action)
 :config
 (setq helm-autoresize-max-height 0
       helm-autoresize-min-height 40
       helm-M-x-fuzzy-match t
       helm-recentf-fuzzy-match t
       helm-semantic-fuzzy-match t
       helm-imenu-fuzzy-match t
       helm-split-window-inside-p t
       helm-move-to-line-cycle-in-source nil
       helm-ff-search-library-in-sexp t
       helm-scroll-amount 8
       helm-echo-input-in-header-line t)


 (when (executable-find "curl")
   (setq helm-net-prefer-curl t))

 :init
 (helm-mode 1))

(require 'helm-config)
(helm-autoresize-mode 1)
(define-key helm-find-files-map (kbd "<tab>") 'helm-find-files-up-one-level)

(use-package ivy
  :ensure t)

(use-package htmlize)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow)))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch))

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

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring)
  :config
  (setq save-interprogram-paste-before-kill t))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package 4clojure
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (yas-global-mode 1))

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(when window-system (global-hl-line-mode t))

(when window-system (global-prettify-symbols-mode t))

(set-frame-font "M+ 1mn")

(unless (package-installed-p 'moe-theme)
  (package-refresh-contents)
  (package-install 'moe-theme))

(require 'moe-theme)
(moe-light)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
