(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu"          . "https://elpa.gnu.org/packages/")
			 ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
(setq use-package-always-ensure t
      use-package-expand-minimally t))

(require 'use-package)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(setq inhibit-startup-message t)

(setq make-backup-file nil)

(setq create-lockfiles nil)

(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'exec-path "/usr/local/bin")

(setq tramp-default-method "ssh")

(setq register-separator ?+)
(set-register register-separator "\n\n")

(prefer-coding-system 'utf-8)

(setq display-line-numbers 'relative)

(global-set-key "\M-Z" 'zap-up-to-char)

(defun split-and-follow-horizontally ()
   "Splits a window horizontally and follows to opened window"
   (interactive)
   (split-window-below)
   (balance-windows)
   (other-window 1)
   )

 (defun split-and-follow-vertically ()
   "Splits a window vertically and follows to opened window"
   (interactive)
   (split-window-right)
   (balance-windows)
   (other-window 1)
   )

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun mp-insert-date ()
  (interactive)
  (insert (format-time-string "%x")))

(defun mp-insert-time ()
  (interactive)
  (insert (format-time-string "%X")))

(global-set-key (kbd "C-c i d") 'mp-insert-date)
(global-set-key (kbd "C-c i t") 'mp-insert-time)

(defun my-copy-rectangle (start end)
   "Copy the region-rectangle instead of `kill-rectangle'."
   (interactive "r")
   (delete-rectangle start end)
   (setq killed-rectangle (extract-rectangle start end)))

(global-set-key (kbd "C-x r M-w") 'my-copy-rectangle)

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(ido-mode 1)
;; (require 'ido)
;; (ido-mode t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;; (require 'org-checklist)

(setq org-log-done 'note)

(setq org-agenda-files '("~/gtd/inbox.org"
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

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
			   ("~/gtd/someday.org" :level . 1)
			   ("~/gtd/tickler.org" :maxlevel . 2)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "PROJ(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-goto-auto-isearch nil)

(setq org-list-indent-offset 2)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-src-fontify-natively t)

(setq org-list-description-max-indent 5)

(setq org-adapt-indentation nil)

(use-package ob-python
  :defer t
  :ensure org-contrib
  :commands (org-babel-execute:python))

(use-package ob-shell
  :defer t
  :ensure org-contrib
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh

   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-C
  :defer t
  :ensure org-contrib
  :commands
  (org-babel-execute:C
   org-babel-expand-body:C))

(use-package ob-R
  :defer t
  :ensure org-contrib
  :commands
  (org-babel-execute:R
   org-babel-expand-body:R))

(use-package ob-ditaa
  :defer t
  :ensure org-contrib
  :commands
  (org-babel-execute:ditaa
   org-babel-expand-body:ditaa))

(use-package ob-gnuplot
  :defer t
  :ensure org-contrib
  :commands
  (org-babel-execute:gnuplot
   org-babel-expand-body:gnuplot))

(defun proced-settings ()
  "Function for setting proced settings."
  (proced-toggle-auto-update))

(add-hook 'proced-mode-hook 'proced-settings)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0))
)

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

;; (use-package cider
;;  :ensure t)

(use-package company
  :ensure t
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-company-provider :capf)
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 3)
  :bind (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("M-<" . company-select-first)
	      ("M->" . company-select-last)
	      ("SPC" . company-abort))
  )

  (defun ora-company-number ()
    "Forward to `company-complete-number'.

     Unless the number is potentially part of the candidate.
     In that case, insert the number"
    (interactive)
    (let* ((k (this-command-keys))
	 (re (concat "^" command-prefix k)))
    (if (find-if (lambda (s) (string-match re s))
		    company-candidates)
	(self-insert-command 1)
      (company-complete-number (string-to-number k)))))

;; (let ((map company-active-map))
;; (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
;; 	(number-sequence 0 9))
;; (define-key map " " (lambda ()
;;                       (interactive)
;;                       (company-abort)
;;                       (self-insert-command 1)))
;; (define-key map (kbd "<return>") nil))

(use-package company-irony
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-irony)
  )

(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

  (add-hook 'python-mode-hook 'my/python-mode-hook)
  :after company
)

(use-package counsel
  :ensure t
  :after (ivy swiper)
  :bind (("M-x" . counsel-M-x)
	 ("C-c j" . counsel-git-grep)
	 ("C-h b" . counsel-descbinds)
	 ("C-h f" . counsel-describe-function)
	 ("C-h v". counsel-describe-variable)
	 ("C-h a" . counsel-apropos)
	 ("C-h S" . counsel-info-lookup-symbol)
	 ("C-x r b" . counsel-bookmark)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c P" . counsel-package)
	 ("C-r" . counsel-minibuffer-history)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)
	 :map shell-mode-map
	 ("C-r" . counsel-shell-history)))

(use-package dap-mode
  :after (lsp-mode)
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  )

;;  (use-package exec-path-from-shell
;;    :config
;;    (when (memq window-system '(mac ns x))
;;      (exec-path-from-shell-initialize))
;;    )

(use-package flycheck
  :init (global-flycheck-mode))

(use-package hydra
 :config
 (defhydra hydra-zoom (global-map "<f2>")
   "zoom"
   ("g" text-scale-increase "in")
   ("l" text-scale-decrease "out")))

(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("F" forward-word)
   ("b" backward-char)
   ("B" backward-word)
   ("a" move-beginning-of-line)
   ("A" backward-sentence)
   ("e" move-end-of-line)
   ("E" forward-sentence)
   ("v" scroll-up-command)
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)
   (">" end-of-buffer)
   ("<" beginning-of-buffer))
 )

(use-package ivy
    :ensure t
    :config
    (ivy-mode 1)
    :custom
    (ivy-use-virtual-buffers t)
    (ivy-height 10)
    (ivy-count-format "%d/%d ")
    (ivy-initial-inputs-alist nil)
    (ivy-rebuilders-alist '((t . ivy--regex-ignore-order)))
    :bind
    (("C-c C-r" . ivy-resume)
     ("C-x b" . ivy-switch-buffer)
     :map ivy-minibuffer-map
     ("C-n" . ivy-next-line))
)

;;  (use-package htmlize)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :hook (scala-mode .lsp)
  (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure
  :after lsp
  :hook lsp-mode-hook
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-show-hover t)
  (lsp-ui-doc-enable t))

(use-package lsp-metals
  :after lsp-mode)

(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode-hook . paredit-mode)
	 (lisp-interaction-mode-hook . paredit-mode)
	 (ielm-mode-hook . paredit-mode)
	 (lisp-mode-hook . paredit-mode)
	 (eval-expression-minibuffer-setup-hook . paredit-mode))
  :bind (("C-M-u" . paredit-backward-up)
	 ("C-M-n" . paredit-forward-up)
	 ("M-S" . paredit-splice-sexp-killing-backward)
	 ("M-R" . paredit-raise-sexp)
	 ("M-(" . paredit-wrap-round)
	 ("M-[" . paredit-wrap-square)
	 ("M-{" . paredit-wrap-curly))
  :config
  (diminish 'paredit-mode "()"))

(use-package projectile
  :ensure t
  :after (magit cider)
  :init
  (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (("C-x p" . projectile-mode-map)
	 :map projectile-mode-map
	 ("s-p" . projectile-command-map))
  :custom
  (projectile-create-missing-test-files t)
  (projectile-switch-projectile-action projectile-commander)
  :config
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (projectile-run-shell))
  (def-projectile-commander-method ?c
    "Run `compile' in the project."
    (projectile-compile-project nil))

  (def-projectile-commander-method ?\C-?
    "Go back to project selection."
    (projectile-switch-project))

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?F
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
	(call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))

  (def-projectile-commander-method ?j
    "Jack-in."
    (let* ((opts (projectile-current-project-files))
	   (file (ido-completing-read
		  "Find file: "
		  opts
		  nil nil nil nil
		  (car (cl-member-if
			(lambda (f)
			  (string-match "core\\.clj\\'" f))
			opts)))))
      (find-file (expand-file-name
		  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in)))
  )

(use-package python-mode
  :custom
  (py-shell-name "ipython")
  (py-force-py-shell-name-p t)
  (py-switch-buffers-on-execute-p t)
  (py-smart-indentation t)
  (py-split-windows-on-execute-p nil)
  (py-python-command-args '("--gui=wx" "--pylab=wx" "-colors"))
  (py-smart-indentation t)
  :config
  (setq-default py-which-bufname "IPython"))



 ;;

(use-package rainbow-mode
 :ensure t
 :init (rainbow-mode 1))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper-isearch)))

(use-package sbt-mode
:commands sbt-start sbt-command
:config
;; WORKAROUND: allows using SPACE when in the minibuffer
(substitute-key-definition
'minibuffer-complete-word
'self-insert-command
minibuffer-local-completion-map))

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

(use-package paradox
  :config
  (paradox-enable)
)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring)
  :config
  (setq save-interprogram-paste-before-kill t))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  (yas-global-mode 1))

(setq default-tab-width 4)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode 1)
(set-face-attribute 'mode-line nil :background "light blue")
(set-face-attribute 'mode-line-buffer-id nil :background "blue" :foreground)
(defface mode-line-directory
  '((t : background "blue" :foreground "gray"))
  "Face used for buffer identification parts of the mode line."
  :group 'mode-line-faces
  :group 'basic-faces)

(set-face-attribute 'mode-line-highlight nil :box nil :background "deep sky blue")
(set-face-attribute 'mode-line-inactive nil :inherit 'default)

(setq mode-line-position
      '((line-number-mode ("%l" (column-number-mode ":%c")))))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name) (concat " " (shorten-directory default-directory 20)) " "))
    face mode-line-directory)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

(setq-default mode-line-buffer-identification
	      (propertized-buffer-identification "%b "))

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		;; mode-line-mule-info --
		mode-line-client
		mode-line-modified
		;; mode-line-remote -- no need to indicate this specially
		;; mode-line-frame-identification
		" "
		mode-line-directory
		mode-line-buffer-identication
		" "
		mode-line-position
		(flycheck-mode flycheck-mode-line)
		" "
		mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))

(when window-system (global-hl-line-mode t))

(when window-system (global-prettify-symbols-mode t))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(unless (package-installed-p 'moe-theme)
  (package-refresh-contents)
  (package-install 'moe-theme))

(require 'moe-theme)
(moe-dark)

(setq-default c-basic-offset 4)

(use-package cider)

;; (global-set-key (kbd "C-c e l") #'find-library)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq library-lisp-implementations '((sbcl ("sbcl")))
      slime-default-lisp 'sbcl
      slime-contribs '(slime-fancy))

(use-package paren-face
  :defer)

(defun my-emacs-lisp-mode-hook-fn ()
  (set (make-local-variable 'lisp-indent-function) #'lisp-indent-function)
  (local-set-key (kbd "C-c S") (global-key-binding (kbd "M-s")))
  (show-paren-mode 1)
  (paren-face-mode)
  )

(use-package slime-company
  :defer)

(use-package slime
  :demand
  :config
  (slime-setup '(slime-fancy slime-company slime-cl-indent)))

(add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

(defun javap-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
	(call-process "javap" nil (current-buffer) nil "-verbose"
		      "-classpath" (file-name-directory file)
		      (file-name-sans-extension
		       (file-name-nondirectory file)))
	(setq buffer-file-name file)
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(goto-char (point-min))
	(java-mode)
	(current-buffer))))
   ((javap-handler-real op args))))

(defun javap-handler-real (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
	 (cons 'javap-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(use-package scala-mode
  :mode "\\.\\(sc\\|scala\\)|\\'"
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
