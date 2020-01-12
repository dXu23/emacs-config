


;;;* Have outline mode in emacs-lisp:
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp ";;;\\*+\\|\\`")
	    (make-local-variable 'outline-heading-end-regexp)
	    (setq outline-heading-end-regexp ":\n")
	    (outline-minor-mode 1)
	    ))


;;;* Display agenda on startup
(add-hook 'after-init-hook 'org-agenda-list)

;;;* Set up recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\C-r" 'recentf-open-files)

;;;* config edit/reload:
;;;** edit:

  (defun config-visit ()
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  (global-set-key (kbd "C-c e") 'config-visit)

;;;** reload:

  (defun config-reload ()
    (interactive)
    (load-file (expand-file-name "~/.emacs.d/init.el")))
  (global-set-key (kbd "C-c r") 'config-reload)

;;;* 'Fixing' Emacs::
;;;** Set emacs so that scratch buffer shows up instead of help screen:
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

(setq visible-bell t)

;;;** Prevent emacs from freezing when pressing 'C-x C-c':
(setq x-select-enable-clipboard-manager nil)

;;;**Prevent emacs from making backup files:
(setq make-backup-file nil)

;;;**Get rid of lock mode:
(setq create-lockfiles nil)

;;;**Prevent emacs from auto-saving:
(setq auto-save-default nil)

;;;**Make yes or no prompts as simple as typing 'y' or 'n':

(defalias 'yes-or-no-p 'y-or-n-p)

;;;**Adding '/usr/local/bin' to exec-path:

(add-to-list 'exec-path "/usr/local/bin")

;;;**Set default tramp method to ssh:
  (setq tramp-default-method "ssh")

;;;**Use newlines  as a separator when appending and prepending to registers:
  (setq register-separator ?+)
  (set-register register-separator "\n\n")


;;;**utf-8:
  (prefer-coding-system 'utf-8)

;;;**Relative line numbers:
  (setq display-line-numbers 'relative)


;;;* Some preferred keybindings:
;;;** Set 'M-S-z' to zap-up-to-char:

  (global-set-key "\M-Z" 'zap-up-to-char)

;;;** Split and follow function:

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


(defun other-window-backward (&optional n)
  "Select Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-window n))))


;;;* Insert date and time easily:
  (defun mp-insert-date ()
    (interactive)
    (insert (format-time-string "%x")))

  (defun mp-insert-time ()
    (interactive)
    (insert (format-time-string "%X")))

  (global-set-key (kbd "C-c i d") 'mp-insert-date)
  (global-set-key (kbd "C-c i t") 'mp-insert-time)

;;;* Copy rectangle region:

   (defun my-copy-rectangle (start end)
     "Copy the region-rectangle instead of `kill-rectangle'."
     (interactive "r")
     (delete-rectangle start end)
     (setq killed-rectangle (extract-rectangle start end)))

(global-set-key (kbd "C-x r M-w") 'my-copy-rectangle)


;;;* Preinstalled packages::
;; ido-mode:

  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode 1)
  ;; (require 'ido)
  ;; (ido-mode t)

;; Whitespace:

  (require 'whitespace)
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t)

;; Org:
;;;* Org-mode keybindings::

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)


;;;* Set org-mode agenda files::
;; Going to take a break from using org-mode to organize stuff
;;  (setq org-agenda-files '("~/gtd/inbox.org"
;;			   "~/gtd/gtd.org"
;;			   "~/gtd/tickler.org"))



;;;* Set priority range from A to C with default A::

  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)


;;;* Set colors for priorities::
;;;* Set ctr-a and ctrl-e for org-mode ::

(setq org-special-ctrl-a t)
(setq org-special-ctrl-e t)


;;;* Set org-log-done to true::

(setq org-log-done 'note)

  (setq org-priority-faces '((?A . (:foreground "#DC143C" :weight bold))
			     (?B . (:foreground "#FFA500"))
			     (?C . (:foreground "#48D1CC"))))


;;;* Org-mode templates::

  (setq org-capture-templates '(("t" "Todo [inbox]" entry
				 (file+headline "~/gtd/inbox.org" "Tasks")
				 "* TODO [#A] %i%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
				("T" "Tickler" entry
				 (file+headline "~/gtd/tickler.org" "Tickler")
				 "* %i%? \n %U")))

;;;* 
;;;* Hide emphasis markers::

(setq org-hide-emphasis-markers t)

;;;* Set header-line::
(setq header-line-format " ")

;;;* open agenda in current window::
  (setq org-agenda-window-setup (quote current-window))


;;;* Warn about any deadline in next 7 days::

  (setq org-deadline-warning-days 7)


;;;* Show tasks scheduled/due in next fortnight::

  (setq org-agenda-span (quote fortnight))


;;;* Do not show tasks as scheduled if already shown as deadline::

  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)


;;;* Do not give warning colors to tasks w/ impending deadlines::

  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))


;;;* Do not show tasks that are scheduled or have deadlines in normal todo list::

  (setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))

;; Org timer

(setq org-timer-default-timer 25)

(add-hook 'org-clock-in-hook (lambda ()
			       (if (not org-timer-current-timer)
				   (org-timer-set-timer '(16)))))

;;;* How tasks should be sorted::
  (setq org-agenda-sorting-strategy
	(quote
	 ((agenda deadline-up priority-down)
	  (todo priority-down category-keep)
	  (tags priority-down category-keep)
	  (search category-keep))))


;;;* org-refile targets::

;; (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
;;			     ("~/gtd/someday.org" :level . 1)
;;			     ("~/gtd/tickler.org" :maxlevel . 2)))

;;;* org-mode todo keywords::

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "PROJ(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))


;;;* Turn off org-goto-auto-isearch::

  (setq org-goto-auto-isearch nil)


;;;* Set org-indent to 2::
  (setq org-list-indent-offset 2)

;;;* Save clock history across emacs sessions::

  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)


;;;* Syntax highlight text in block::

  (setq org-src-fontify-natively t)

;;;* Maximum indentation for description lists::

  (setq org-list-description-max-indent 5)

;;;* prevent demoting heading::

  (setq org-adapt-indentation nil)



;;;* Have org-mode support programming languages::
   
     (org-babel-do-load-languages
      'org-babel-load-languages
      '(
	(shell . t)
	(C . t)
	(python . t)
	(R . t)
	(ocaml . t)
	(ditaa . t)
	(dot . t)
	(gnuplot . t)
	))


;;;* Migrate to straight.el:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;;* Extra Packages::
;; package-list:

  ;; </use-package
;;;;  package.el
;;; Minimal package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))


;; avy:

  (use-package avy
    :bind
    ("C-;" . avy-goto-char)
    ("C-=" . avy-goto-word-1)
    :config
    (setq avy-style 'words))


;; beacon:
(use-package beacon
  :init
  (beacon-mode 1))

;; Company:

    (use-package company
      :config
      (setq company-idle-delay 0.5)
      (setq company-show-numbers t)
      (setq company-minimum-prefix-length 3)
      :bind (:map company-active-map
		  ("M-n" . nil)
		  ("M-p" . nil)
		  ("C-n" . company-select-next)
		  ("C-p" . company-select-previous)
		  ("SPC" . company-abort)
		  )
      )

      (defun ora-company-number ()
	"Forward to `company-complete-number'. 

	 Unless the number is potentially part of the candidate.
	 In that case, insert the number"
	(interactive)
	(let* ((k (this-command-keys))
	     (re (concat "^" command-prefix k)))
	(if (cl-find-if (lambda (s) (string-match re s))
			company-candidates)
	    (self-insert-command 1)
	  (company-complete-number (string-to-number k)))))

    ;;(mapc (lambda (x) (define-key company-active-map
    ;;		   (format "%d" x)
    ;;		   'ora-company-number))
    ;;	  (number-sequence 0 9))



;; Company-irony:

  (use-package company-irony

    :after company
    :config
    (add-to-list 'company-backends 'company-irony)
    )

;; Company-jedi:

  (use-package company-jedi
    :config
    (defun my/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))

    (add-hook 'python-mode-hook 'my/python-mode-hook)
    :after company
  )

;; Exec-from-path-initialize:

  (use-package exec-path-from-shell
    :config
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    )


;; Hydra:

   (use-package hydra
    :config
    (defhydra hydra-zoom (global-map "<f2>")
      "zoom"
      ("g" text-scale-increase "in")
      ("l" text-scale-decrease "out"))
    (defhydra hydra-avy-cycle ()
      ("n" avy-next "next")
      ("p" avy-prev "prev")
      ("q" nil "quit"))
    :bind
    ("C-M-'" . hydra-avy-cycle/body)
    )

(use-package counsel)

;; Ivy:
(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  :init
  (ivy-mode 1)
  )


;; htmlize:

;;  (use-package htmlize):


;; Magit:

  (use-package magit
    :bind
    ("C-x g" . magit-status)
    ("C-x M-g" . magit-dispatch))

;; Org Bullets:

   (use-package org-bullets
     :hook
     (prog-mode (org-mode . org-bullets-mode))
     )

;; Python mode:


  ;;  (use-package python-mode)
  ;;use-package 'python-mode
  ;; :config
  ;; (setq-default py-shell-name "ipython")
  ;; (setq-default py-which-bufname "IPython")
  ;;
  ;; (setq py-force-py-shell-name-p t)
  ;;
  ;; (setq py-shell-switch-buffers-on-execute-p t)
  ;; (setq py-switch-buffers-on-execute-p t)
  ;;
  ;; (setq py-split-windows-on-execute-p nil)
  ;;
  ;; (setq py-smart-indentation t)
  ;;

;; rainbow:

  ;; (use-package rainbow-mode
  ;;  :init (rainbow-mode 1))



;; Swiper:

  (use-package swiper
    :bind ("C-s" . swiper-isearch)
    ("C-r" . swiper-isearch-backward))



;; switch-window:


  (use-package switch-window
    :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
	  '("a" "s" "d" "f" "j" "k" "l"))
    :bind
    ([remap other-window] . switch-window))



;; popup-kill-ring:

  (use-package popup-kill-ring
    :bind ("M-y" . popup-kill-ring)
    :config
    (setq save-interprogram-paste-before-kill t))



;; which-key:

(use-package which-key
  :init
  (which-key-mode))


;; Yasnippet:

  (use-package yasnippet
    :config
    (use-package yasnippet-snippets)
    (yas-reload-all)
    (yas-global-mode 1))


;;;* Aesthetic Changes::
;; Change default tab-with to 4 spaces:

  (setq default-tab-width 4)

;; Getting rid of all bars:

(tool-bar-mode 0)
(menu-bar-mode 0)


;;;* Scroll bar::

(scroll-bar-mode 0)

;; Change modeline:

  (column-number-mode 1)
  (set-face-attribute 'mode-line nil :background "light green")
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

;;;* Highlight current line::

(when window-system (global-hl-line-mode t))

;;;* Prettify symbols::

(when window-system (global-prettify-symbols-mode t))

;;;* Set font to M+ 1mn::

  (set-frame-font "M+ 1mn")

;;;* Set emacs theme to simple grey on black::

(custom-set-faces
 '(default ((t (:background "black" :foreground "grey"))))
 '(fringe ((t (:background "black")))))
	      

;;;* Language-Specific Settings::
;; C:

  (setq-default c-basic-offset 4)

;;;* Terminal::
;; Setting default shell to bash:

  (defvar my-term-shell "/bin/bash")
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term)


(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
