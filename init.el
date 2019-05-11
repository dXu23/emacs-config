
;; Load settings from org mode file
(require 'org)
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
		 "lisp" (expand-file-name
			 "org" (expand-file-name
				"src" dotfiles-dir))))
        (org-contrib-dir (expand-file-name
              		 "lisp" (expand-file-name
	        		 "contrib" (expand-file-name
		         		    ".." org-dir))))
	(load-path (append (list org-dir org-contrib-dir)
			   (or load-path nil))))

  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle))

(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

;; (defvar my-term-shell "/bin/bash")
;;  (defadvice ansi-term (before force-bash)
;;    (interactive (list my-term-shell)))
;; (ad-activate 'ansi-term)

;; (global-set-key (kbd "<s-return>") 'ansi-term)


(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(jdee-jdk-registry
   (quote
    (("11.0.1" . "/usr/lib/jvm/java-11-openjdk")
     ("1.8.0_192" . "/usr/lib64/jvm/java-11-openjdk"))))
 '(jdee-server-dir "~/jdee-server/target/")
 '(package-selected-packages
   (quote
    (company-jedi org company-irony company ivy swiper yasnippet-snippets yasnippet magit htmlize 4clojure helm geiser spaceline cider emacsql org-bullets smartparens fill-column-indicator gradle-mode rtags beacon jdee fsharp-mode which-key use-package moe-theme ein))))

(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
