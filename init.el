

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
  (require 'ob-tangle))

(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(setq scroll-conservatively 100)

(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(ivy-count-format "%d/%d ")
 '(ivy-height 10)
 '(ivy-initial-inputs-alist nil)
 '(ivy-rebuilders-alist '((t . ivy--regex-ignore-order)) t)
 '(ivy-use-virtual-buffers t)
 '(jdee-jdk-registry
   '(("11.0.1" . "/usr/lib/jvm/java-11-openjdk")
     ("1.8.0_192" . "/usr/lib64/jvm/java-11-openjdk")))
 '(jdee-server-dir "~/jdee-server/target/")
 '(lsp-eldoc-render-all t)
 '(lsp-idle-delay 0.6)
 '(lsp-rust-analyzer-cargo-watch-command "clippy" t)
 '(lsp-rust-analyzer-server-display-inlay-hints t t)
 '(package-selected-packages
   '(geiser-guile org-alert inf-ruby pikchr-mode haskell-mode js2-refactor xref-js2 js2-mode emacsql-sqlite crystal-mode lsp-metals counsel benchmark-init paradox posframe dap-mode paren-face slime-company slime scala-mode org python-mode flycheck arduino-mode hydra company company-irony company-jedi ivy swiper yasnippet-snippets yasnippet magit htmlize 4clojure helm geiser spaceline cider emacsql smartparens fill-column-indicator gradle-mode rtags beacon jdee fsharp-mode which-key use-package moe-theme ein))
 '(py-force-py-shell-name-p t)
 '(py-python-command "ipython")
 '(py-python-command-args '("--gui=wx" "--pylab=wx" "-colors"))
 '(py-smart-indentation t)
 '(py-split-windows-on-execute-p nil t)
 '(py-switch-buffers-on-execute-p t))

(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))
