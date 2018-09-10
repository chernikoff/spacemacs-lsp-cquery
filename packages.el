;;; packages.el --- lsp-cquery layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: scturtle <scturtle@gmail.com>
;; URL: https://github.com/scturtle/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: Unlicense

(defconst spacemacs-lsp-cquery-packages
  '(
    cquery
    company-lsp
    lsp-ui
    ))

(defun spacemacs-lsp-cquery/init-cquery ()
  (use-package cquery
    :init
    (progn
      (spacemacs/add-to-hooks #'lsp-cquery-enable '(c-mode-hook c++-mode-hook)))
    :config
    (progn
      (setq cquery-executable lsp-cquery-executable)
    )))

(defun spacemacs-lsp-cquery/init-company-lsp ()
  (use-package company-lsp
	       :init
	       (progn
		 (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
		 (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
		 (setq cquery-extra-init-params '(:completion (:detailedLabel t)))	
		 )))

(defun spacemacs-lsp-cquery/init-lsp-ui ()
  (use-package lsp-ui
	       :init
	       (progn
		 (dolist (mode '(c-mode c++-mode))
		   (evil-leader/set-key-for-mode mode
						 "g." #'lsp-ui-peek-find-definitions
						 "g," #'lsp-ui-peek-find-references
						 "g[" #'lsp-ui-peek-jump-backward
						 "g]" #'lsp-ui-peek-jump-forward
						 "qb" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
						 "qc" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/callers"))
						 "qd" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/derived"))
						 "qv" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/vars"))
						 "R"  #'cquery-freshen-index
						 "hm" #'cquery-member-hierarchy
						 "hi" #'cquery-inheritance-hierarchy
						 "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
						 "hc" #'cquery-call-hierarchy
						 "hC" (lambda () (interactive) (cquery-call-hierarchy t))
						 "ll" #'lsp-ui-imenu
						 "lr" #'lsp-rename
						 )))))

;;; packages.el ends here
