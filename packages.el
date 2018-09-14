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
    (cquery :requires company company-lsp lsp-mode lsp-ui)
    ))

(defun spacemacs-lsp-cquery/init-cquery ()
  (use-package cquery
    :init
    (progn
      (spacemacs/add-to-hooks #'lsp-cquery-enable '(c-mode-hook c++-mode-hook)))
    :config
    (progn
      (setq cquery-executable lsp-cquery-executable)

      (cquery-use-default-rainbow-sem-highlight)

      (defun spacemacs//c-c++-lsp-string (prefix suffix)
	  (concat prefix "cquery" suffix))

      (defun spacemacs//c-c++-lsp-symbol (prefix suffix)
	  "Return a symbol for the LSP backend specified by the `c-c++-backend' configuration variable."
	    (intern (spacemacs//c-c++-lsp-string prefix suffix)))

      (defun spacemacs//c-c++-lsp-call-function (prefix suffix &rest args)
	  (apply (spacemacs//c-c++-lsp-symbol prefix suffix) args))

      (defun spacemacs//c-c++-lsp-funcall-interactively (prefix suffix &rest args)
	  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix) args))

      (defun spacemacs//c-c++-lsp-funcall-interactively-no-args (prefix suffix)
	  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix)))

      (defun spacemacs//c-c++-lsp-set-symbol (prefix suffix value)
	  (set (spacemacs//c-c++-lsp-symbol prefix suffix) (symbol-value value)))

      (defun c-c++/callers () (interactive) (lsp-ui-peek-find-custom 'callers (spacemacs//c-c++-lsp-string "$" "/callers")))
      (defun c-c++/vars () (interactive) (lsp-ui-peek-find-custom 'vars (spacemacs//c-c++-lsp-string "$" "/vars")))

      (defun c-c++/references-address ()
	(interactive)
	(lsp-ui-peek-find-custom
	  'address "textDocument/references"
	  (plist-put (lsp--text-document-position-params) :context
		     '(:role 128))))

      (defun c-c++/references-read ()
	(interactive)
	(lsp-ui-peek-find-custom
	  'read "textDocument/references"
	  (plist-put (lsp--text-document-position-params) :context
		     '(:role 8))))

      (defun c-c++/references-write ()
	(interactive)
	(lsp-ui-peek-find-custom
	  'write "textDocument/references"
	  (plist-put (lsp--text-document-position-params) :context
		     '(:role 16))))

      (defun spacemacs//c-c++-lsp-ccls-customise-lsp-ui-peek ()
	(defun c-c++/base ()
	  (interactive)
	  (lsp-ui-peek-find-custom 'base "$ccls/inheritanceHierarchy"
				   (append (lsp--text-document-position-params) '(:flat t :level 3)))))

      (defun c-c++/call-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy" nil))
      (defun c-c++/call-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy" t))
      (defun c-c++/inheritance-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy"))
      (defun c-c++/inheritance-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy" t))
      (defun c-c++/member-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively-no-args nil "-member-hierarchy"))
      (defun c-c++/freshen-index () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-freshen-index"))
      (defun c-c++/preprocess-file () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-preprocess-file"))
      (dolist (mode c-c++-modes)
	(spacemacs/set-leader-keys-for-major-mode mode
						  ;; goto
						  ;; FIXME gf conflicts with `lsp-ui-flycheck-list' binding in lsp layer
						  ;; Better corrected in lsp-layer? rebinding here for now
						  "g." #'lsp-ui-peek-find-definitions
						  "gi" #'lsp-ui-peek-find-implementation
						  "gw" #'lsp-ui-peek-find-workspace-symbol
						  "g," #'lsp-ui-peek-find-references
						  "g[" #'lsp-ui-peek-jump-backward
						  "g]" #'lsp-ui-peek-jump-forward
						  "ge" #'lsp-ui-flycheck-list
						  "gf" 'find-file-at-point
						  "gF" 'ffap-other-window
						  "g&" #'c-c++/references-address
						  "gR" #'c-c++/references-read
						  "gW" #'c-c++/references-write
						  "gc" #'c-c++/callers
						  "gv" #'c-c++/vars
						  ;; help/hierarchy
						  "hc" #'c-c++/call-hierarchy
						  "hC" #'c-c++/call-hierarchy-inv
						  "hi" #'c-c++/inheritance-hierarchy
						  "hI" #'c-c++/inheritance-hierarchy-inv
						  "hm" #'c-c++/member-hierarchy
						  ;; lsp/backend
						  "lf" #'c-c++/freshen-index
						  "lp" #'c-c++/preprocess-file
						  "ll" #'lsp-ui-imenu
						  "lr" #'lsp-rename
						  "la" #'lsp-ui-sideline-apply-code-actions
						  ))

      (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
      (setq cquery-extra-init-params '(:completion (:detailedLabel t)))
    )))

;;; packages.el ends here
