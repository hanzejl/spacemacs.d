;;; packages.el --- sweatlake-prog layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: zr <zr@zrs-MacBook-Pro.shanbay>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `sweatlake-prog-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sweatlake-prog/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sweatlake-prog/pre-init-PACKAGE' and/or
;;   `sweatlake-prog/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


(defconst sweatlake-prog-packages
  '(
    add-node-modules-path
    company-flow
    eslintd-fix
    flycheck
    rjsx-mode
    prettier-js
    (elixir-format :location (recipe :fetcher github :repo "anildigital/mix-format.el"))
    ))

(defun sweatlake-prog/init-eslintd-fix ()
  (use-package eslintd-fix
    :defer t
    :commands eslintd-fix-mode
    :init
    (progn
      (add-hook 'rjsx-mode-hook 'eslintd-fix-mode t))))

(defun sweatlake-prog/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

      (setq
       js2-mode-show-strict-warnings nil
       js2-mode-show-parse-errors nil
       js-indent-level 2
       js2-basic-offset 2
       js2-strict-trailing-comma-warning nil
       js2-strict-missing-semi-warning nil)

      (advice-add #'js-jsx-indent-line
                  :after
                  #'sweatlake-prog/js-jsx-indent-line-align-closing-bracket)
      (add-hook 'rjsx-mode-hook #'sweatlake-prog/eslintd-set-flycheck-executable t))
    :config
    (modify-syntax-entry ?_ "w" js2-mode-syntax-table)))

(defun sweatlake-prog/init-add-node-modules-path ()
  (use-package add-node-modules-path
    :defer t
    :init
    (progn
      (add-hook 'web-typescript-mode-hook #'add-node-modules-path)
      (add-hook 'web-mode-hook #'add-node-modules-path)
      (add-hook 'typescript-mode-hook #'add-node-modules-path)
      (with-eval-after-load 'rjsx-mode
        (add-hook 'rjsx-mode-hook #'add-node-modules-path)))))

(defun sweatlake-prog/post-init-company-flow ()
  (spacemacs|add-company-backends
    :backends
    '((company-flow :with company-dabbrev-code)
      company-files)))

(defun sweatlake-prog/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (push 'javascript-jshint flycheck-disabled-checkers)
    (push 'json-jsonlint flycheck-disabled-checkers))

  (spacemacs/enable-flycheck 'rjsx-mode))


(defun sweatlake-prog/init-prettier-js ()
  (use-package prettier-js
    :defer t
    :init
    (progn
      (with-eval-after-load 'web-mode
        (add-hook 'js2-mode-hook 'prettier-js-mode)
        (add-hook 'typescript-mode-hook 'prettier-js-mode)
        (add-hook 'json-mode-hook 'prettier-js-mode)
        (add-hook 'web-mode-hook 'prettier-js-mode)
        (add-hook 'rjsx-mode-hook 'prettier-js-mode)
        )
      )
    )
  )

(defun sweatlake-prog/init-elixir-format ()
  (use-package elixir-format
    :defer t
    :init
    (progn
      (setq-default elixir-format-elixir-path "/usr/local/bin/elixir"
                    elixir-format-mix-path "/usr/local/bin/mix")
      (add-hook 'elixir-mode-hook
                (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

      (setq elixir-format-arguments
            (list "--dot-formatter"
                  "/Users/zr/Developer/formatter/elixir/.formatter.exs"))
      )))

;;; packages.el ends here
