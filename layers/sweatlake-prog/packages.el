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
    rjsx-mode
  ))


(defun sweatlake-prog/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
      ;; Required to make imenu functions work correctly
      )
    ))

;;; packages.el ends here
