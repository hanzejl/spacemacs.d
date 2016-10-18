;;; packages.el --- magic-gui layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: zr <zr@zrdeMacBook-Pro.local>
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
;; added to `magic-gui-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `magic-gui/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `magic-gui/pre-init-PACKAGE' and/or
;;   `magic-gui/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defvar sweatlake-ui-packages '(tabbar))

;; (defun sweatlake-ui/init-cursor-chg ()
;;   (use-package change-chg))

(defun sweatlake-ui/init-tabbar()
  (use-package tabbar))

;;; packages.el ends here
