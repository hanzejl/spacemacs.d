;;; packages.el --- magic-evil layer packages file for Spacemacs.
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
;; added to `magic-evil-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `magic-evil/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `magic-evil/pre-init-PACKAGE' and/or
;;   `magic-evil/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defconst sweatlake-evil-packages
      '(evil))

(defun sweatlake-evil/post-init-evil()
  (progn

    (loop for (mode . state) in
          '((minibuffer-inactive-mode . emacs)
            (ggtags-global-mode . emacs)
            (grep-mode . emacs)
            (Info-mode . emacs)
            (term-mode . emacs)
            (sdcv-mode . emacs)
            (anaconda-nav-mode . emacs)
            (log-edit-mode . emacs)
            (vc-log-edit-mode . emacs)
            (magit-log-edit-mode . emacs)
            (inf-ruby-mode . emacs)
            (direx:direx-mode . emacs)
            (yari-mode . emacs)
            (erc-mode . emacs)
            (w3m-mode . emacs)
            (gud-mode . emacs)
            (help-mode . emacs)
            (eshell-mode . emacs)
            (shell-mode . emacs)
            ;;(message-mode . emacs)
            (fundamental-mode . emacs)
            (weibo-timeline-mode . emacs)
            (weibo-post-mode . emacs)
            (sr-mode . emacs)
            (dired-mode . emacs)
            (compilation-mode . emacs)
            (speedbar-mode . emacs)
            (magit-commit-mode . normal)
            (magit-diff-mode . normal)
            (js2-error-buffer-mode . emacs)

            ;; add by my self
            (org-mode . emacs)
            (markdown-mode . emacs)
            (hexo-mode . emacs)

            ;; some interactive mode
            (inferior-python-mode . emacs)
            (inferior-octave-mode . emacs)
            (2048-mode . emacs)
            (inferior-erlang . emacs)
            (inferior-tcl . emacs)
            (inferior-js-mode . emacs)
            (inferior-ess . emacs)
            (inferior-ess-mode . emacs)
            (inferior-scheme-mode . emacs)
            (inferior-lisp . emacs))
          do (evil-set-initial-state mode state))

    (defun spacemacs/sweatlake-evil()
      (interactive)
      (magic-evil))
  ))

;;; packages.el ends here
