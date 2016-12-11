;;; packages.el --- sweatlake-better-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: zr <zr@promote.cache-dns.local>
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
;; added to `sweatlake-better-defaults-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sweatlake-better-defaults/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sweatlake-better-defaults/pre-init-PACKAGE' and/or
;;   `sweatlake-better-defaults/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sweatlake-better-defaults-packages
  '(
    (dired-mode :location built-in)
    (recentf :location built-in)))

(defun sweatlake-better-defaults/init-dired-mode()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ))

      (setq dired-omit-files
            concat(dired-omit-files "\\|.^DS_STORE$|^.projectile$"))

      (put 'dired-find-alternate-file 'disabled nil)
      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
      )
    )
  )

(defun sweatlake-better-defaults/post-init-recentf()
  (progn
    (setq recentf-exclude
          '(
            "COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 1024)))

;;; packages.el ends here
