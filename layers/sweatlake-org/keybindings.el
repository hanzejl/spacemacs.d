;;; keybindings.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; A complementary binding to the apropos-command (C-h a)

(spacemacs/declare-prefix "ab" "blog")
(spacemacs/set-leader-keys "abb" 'sweatlake/hexo-relume-me)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "ic" 'sweatlake/org-insert-src-block)
