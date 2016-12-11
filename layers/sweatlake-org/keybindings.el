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
(spacemacs/set-leader-keys "abb" 'blog-admin-start)
(spacemacs/set-leader-keys "abd" 'blog-admin-backend-hexo-deploy-site)
(spacemacs/set-leader-keys "abc" 'blog-admin-backend-hexo-config-file)
(spacemacs/set-leader-keys "abg" 'blog-admin-backend-hexo-build-site)
