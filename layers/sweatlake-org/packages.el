;;; packages.el --- magic-org layer packages file for Spacemacs.
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
;; added to `magic-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `magic-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `magic-org/pre-init-PACKAGE' and/or
;;   `magic-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sweatlake-org-packages '(org
                                   (ox-publish :location built-in)
                                   blog-admin
                                   org-octopress))

(defun sweatlake-org/post-init-org()
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (setq-default
   org-html-validation-link nil
   ;; some variables about the startup
   org-startup-with-inline-images nil       ; not show the inline images
   ;; org-agenda-include-diary t               ; show the diary integration
   org-agenda-start-on-weekday nil          ; start on weekday none
   org-agenda-span 14                       ; set the org-days from now
   org-agenda-window-setup 'current-window  ; current-window to open the agenda view
   org-startup-indented t

   ;; variables about the TODO Keywords
   org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                             (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                                       "PROJECT(p@)" "|" "CANCELLED(c@/!)" "|" "CLOSED(s@/!)"
                                       "PHONE" "METTING")))

   org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                  ("NEXT" :foreground "blue" :weight bold)
                                  ("DONE" :foreground "forest green" :weight bold)
                                  ("WAITING" :foreground "orange" :weight bold)
                                  ("HOLD" :foreground "magenta" :weight bold)
                                  ("CANCELLED" :foreground "forest green" :weight bold)
                                  ("CLOSED" :foreground "forest green" :weight bold)
                                  ("MEETING" :foreground "forest green" :weight bold)
                                  ("PHONE" :foreground "forest green" :weight bold)))

   org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                        ("WAITING" ("WAITING" . t))
                                        ("CLOSED" ("CLOSED" . t))
                                        ("HOLD" ("WAITING") ("HOLD" . t))
                                        ("DONE" ("WAITING") ("HOLD"))
                                        ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                        ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

   ;; variables about the org-priority
   ;; see the "Put First Things First"
   ;; -- A is for Urgent and Important
   ;; -- B is for Not Urgent but Important
   ;; -- C is for not Urgent and not Important
   ;; -- D is for Urgent but Not Important
   org-highest-priority ?A
   org-lowest-priority ?D
   org-default-priority ?B
   org-priority-faces (quote ((?A . (:foreground "#FF00FF" :weight bold))
                              (?B . (:foreground "green" :weight bold))
                              (?C . (:foreground "#00CCFF"))
                              (?D . (:foreground "#9900FF"))
                              ))

   ;; variables about the org-agenda files
   org-agenda-files '("~/work/Dropbox/life/gtd/")

   ;; variables about the org-capture directory
   org-capture-directory "~/work/Dropbox/life/capture/"
   org-default-notes-files (concat org-capture-directory "/todo.org")

   ;; capture templates
   org-capture-templates
   (quote (("t" "Todo" entry (file+headline (concat org-capture-directory "/todo.org") "Plans")
            "* TODO %?\n%U\n" :clock-in t :clock-resume t)
           ("m" "Meeting" entry (file+headline (concat org-capture-directory "/todo.org") "Meetings")
            "* MEETING with %? :MEETING:\n%U\n" :clock-in t :clock-resume t)
           ("p" "Phone call" entry (file+headline (concat org-capture-directory "/todo.org") "Phone Call")
            "* PHONE %? :PHONE:\n%U\n" :clock-in t :clock-resume t)

           ("b" "Books" item (file+headline (concat org-capture-directory "/todo.org") "Books")
            "- 需要阅读《%?》    %U\n")
           ("f" "Films" item (file+headline (concat org-capture-directory "/todo.org") "Films")
            "- 优秀电影、纪录片《%?》    %U\n")
           ("j" "Jounery" entry (file+headline (concat org-capture-directory "/todo.org") "Jounery")
            "** 计划去<%?>地方旅行\nSCHEDULED: %u\n")
           ("e" "Entertainment" entry (file+headline (concat org-capture-directory "/todo.org") "Entertainment")
            "** <%?>: %u")

           ;; note file
           ("n" "Note" entry (file (concat org-capture-directory "/note.org"))
            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

           ;; the new habit
           ("h" "Habit" entry (file (concat org-capture-directory "/habit.org"))
            "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")

           ;; diary file
           ("d" "Diary" entry (file+datetree (concat org-capture-directory "/diary.org"))
            "* %?\n%U\n" :clock-in t :clock-resume t)
           ))

   )
)


(defun sweatlake-org/init-ox-publish ()
  (use-package ox-publish
    :init
    (progn
      (setq org-publish-project-alist
            (quote ((
                     ("notes-org"
                      :base-directory "~/work/Dropbox/wiki/org/"
                      :base-extension "org"
                      :publishing-directory "~/work/Dropbox/wiki/site/"
                      :recursive t
                      :publishing-function org-html-publish-to-html
                      :auto-sitemap t
                      )

                     ("notes-static"
                      :base-directory "~/work/Dropbox/wiki/org/"
                      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
                      :publishing-directory "~/work/Dropbox/wiki/site/"
                      :recursive t
                      :publishing-function org-publish-attachment
                      )

                     ("notes" :components ("notes-org" "notes-static"))
                     ))))
      )))


(defun sweatlake-org/init-org-octopress ()
  (use-package org-octopress
    :commands (org-octopress org-octopress-setup-publish-project)
    :init
    (progn
      (evilified-state-evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))
      (setq org-blog-dir "~/work/Dropbox/blog/")
      (setq org-octopress-directory-org-top (concat org-blog-dir "source"))
      (setq org-octopress-directory-org-posts (concat org-blog-dir "source/_posts"))
      (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))

      (defun org-octopress/org-save-and-export ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      )))


(defun sweatlake-org/init-blog-admin ()
  (use-package blog-admin
    :config (progn
              (setq blog-admin-backend-type 'hexo
                    blog-admin-backend-path "~/work/Dropbox/blog"
                    blog-admin-backend-new-post-in-drafts t
                    blog-admin-backend-new-post-with-same-name-dir nil
                    blog-admin-backend-hexo-config-file "_config.yml")
              (spacemacs/set-leader-keys "abb" 'blog-admin-start)
              (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
              (setq blog-admin-backend-hexo-template-org-post  ;; post模板
                    "#+TITLE: %s\n#+AUTHOR: 張榮\n#+EMAIL: hanzejl@gmail.com\n#+DATE: %s\n#+LAYOUT: post\n#+TAGS:\n#+CATEGORIES:\n#+DESCRIPTON: \n"))))

;;; packages.el ends here
