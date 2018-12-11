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
;; added to `sweatlake-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `sweatlake-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `sweatlake-org/pre-init-PACKAGE' and/or
;;   `sweatlake-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst sweatlake-org-packages
  '((org :location built-in)
    org-pomodoro
    ox-hugo
    ))


(defun sweatlake-org/post-init-ox-hugo ()
  (use-package ox-hugo
    :config
    (progn
      (setq org-hugo-default-section-directory "post")
      ))
  )


(defun sweatlake-org/post-init-org-pomodoro ()
  (use-package org-pomodoro
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "p" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "p" 'org-pomodoro)
      (setq
       org-pomodoro-format "§·%s"
       org-pomodoro-short-break-format "♨·%s"
       org-pomodoro-long-break-format "❆·%s")

      (add-hook 'org-pomodoro-finished-hook
                (lambda ()
                  (sweatlake/terminal-notify-osx "Pomodoro Finished"
                                                 "Have a break!")))
      (add-hook 'org-pomodoro-short-break-finished-hook
                (lambda ()
                  (sweatlake/terminal-notify-osx "Short Break"
                                                 "Ready to Go?")))
      (add-hook 'org-pomodoro-long-break-finished-hook
                (lambda ()
                  (sweatlake/terminal-notify-osx "Long Break"
                                                 "Ready to Go?")))
      (add-hook 'org-pomodoro-killed-hook
                (lambda () (sweatlake/terminal-notify-osx
                            "Pomodoro Killed"
                            "One does not simply kill a pomodoro!")))))
  )

(defun sweatlake-org/post-init-org()
  (add-hook 'org-mode-hook 'turn-on-auto-fill)

  (with-eval-after-load 'org
    (progn
      (setq-default
       org-html-validation-link nil
       ;; some variables about the startup
       org-startup-with-inline-images nil       ; not show the inline images
       org-agenda-start-on-weekday nil          ; start on weekday none
       org-agenda-span 14                       ; set the org-days from now
       org-agenda-window-setup 'current-window  ; current-window to open the agenda view
       org-startup-indented t
       org-log-into-drawer t

       org-export-with-priority t
       org-use-tag-inheritance nil

       org-agenda-files '("~/Developer/earth/ocean/caterpillar/GTD")
       ;; variables about the org-capture directory

       org-capture-directory "~/Developer/earth/ocean/caterpillar/GTD/capture/"
       org-default-capture-file (concat org-capture-directory "inspiration.org")
       org-html-htmlize-output-type 'css
       )

      (defadvice org-html-paragraph (before org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long "
        "line without unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents
                (replace-regexp-in-string
                 (concat
                  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2"
                 origin-contents)))
          (ad-set-arg 1 fixed-contents)))

      ;; variables about the TODO Keywords
      (setq-default
       org-todo-keywords
       (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
               (sequence "WAITING(w@/!)" "HOLD(h@/!)"
                         "PROJECT(p@)" "|" "CANCELLED(c@/!)" "|" "CLOSED(s@/!)"
                         "METTING")))

       org-todo-keyword-faces
       (quote (("TODO" :foreground "red" :weight bold)
               ("NEXT" :foreground "blue" :weight bold)
               ("DONE" :foreground "forest green" :weight bold)
               ("WAITING" :foreground "orange" :weight bold)
               ("HOLD" :foreground "magenta" :weight bold)
               ("CANCELLED" :foreground "forest green" :weight bold)
               ("CLOSED" :foreground "forest green" :weight bold)
               ("MEETING" :foreground "forest green" :weight bold)
               ))
       )

      ;; variables about the org-priority
      ;; see the "Put First Things First"
      ;; -- A is for Urgent and Important
      ;; -- B is for Not Urgent but Important
      ;; -- C is for not Urgent and not Important
      ;; -- D is for Urgent but Not Important
      (setq-default
       org-highest-priority ?A
       org-lowest-priority ?D
       org-default-priority ?B
       org-priority-faces (quote ((?A . (:foreground "#FF00FF" :weight bold))
                                  (?B . (:foreground "green" :weight bold))
                                  (?C . (:foreground "#00CCFF"))
                                  (?D . (:foreground "#9900FF"))
                                  )))

      (setq org-clocktable-defaults
            '(:tcolumns 1 :properties("PRIORITY" "TAGS" "Effort") :formula % :sort (6 . ?N))

            org-clock-clocktable-default-properties
            '(:scope subtree :maxlevel 4)
            )

      (setq-default

       ;; capture templates
       org-capture-templates
       (quote (("b" "Books" item (file+headline org-default-capture-file "书籍")
                "- [ ] %U 想看书 *《%?》*")
               ("f" "Films" checkitem (file+headline org-default-capture-file "电影")
                "- [ ] %U 想看电影 *《%?》*")
               ("e" "Eats" checkitem (file+headline org-default-capture-file "美食")
                "- [ ] %U 想吃 *%?*")
               ("g" "Game" checkitem (file+headline org-default-capture-file "游戏")
                "- [ ] %U 想玩 *%?*")
               ("j" "Jounery" item (file+headline org-default-capture-file "旅行")
                "- [ ] %U 想去 *%?* 地方看看")
               ("o" "Others" item (file+headline org-default-capture-file "其他")
                "- [ ] %U 想到了件事情：%?")
               ))
       )

      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")
      (setq-default org-babel-results-keyword "results")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (latex . t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)))

      (setq org-confirm-babel-evaluate nil)
      (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

      ;; org-mode 設定
      (require 'org-crypt)

      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)

      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")

      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)

      (require 'ox-publish)
      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %背景框
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; {{ export org-mode in Chinese into PDF
      ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
      ;; and you need install texlive-xetex on different platforms
      ;; To install texlive-xetex:
      ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
      ;; }}
      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '(
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "xelatex -interaction nonstopmode -output-directory %o %f"
              "rm -fr %b.out %b.log %b.tex auto"))

      (setq org-latex-listings t)

      )))

;;; packages.el ends here
