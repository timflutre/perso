;; -*- mode: lisp  -*-

;; Aim: customize Emacs
;; Person: Timothée Flutre [cre,aut]
;; Versioning: https://github.com/timflutre/perso

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/index.html
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/Emacs-Initialization.html

;; emacs --daemon
;; emacsclient -e '(kill-emacs)'

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-user-dir "~/.emacs.d/lisp/") ;; http://emacs.stackexchange.com/a/16606
(package-initialize)
;; https://emacs.stackexchange.com/a/27300/9680
(setq package-selected-packages
      '(
        auctex
        auto-complete
        elpy
        ein
        ess
        ;; julia-mode ;; use ESS instead, conflict with both
        jupyter
        markdown-mode
        polymode
        xclip
        ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "UTF-8")
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-mode 1)
 '(global-font-lock-mode t nil (font-lock))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(size-indication-mode t))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message
			(concat "Hello " (user-full-name) "!"
							"\n* system type: " (symbol-name system-type)
              "\n* Emacs version: " emacs-version
							"\n* system name: " system-name))

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Bars.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Feature-Testing.html
(if (display-graphic-p)
    (menu-bar-mode 1)
  (menu-bar-mode 0))

;; http://www.emacswiki.org/emacs/DotEmacsDotD
;; http://www.emacswiki.org/emacs/LoadPath
;; (add-to-list 'load-path "~/.emacs.d/")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; http://www.emacswiki.org/emacs/ShowParenMode
(setq show-paren-delay 0)
(show-paren-mode 1)

;; http://stackoverflow.com/a/64558/597069
(setq x-select-enable-clipboard t)

;; http://stackoverflow.com/a/14659015/597069
(when (and (executable-find "xclip")
           (require 'xclip nil 'noerror))
  (xclip-mode 1))

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/TeX-Print.html
(setq latex-run-command "pdflatex")

;; http://www.emacswiki.org/emacs/AutoCompressionMode
(auto-compression-mode 1)

;; http://www.emacswiki.org/emacs/DiaryMode
(setq european-calendar-style t)

;; http://www.emacswiki.org/emacs/InputMethods
(setq default-input-method "Tex")

;; http://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-default-style "bsd"
      c-basic-offset 2
      tab-width 2
      indent-tabs-mode t)
(setq sh-basic-offset 2
      sh-indentation 2)

;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode t)

;; http://www.emacswiki.org/emacs/EasyPG
(require 'epa-file)
(epa-file-enable)

;; http://unix.stackexchange.com/a/186565/34919
(defun term-new ()
  (interactive)
  (command-execute 'term)
  (setq-default truncate-lines nil)
  (if (not (boundp 'term-number))
      (defvar term-number 1 "term index in the current emacs session") )
  (rename-buffer (concat "*terminal*<" (int-to-string term-number) ">"))
  (setq term-number (+ 1 term-number))
  )

;;----------------------------------------------------------------------------

;;;; START config Auto-Complete
;; https://github.com/auto-complete/auto-complete

(require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete-20170124.1845/dict/")
(ac-config-default)

;; END config Auto-Complete

;;----------------------------------------------------------------------------

;;;; START config ESS
;; http://ess.r-project.org/

;; https://github.com/emacs-ess/ESS/issues/200
(require 'cl-lib)
(require 'cl)

;; http://ess.r-project.org/Manual/ess.html#Installation
;; use the system-wide version (in /usr/share/emacs):
;; or use the version from M-x list-packages:
;; (add-to-list 'load-path "~/.emacs.d/lisp/ess-20180109.1719/lisp/")
(require 'ess-site)

(add-to-list 'auto-mode-alist '("\\.jl" . ess-julia-mode))

;; tips from http://emacswiki.org/emacs/EmacsSpeaksStatistics
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session

;; http://cran.r-project.org/doc/manuals/R-ints.html#R-coding-standards
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'GNU 'quiet)
            ;; Because
            ;;                                 DEF GNU BSD K&R C++
            ;; ess-indent-level                  2   2   8   5   4
            ;; ess-continued-statement-offset    2   2   8   5   4
            ;; ess-brace-offset                  0   0  -8  -5  -4
            ;; ess-arg-function-offset           2   4   0   0   0
            ;; ess-expression-offset             4   2   8   5   4
            ;; ess-else-offset                   0   0   0   0   0
            ;; ess-close-brace-offset            0   0   0   0   0
            (add-hook 'local-write-file-hooks
                      (lambda ()
                        (ess-nuke-trailing-whitespace)))))
(setq ess-nuke-trailing-whitespace-p 'ask)
;; or even
;; (setq ess-nuke-trailing-whitespace-p t)

;; https://stat.ethz.ch/pipermail/ess-help/2013-June/009094.html
;; https://stat.ethz.ch/pipermail/ess-help/2011-December/007342.html
(setq ess-roxy-template-alist
      (list (cons "description" "")
            (cons "details" "")
            (cons "param" "")
            (cons "return" "")
            (cons "author" "Timothee Flutre")
            (cons "export" "")))

;; https://github.com/emacs-ess/ESS/issues/760
(define-key ess-mode-map (kbd "_") 'ess-insert-assign)
(define-key inferior-ess-mode-map (kbd "_") 'ess-insert-assign)

;; http://www.emacswiki.org/emacs/ESSAuto-complete
;; (setq ess-use-auto-complete t)

;; END config ESS

;; https://emacs.stackexchange.com/a/27419
(defun rmd-insert-chunk (header)
  "Insert an R chunk."
  (interactive "sHeader: ")
	(if (equal "" header)
			(insert (concat "```{r}\n\n```"))
		(insert (concat "```{r " header "}\n\n```")))
  (forward-line -1))

;; execute (all) R chunks at once from an Rmd document
;; http://stackoverflow.com/a/40966640/597069
(eval-when-compile
  (require 'polymode-core)  ;; SO format :('
  (defvar pm/chunkmode))
(declare-function pm-map-over-spans "polymode-core")
(declare-function pm-narrow-to-span "polymode-core")

(defun rmd-send-chunk ()
  "Send current R chunk to ESS process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode) ;;'
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (ess-eval-region (point) (point-max) nil nil 'R)))) ;;'

(defun rmd-send-buffer (arg)
  "Send all R chunks in buffer to ESS process. With prefix, send chunks above."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'rmd-send-chunk (point-min) ;;'
       ;; adjust this point to send prior regions
       (if arg (point) (point-max))))))

;; clear R console
;; http://stackoverflow.com/a/3450038/597069
;; (defun clear-shell ()
;;    (interactive)
;;    (let ((old-max comint-buffer-maximum-size))
;;      (setq comint-buffer-maximum-size 0)
;;      (comint-truncate-buffer)
;;      (setq comint-buffer-maximum-size old-max)))

;;----------------------------------------------------------------------------

;;;; START config AUCTeX
;; http://www.gnu.org/software/auctex/
;; http://www.emacswiki.org/emacs/AUCTeX

;; (load "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX/auctex.el" nil t t)
;; (load "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX/preview-latex.el" nil t t)
;; (add-to-list 'load-path "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t) ;; http://superuser.com/a/452409

;; END config AUCTeX

;;----------------------------------------------------------------------------

;;;; START config org-mode (version >= 8)
;; http://orgmode.org/
;; installation: http://emacs.stackexchange.com/q/17017/9680

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (shell . t)
   (python . t)
   ))
(setq org-confirm-babel-evaluate nil)
;; https://emacs.stackexchange.com/a/37695/9680

;; http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(setq org-src-fontify-natively t)

;; http://stackoverflow.com/a/19319921/597069
(setq org-src-tab-acts-natively t)

;; http://stackoverflow.com/a/9768225/597069
(setq org-src-preserve-indentation t)

;; http://raebear.net/comp/emacscolors.html
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:foreground "#191970"))))
 '(org-level-2 ((t (:foreground "#6495ed"))))
 '(org-level-3 ((t (:foreground "#191970"))))
 '(org-level-4 ((t (:foreground "#6495ed"))))
 '(org-level-5 ((t (:foreground "#191970"))))
 '(org-level-6 ((t (:foreground "#6495ed"))))
 '(org-level-7 ((t (:foreground "#191970"))))
 '(org-level-8 ((t (:foreground "#6495ed")))))

;; http://orgmode.org/manual/Workflow-states.html
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "ONEDAY" "CANCELLED")
        (sequence "àfaire" "débuté" "attente" "|" "fait" "unjour" "annulé"))
      )

;; http://orgmode.org/manual/Faces-for-TODO-keywords.html#Faces-for-TODO-keywords
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("àfaire" . org-warning)
        ("STARTED" . (:foreground "#ff8c00"))
        ("débuté" . (:foreground "#ff8c00"))
        ("WAITING" . (:foreground "#ff8c00"))
        ("attente" . (:foreground "#ff8c00"))
        ("CANCELED" . (:foreground "gray20"))
        ("annulé" . (:foreground "gray20")))
      )

(setq org-tags-exclude-from-inheritance '("projet" "project"))

;; http://tex.stackexchange.com/a/115081/11434
(add-to-list 'org-latex-packages-alist '("" "lmodern"))

(add-to-list 'org-latex-packages-alist '("" "parskip"))

;; http://thread.gmane.org/gmane.emacs.orgmode/94685/focus=94692
;; otherwise, keep it but use the grffile package
;; (add-to-list 'org-latex-packages-alist '("" "underscore"))

(add-to-list 'org-latex-packages-alist '("usenames,dvipsnames" "color"))
(add-to-list 'org-latex-packages-alist '("" "listings"))
(setq org-latex-listings t)
(setq org-latex-listings-options
      '(("breaklines" "true")
        ;; ("breakatwhitespace" "true") ;can lead to too-long lines
        ;; ("breakautoindent" "true")
        ("linewidth" "\\textwidth")
        ("showspaces" "false")
        ("showstringspaces" "false")
        ("showtabs" "false")
        ("tabsize" "2") ;instead of 8
        ("basicstyle" "\\ttfamily") ;looks like verbatim
        ;; ("numbers" "left") ;bad copy-paste for sh from pdf
        ("numberstyle" "\\footnotesize")
        ("stepnumber" "1")
        ("numbersep" "5pt")
        ("captionpos" "b")
        ;; ("frame" "single")
        ("backgroundcolor" "\\color[RGB]{248,248,248}") ;light grey
        ("keywordstyle" "\\color{Blue}")
        ("stringstyle" "\\color{BrickRed}")
        ("commentstyle" "\\color{ForestGreen}")
        ("columns" "fullflexible")) ;avoid adding spaces
      )
(setq org-log-done 'time)
(setq org-startup-truncated nil) ;so that lines longer than the screen are not truncated
(setq org-agenda-files (list "~/org/gtd_pro.gpg"
                             "~/org/gtd_perso.gpg"))
(setq org-agenda-include-diary t)

;; add "babel-francais" before "listings"
(add-to-list 'org-latex-packages-alist '("francais" "babel"))

;; END config org-mode

;;----------------------------------------------------------------------------

;;;; START config polymode
;; https://github.com/polymode/polymode

(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))

;; "M-n v v": eval all inner chunks in a region if region is active or the current chunk at point
;; "M-n v b": eval all inner chunks in a buffer
;; "M-n v u": eval from beginning of buffer till point
;; "M-n v d": eval from point till end of buffer

;; END config polymode

;;----------------------------------------------------------------------------

;;;; START config octave-mode
;; https://www.gnu.org/software/octave/doc/v4.0.3/Using-Octave-Mode.html
;; https://www.emacswiki.org/emacs/OctaveMode

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; END config octave-mode

;;----------------------------------------------------------------------------

;;;; START config elpy

(elpy-enable)

;; END config elpy

;;----------------------------------------------------------------------------

;;;; START config julia-mode

;; use ESS instead (M-x ess-julia-mode), conflict with both

;; (require 'julia-mode)

;;;; END config julia-mode
