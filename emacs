(custom-set-variables
 '(column-number-mode t) ;; in the mode line
 '(size-indication-mode t) ;; in the mode line
 '(display-time-mode 1) ;; in the mode line
 '(display-time-day-and-date t) ;; in the mode line
 '(display-time-24hr-format t) ;; in the mode line
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(global-font-lock-mode t nil (font-lock))
)

;; http://www.emacswiki.org/emacs/LoadPath
;; http://www.emacswiki.org/emacs/DotEmacsDotD
(add-to-list 'load-path "~/.emacs.d/")

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; http://www.emacswiki.org/emacs/ShowParenMode
(setq show-paren-delay 0)
(show-paren-mode 1)

 ;; http://stackoverflow.com/a/64558/597069
(setq x-select-enable-clipboard t)

;; http://stackoverflow.com/a/14659015/597069
(add-to-list 'load-path "~/.emacs.d/elpa/xclip-1.3/")
(require 'xclip)
(xclip-mode 1)

(setq latex-run-command "pdflatex")

;; http://www.emacswiki.org/emacs/AutoCompressionMode
(auto-compression-mode 1)

;; http://www.emacswiki.org/emacs/DiaryMode
(setq european-calendar-style t)

;; http://www.emacswiki.org/emacs/InputMethods
(setq default-input-method "Tex")

;; config for code
;; http://www.emacswiki.org/emacs/NoTabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq c-default-style "bsd"
      c-basic-offset 2
      tab-width 2
      indent-tabs-mode t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;----------------------------------------------------------------------------

;; START config Auto-Complete
;; http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; END config Auto-Complete

;;----------------------------------------------------------------------------

;; START config ESS
;; http://ess.r-project.org/
(require 'ess-site)
;; (add-to-list 'load-path "~/src_ext/ess/ess-13.09-1/lisp/")
;; (load "ess-site")

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

;; tips source: http://www.emacswiki.org/emacs/ESSAuto-complete
;; (setq ess-use-auto-complete t)
;; END config ESS

;; clear R console (http://stackoverflow.com/a/3450038/597069)
;; (defun clear-shell ()
;;    (interactive)
;;    (let ((old-max comint-buffer-maximum-size))
;;      (setq comint-buffer-maximum-size 0)
;;      (comint-truncate-buffer)
;;      (setq comint-buffer-maximum-size old-max)))

;;----------------------------------------------------------------------------

;; START config AUCTeX
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

;; START config org-mode (version >= 8)
;; http://orgmode.org/
;; (setq load-path (cons "/home/tflutre/src_ext/ORG-MODE/org-7.8.03/lisp" load-path))
;; (setq load-path (cons "/home/tflutre/src_ext/ORG-MODE/org-7.8.03/contrib/lisp" load-path))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (sh . t)
   (python . t)
   ))
(setq org-confirm-babel-evaluate nil)

;; http://tex.stackexchange.com/a/115081/11434
(add-to-list 'org-latex-packages-alist '("" "lmodern"))

(add-to-list 'org-latex-packages-alist '("" "listings"))
(setq org-latex-listings t)
(setq org-latex-listings-options
      '(("breaklines" "true")
        ("basicstyle" "\\ttfamily") ;looks like verbatim
        ("columns" "fullflexible"))) ;avoid adding spaces
(setq org-log-done 'time)
(setq org-startup-truncated nil) ;so that lines longer than the screen are not truncated
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/perso.org"))
;; END config org-mode

;;----------------------------------------------------------------------------

;; START config python-mode
;; http://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; END config python-mode

;;----------------------------------------------------------------------------

;; START config polymode
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
;; END config polymode
