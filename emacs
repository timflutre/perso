(custom-set-variables
 '(column-number-mode t) ;; in the mode line
 '(size-indication-mode t) ;; in the mode line
 '(display-time-mode 1) ;; in the mode line
 '(display-time-day-and-date t) ;; in the mode line
 '(display-time-24hr-format t) ;; in the mode line
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(global-font-lock-mode t nil (font-lock))
 '(show-paren-mode t)
)

(setq inhibit-startup-message t)

(add-to-list 'load-path "~/.emacs.d/")

(setq latex-run-command "pdflatex")

;; config for C/C++ code
(setq c-default-style "bsd"
          c-basic-offset 2
          tab-width 2
          indent-tabs-mode t)

;;----------------------------------------------------------------------------

;; START config Auto-Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; END config Auto-Complete

;;----------------------------------------------------------------------------

;; START config ESS
(require 'ess-site)
(add-to-list 'load-path "~/src_ext/ess/ess-13.09-1/lisp/")
(load "ess-site")

;; tips from http://emacswiki.org/emacs/EmacsSpeaksStatistics
(setq ess-eval-visibly-p nil) ;otherwise C-c C-r (eval region) takes forever
(setq ess-ask-for-ess-directory nil) ;otherwise you are prompted each time you start an interactive R session

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
;;(load "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX/auctex.el" nil t t)
;;(load "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX/preview-latex.el" nil t t)
;;(add-to-list 'load-path "/home/tflutre/src_ext/LATEX_PKGS/AUCTEX")
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t) ;; to have a pdf output for all LaTeX documents
;; END config AUCTeX

;;----------------------------------------------------------------------------

;; START config org-mode
(setq load-path (cons "/home/tflutre/src_ext/ORG-MODE/org-7.8.03/lisp" load-path))
(setq load-path (cons "/home/tflutre/src_ext/ORG-MODE/org-7.8.03/contrib/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
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
(require 'org-latex)
(add-to-list 'org-export-latex-packages-alist '("" "listings"))
(setq org-export-latex-listings t)
(setq org-export-latex-listings-options
      '(("breaklines")))
(setq org-log-done 'time)
(setq org-startup-truncated nil) ;so that lines longer than the screen are not truncated
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/perso.org"))
;; END config org-mode

;;----------------------------------------------------------------------------

;; START config python-mode
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
