(package-initialize)

;;;(package-initialize)

;;;(require 'package)

;;; package-activated-list
;;package-activated-list
;;


(setq package-list '(auto-complete
		     ein
		     elpy
		     company
		     find-file-in-project
		     highlight-indentation
		     ivy
		     magit
		     git-commit
		     magit-popup
		     popup
		     pyvenv
		     realgud
		     loc-changes
		     load-relative
		     request
		     s
		     test-simple
		     websocket
		     with-editor
		     dash
		     async
		     yasnippet))


; list the repositories containing them
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24) (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("milkypostman" . "https://github.com/milkypostman/melpa"))

; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))






(ac-config-default)
(global-set-key (kbd "C-x g") 'magit-status)

(setq-default fill-column 120)

;;(setq server-socket-dir "~/.emacs.d/server")
(server-start)

(setq ispell-program-name "/usr/local/bin/ispell")

;; wtf. this was supposed to (in my mind) fix my ipython prompt. Need a better fix.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;;(add-to-list 'comint-preoutput-filter-functions 'ansi-color-process-output)

;; lel
(setq python-shell-prompt-detect-failure-warning nil)

(elpy-enable)
(require 'pyvenv)
(pyvenv-activate "~/virtualenv")
(elpy-use-ipython)
(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")
;;(setq python-shell-interpreter "ipython")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(require 'ein)


(defun my-restart-python-console ()
  "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
  (interactive)
  (kill-process "Python")
  (sleep-for 0.05)
  (kill-buffer "*Python*")
  (elpy-shell-send-region-or-buffer))


(load-library "realgud")

(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode -1))))
(add-hook 'python-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    ))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (realgud magit elpy ein auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
