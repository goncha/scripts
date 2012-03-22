;; dotemacs.el

;;; setup load-path
(require 'cl)

(setq gc-emacs-directory (file-name-directory
			  (file-chase-links load-file-name)))
(setq load-path
      (append
       (remove-if-not 'file-directory-p
		      (cddr (directory-files
			     gc-emacs-directory
			     t)))
       load-path))


;;; basic
(setq confirm-kill-emacs 'y-or-n-p)
(setq make-backup-files nil)
(setq uniquify-buffer-name-style 'post-forward)
(setq visible-bell nil)
(setq Info-directory-list
      `(,(concat (file-name-directory (file-chase-links load-file-name)) "info")
	,@Info-default-directory-list))
;(setq tramp-shell-prompt-pattern "^.*[#$%>] *")

(fset 'yes-or-no-p 'y-or-n-p)

(if window-system
    (tool-bar-mode 0))
(column-number-mode 1)
(global-font-lock-mode 1)
(iswitchb-mode 1)


;;;; unleash some disabled functions
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-defun 	 'disabled nil)
(put 'narrow-to-page 	 'disabled nil)
(put 'narrow-to-region 	 'disabled nil)
(put 'capitalize-region	 'disabled nil)
(put 'downcase-region	 'disabled nil)
(put 'upcase-region	 'disabled nil)


;;;; F1 to help, C-h to delete backward
;; (global-set-key (kbd "<f1>") 'help-command)
;; (global-set-key "\C-h" 'delete-backward-char)


;;;; gui
(case window-system
 ;; Windows
 ((w32)
  ;; setup default frame style
  (setq default-frame-alist
	'((vertical-scroll-bars . nil)
	  (background-color . "AliceBlue")
	  (font . "-raster-ProggyCleanSZ-normal-r-normal-normal-10-75-96-96-c-*-iso8859-1")))

  ;; use UNIX eol style
  ;; (setq default-buffer-file-coding-system 'utf-8-unix)
  
  ;; PuTTY's plink is default tramp method
  (setq tramp-default-method "plink")

  ;; browse-url
  (setq browse-url-browser-function 'browse-url-firefox)
  (setq browse-url-firefox-program "C:\\Program Files\\Mozilla Firefox\\firefox.exe"))

 ;; X11 in .Xresources
 ((x)
  (setq default-frame-alist
	'((vertical-scroll-bars nil)
	  (background-color . "AliceBlue")
	  (font . "-unknown-ProggyCleanTT-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")))
  ;; C-z freeze emacs under dwm, ooh damn dynamic window managers
  (global-unset-key (kbd "C-z")))
 )

;;;; start server
(server-start)


;;; Modules
;;;; abbrev
;; (set-default 'abbrev-mode t)

;;;; dired
(autoload 'wdired-change-to-wdired-mode "wdired")
(eval-after-load 'dired
  '(progn
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))

;;;; misc
;; (defun gc:insert-date (format)
;;   "Wrapper around format-time-string"
;;   (interactive "MFormat:")
;;   (insert (format-time-string format)))

;; (defun gc:insert-standard-date ()
;;   "Inserts standard date time string"
;;   (interactive)
;;   (gc:insert-date "%c"))

(defun gc-move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place"
  (interactive "p")
  (let ((col (current-column))
	(n (if (null n) 1 n)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (forward-char col)))

(defun gc-move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (gc-move-line (if (null n) -1 (- n))))

(defun gc-move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (gc-move-line n))

(global-set-key (kbd "M-<down>") 'gc-move-line-down)
(global-set-key (kbd "M-<up>") 'gc-move-line-up)

;;;; editing sexp
(require 'paredit)

(defun gc-enhance-lisp-mode-map (m)
  (define-key m (kbd "\"")
    'paredit-doublequote)
  (define-key m (kbd "(")
    'paredit-open-parenthesis)
  (define-key m (kbd ")")
    'paredit-close-parenthesis)
  (define-key m (kbd "[")
    'paredit-open-bracket)
  (define-key m (kbd "]")
    'paredit-close-bracket)
  (define-key m (kbd "C-h")
    'paredit-backward-delete)
  (define-key m (kbd "DEL")
    'paredit-backward-delete)
  (define-key m (kbd "C-k")
    'paredit-kill)
  (define-key m (kbd "M-k")
    'kill-line)
  (define-key m (kbd "C-(")
    'paredit-backward-slurp-sexp)
  (define-key m (kbd "C-)")
    'paredit-forward-slurp-sexp)
  (define-key m (kbd "C-{")
    'paredit-backward-barf-sexp)
  (define-key m (kbd "C-}")
    'paredit-forward-barf-sexp)
  (define-key m (kbd "M-n")
    'forward-sexp)
  (define-key m (kbd "M-p")
    'backward-sexp)
  (define-key m (kbd "C-t")
    'transpose-sexps)
  (define-key m (kbd "C-M-t")
    'transpose-chars))

;; enhance lisp-mode-shared-map for all lisp-like lang
(gc-enhance-lisp-mode-map lisp-mode-shared-map)
(define-key lisp-mode-shared-map (kbd "RET")
  'paredit-newline)

;; enhance inferior-scheme-mode-map
(add-hook 'inferior-scheme-mode-hook
	  #'(lambda ()
	      (gc-enhance-lisp-mode-map inferior-scheme-mode-map)))

;; nice M-: to input expressions
(gc-enhance-lisp-mode-map read-expression-map)

;; advanced paren matching  
(require 'mic-paren)
(paren-activate)

;; helper key mapping
(defun gc-byte-compile-buffer-file (&optional load)
  "Byte compile current buffer's file.
With prefix arg, `load' the file after compiling.
The value is non-nil if there were no error, nil if errors."
  (interactive "P")
  (unless buffer-file-name
    (save-buffer))
  (byte-compile-file buffer-file-name load))

(defun gc-eval-and-replace ()
  "Replace the preceding sexp with its value"
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
	      (current-buffer))
    (error (message "Invalid expresion")
	   (insert (current-kill 0)))))

(defun gc-toggle-selective-display ()
  "Toggle selective display of a buffer"
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun gc-dash-to-column (to-column)
  (interactive "P")
  (let ((inserts (- to-column (current-column))))
    (if (> inserts 0)
	(dotimes (i inserts) (insert "-")))))


;;;; anything
(autoload 'anything "anything")
(global-set-key (kbd "M-SPC") 'anything)
(eval-after-load 'anything
  (progn
    (load "anything-config")))


;;;; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-lisp-symbol))


;;;; git
(autoload 'git-status "git")
(global-set-key (kbd "<f9>") 'git-status)


;;;; slime
(load-library "slime-autoloads")
(add-hook 'slime-load-hook
	  #'(lambda ()
	      (setq slime-use-autodoc-mode t)
	      (setq slime-use-highlight-edits-mode t)
	      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
	      (setq slime-fuzzy-completion-in-place nil)
	      (setq slime-complete-symbol*-fancy t)
	      (setq slime-net-coding-system 'utf-8-unix)

	      (setq common-lisp-hyperspec-root
		    (concat "file://" (expand-file-name "~/Documents/Lisp/HyperSpec/")))

	      (global-set-key (kbd "<f12>") 'slime-selector)

	      (define-key slime-mode-map (kbd "TAB")
		'slime-indent-and-complete-symbol)

	      ))
(slime-setup '(slime-fancy
	       slime-fancy-inspector
	       slime-tramp))

(setq slime-lisp-implementations
      `((sbcl
	 (,(expand-file-name "~/local/bin/sbcl")
	  ;; "--core" ,(expand-file-name "~/local/opt/sbcl/lib/sbcl/sbcl.core")
	  "--dynamic-space-size" "200"
	  ;; "--userinit" ,(expand-file-name "~/lisp/user-init.lisp")
	  )
	 :coding-system utf-8-unix)
	(ccl
	 (,(expand-file-name "~/local/bin/lx86cl64")
	  ;; "-I" ,(expand-file-name "~/local/opt/ccl/lx86cl64.image")
	  "-K" "utf-8" "-R" "200000000"
	  )
	 :coding-system utf-8-unix)
	(cmucl
	 (,(expand-file-name "~/local/bin/lisp")
	  "-dynamic-space-size" "200"
	  )
	 :coding-system utf-8-unix)))


;;;; yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (concat gc-emacs-directory "yasnippet/snippets"))


;;;; web
(autoload 'htmlize-file "htmlize" "" t)
(autoload 'htmlize-many-file "htmlize" "" t)
(autoload 'htmlize-many-files-dired "" t)

(autoload 'js2-mode "js2" "JavaScript mode" t)
(setq auto-mode-alist (cons '("\\.js$" . js2-mode)
			    auto-mode-alist))


;;;; autoinsert
(auto-insert-mode 1)
(setq auto-insert-alist
      '(
        ((html-mode . "HTML")
         "Page title: "
	 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" \n
	 "<html>" \n
	 "<head>" \n
	 "<title>" str "</title>" \n
	 "</head>\n\n<body>\n" _ "\n</body>\n</html>\n")
	((python-mode . "Python")
	 nil
	 "# -*- coding: utf-8 -*-" \n
	 _ \n
	 "# Local Variables: **" \n
	 "# comment-column: 56 **" \n
	 "# indent-tabs-mode: nil **" \n
	 "# python-indent: 2 **" \n
	 "# End: **")
	((js2-mode . "JavaScript")
	 nil
	 "// -*- coding: utf-8 -*-" \n
	 _ \n
	 "// Local Variables: **" \n
	 "// comment-column: 56 **" \n
	 "// indent-tabs-mode: nil **" \n
	 "// js2-basic-offset: 2 **" \n
	 "// End: **")))

;;;; global keys
(global-set-key (kbd "<f5>")    'eval-region)
(global-set-key (kbd "<f6>")    'gc-byte-compile-buffer-file)
(global-set-key (kbd "<f7>")    'gc-eval-and-replace)
(global-set-key (kbd "<f8>")    'yas/compile-bundle)
(global-set-key (kbd "<f10>")   'gc-toggle-selective-display)
(global-set-key (kbd "<f11>")   'imenu)
(global-set-key (kbd "<f12>")   'browse-url-of-file)

;;;; global hook
(setq gc-delete-trailing-whitespaces-modes
      '(emacs-list-mode
	python-mode))
(add-hook 'before-save-hook
	  #'(lambda ()
	      (when (find major-mode gc-delete-trailing-whitespaces-modes)
		(delete-trailing-whitespace))))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;; Local Variables: **
;; mode: outline-minor **
;; comment-column: 56 **
;; End: **
