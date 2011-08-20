(setq anything-sources (list))

(defun anything-c-append-source (source)
  "Append source to `anything-sources'"
  (setq anything-sources
	(nconc anything-sources (list source))))

;;;; Match functions

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' matches the
filename (without directory part) of CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' matches the directory
part of CANDIDATE (a file)."
  (let ((dir (file-name-directory candidate)))
    (when dir
      (string-match anything-pattern dir))))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' matches CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))


;;;; Buffers

(defun anything-c-buffer-list ()
  "Return the list of names of buffers with the `anything-buffer'
and hidden buffers filtered out.  The first buffer in the list
will be the last recently used buffer that is not the current
buffer."
  (let ((buffers (remove-if (lambda (name)
                              (or (equal name anything-buffer)
                                  (eq ?\  (aref name 0))))
                            (mapcar 'buffer-name (buffer-list)))))
    (append (cdr buffers) (list (car buffers)))))

(anything-c-append-source
 '((name . "Buffers")
   (candidates . anything-c-buffer-list)
   (volatile)
   (type . buffer)))

;;;; File name history

(anything-c-append-source
 '((name . "File Name History")
   (candidates . file-name-history)
   (match . (anything-c-match-on-file-name
	     anything-c-match-on-directory-name))
   (type . file)))

;;;; Files in current dir

(anything-c-append-source
 '((name . "Files from Current Directory")
   (init . (lambda ()
	     (setq anything-c-default-directory
		   default-directory)))
   (candidates . (lambda ()
		   (directory-files
		    anything-c-default-directory)))
   (volatile)
   (type . file)))

;;;; Man pages

(defvar anything-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke anything with this
source.")

(anything-c-append-source
 `((name . "Manual Pages")
   (candidates . (lambda ()
		   (if anything-c-man-pages
		       anything-c-man-pages
		     ;; XEmacs doesn't have a woman :)
		     (setq anything-c-man-pages
			   (condition-case nil
			       (progn
				 (require 'woman)
				 (woman-file-name "")
				 (sort (mapcar 'car
					       woman-topic-all-completions)
				       'string-lessp))
			     (error nil))))))
   (action . (("Show with Woman" . woman)))
   (requires-pattern . 2)))

;;;; Command history

(anything-c-append-source
 '((name . "Command History")
   (candidates . (lambda ()
		   (mapcar 'prin1-to-string
			   command-history)))
   (volatile)
   (type . sexp)))

;;;; Emacs commands

(anything-c-append-source
 '((name . "Emacs Commands")
   (candidates . (lambda ()
		   (let (commands)
		     (mapatoms (lambda (a)
				 (if (commandp a)
				     (push (symbol-name a)
					   commands))))
		     (sort commands 'string-lessp))))
   (volatile)
   (type . command)
   (requires-pattern . 2)))

;;;; Bookmarks

(anything-c-append-source
 '((name . "Bookmarks")
   (init . (lambda ()
	     (require 'bookmark)))
   (candidates . bookmark-all-names)
   (action . (("Jump to Bookmark" . bookmark-jump)))))

;;;; Locate

(defvar anything-c-locate-options (if (eq system-type 'darwin)
                                      '("locate")
                                    '("locate" "-i" "-r"))
  "A list where the `car' is the name of the locat program
followed by options.  The search pattern will be appended, so the
\"-r\" option should be the last option.")

(anything-c-append-source
 '((name . "Locate")
   (candidates . (lambda ()
		   (apply 'start-process "locate-process" nil
			  (append anything-c-locate-options
				  (list anything-pattern)))))
   (type . file)
   (requires-pattern . 3)
   (delayed)))

;;;; Calculation Result

(anything-c-append-source
 '((name . "Calculation Result")
   (requires-pattern)
   (match (lambda (candidate) t))
   (candidates  "dummy")
   (filtered-candidate-transformer
    . (lambda (candidates source)
	(list
	 (condition-case nil
	     (calc-eval anything-pattern)
	   (error "error")))))
   (volatile)
   (action ("Do Nothing" . ignore))))

;;;; Helper Functions

(defun anything-c-compose (arg-lst func-lst)
  "Call each function in FUNC-LST with the arguments specified in
ARG-LST.  The result of each function will be the new `car' of
ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defvar anything-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git") (or "/" eol))
       ;; Boring files
       (and (or ".class" ".la" ".o") eol)))
  "File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used.")

(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                          ;; fall back to default on XEmacs
                          'default)))
              (if (string-match anything-c-boring-file-regexp file)
                  (setq file (propertize file 'face face))))
            file)
          files))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
skipped."
  (let (filtered-files)
    (loop for file in files
          do (when (not (string-match anything-c-boring-file-regexp file))
               (push file filtered-files))
          finally (return (nreverse filtered-files)))))

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with $HOME."
  (mapcar (lambda (file)
            ;; replace path of HOME directory in paths with the string <home>
            (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                  (getenv "HOME"))))
              (if (string-match home file)
                  (cons (replace-match "$HOME" nil nil file) file)
                file)))
          files))

(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
    actions))

(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "el")
          (string= (file-name-extension candidate) "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "htm")
          (string= (file-name-extension candidate) "html"))
      (append actions '(("Browse with Browser" . browse-url)))
    actions))

(defun anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

(defun anything-c-delete-file (file)
  "Delete the given file after querying the user.  Ask to kill
buffers associated with that file, too."
  (if (y-or-n-p (format "Really delete file %s? " file))
      (progn
        (let ((buffers (anything-c-file-buffers file)))
          (delete-file file)
          (dolist (buf buffers)
            (when (y-or-n-p (format "Kill buffer %s, too? " buf))
              (kill-buffer buf)))))
    (message "Nothing deleted.")))

;;;; Type Attributes

(setq anything-type-attributes
      `((buffer
         (action
          ,@(if pop-up-frames
                '(("Switch to buffer other window" .
		   switch-to-buffer-other-window)
                  ("Switch to buffer" . switch-to-buffer))
              '(("Switch to buffer" . switch-to-buffer)
                ("Switch to buffer other window" .
		 switch-to-buffer-other-window)
                ("Switch to buffer other frame" .
		 switch-to-buffer-other-frame)))
          ("Display buffer" . display-buffer)
          ("Kill buffer" . kill-buffer)))
        (file
         (action
          ,@(if pop-up-frames
                '(("Find file other window" . find-file-other-window)
                  ("Find file" . find-file))
              '(("Find file" . find-file)
                ("Find file other window" . find-file-other-window)
                ("Find file other frame" . find-file-other-frame)))
          ("Open dired in file's directory" . anything-c-open-dired)
          ("Delete file" . anything-c-delete-file)
          ("Open file externally" . anything-c-open-file-externally)
          ("Open file with default tool" . anything-c-open-file-with-default-tool))
         (action-transformer . (lambda (actions candidate)
                                 (anything-c-compose
                                  (list actions candidate)
                                  '(anything-c-transform-file-load-el
                                    anything-c-transform-file-browse-url))))
         (candidate-transformer . (lambda (candidates)
                                    (anything-c-compose
                                     (list candidates)
                                     '(anything-c-shadow-boring-files
                                       anything-c-shorten-home-path)))))
        (command
	 (action ("Call interactively" .
		  (lambda (command-name)
		    (call-interactively (intern command-name))))
		 ("Describe command" .
		  (lambda (command-name)
		    (describe-function (intern command-name))))
		 ("Add command to kill ring" . kill-new)
		 ("Go to command's definition" . (lambda (command-name)
						   (find-function
						    (intern command-name))))))
        (sexp
	 (action ("Eval s-expression" . (lambda (c) (eval (read c))))
		 ("Add s-expression to kill ring" . kill-new))
	 (action-transformer . (lambda (actions candidate)
				 (anything-c-compose
				  (list actions candidate)
				  '(anything-c-transform-sexp-eval-command-sexp)))))))
