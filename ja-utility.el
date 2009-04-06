(defun duplicate-line ()
  "*Insert a copy of the current line below the current line."
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (insert ?\n)
      (insert-buffer-substring (current-buffer) start end))))

(defun query-replace-dir ()
  "run find on a directory, then run a dired query/replace on the found files"
  (interactive)
  (progn
    (find-dired 
     (expand-file-name (read-file-name "search under: "))
     (read-from-minibuffer "find args: " "-name *.rb -or -name *.html.erb"))
    (message "reading files... this may take a minute")
    (switch-to-buffer "*Find*")
    ;; *Find* buffer will update its mode line when it is done running.
    ;; wait for it
    (while (case (type-of mode-line-process)
             ('string (not (string-equal mode-line-process ":exit")))
             ('cons (not (string-equal (car mode-line-process) ":exit")))
             (t t))
      (sit-for 1))
    ;; ignore symlinks
    (dired-mark-symlinks nil)
    (dired-toggle-marks)
    (dired-do-query-replace-regexp
    (read-from-minibuffer "search for regexp: ")
    (read-from-minibuffer "replace with: "))))


(defmacro with-match-subs (re str args &rest body)
  `(progn (string-match ,re ,str)
          (let ,(match-sub-args str args)
            ,@body)))

(defun match-sub-args (str args)
  (mapcar #'(lambda (a) `(,(car a) (match-string ,@(cdr a) ,str))) args))

(defun do-last-command-in-buffer (name)
  (save-excursion
    (display-buffer name)
    (with-current-buffer name
      (erase-buffer)
      (comint-previous-input 1)
      (comint-send-input))))

(provide 'ja-utility)