
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