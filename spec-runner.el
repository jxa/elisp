;; spec-runner.el
;; quickly run rspec examples or entire spec files

(require 'ja-utility)

(defun rspec-run-file-drb ()
  "runs current spec file"
  (interactive)
  (rspec-run-drb nil))

(defun rspec-run-example-drb ()
  "runs previous 'it' block"
  (interactive)
  (rspec-run-drb t))

(defun rspec-run-example-in-console ()
  (interactive)
  (rspec-run-in-console t))

(defun rspec-run-file-in-console ()
  (interactive)
  (rspec-run-in-console nil))

(defun rspec-run-last-command ()
  "make *rspec* buffer visible and run most recent command"
  (interactive)
  (do-last-command-in-buffer "*rspec*"))

(defun rspec-run-drb (with-example-p)
  "run file or example in an emacs buffer"
  (save-excursion
    (let ((rspec-buffer (get-buffer-create "*rspec*")))
      (display-buffer rspec-buffer)
      (save-window-excursion
        (let* ((file-info (rspec-get-file-info))
               (drb (if (spec-server-is-running 8989) " --drb " " "))
               (command (concat "spec" drb (second file-info))))
          (with-current-buffer rspec-buffer
            (erase-buffer)
            (setq default-directory (first file-info))
            (shell (current-buffer))
            (end-of-buffer)
                (if with-example-p
                (insert (concat command " -e \"" (third file-info) "\""))
              (insert command))
            (comint-send-input)))))))

(defun rspec-run-in-console (with-example-p)
  "run specs in given console buffer
   If with-example-p is non-nil it will
   run only the previous 'it' block"
  (save-excursion
    (let* ((file-info (rspec-get-file-info))
           (type (rspec-type (second file-info)))
           (buffer-name (concat "*rspec " type "*"))
           (buffer-exists (bufferp (get-buffer buffer-name)))
           (buffer (get-buffer-create buffer-name)))
      (display-buffer buffer)
      (save-window-excursion
        (rspec-prepare-console (first file-info) buffer buffer-exists)
        (with-current-buffer buffer-name
          (erase-buffer)
          (end-of-buffer)
          (rspec-run-command (second file-info) (third file-info) with-example-p))))))

(defun rspec-prepare-console (dir buffer buffer-exists)
  "start a rails console in test mode
   or if one exists already, call reload!"
  (if buffer-exists
      (with-current-buffer buffer
        (end-of-buffer)
        (insert "reload!")
        (comint-send-input))
    (with-current-buffer buffer
      (setq default-directory dir)
      (shell (current-buffer))
      (sit-for 0.1)
      (insert "script/console test")
      (comint-send-input)
      (insert "require 'spec'")
      (comint-send-input))))

(defun rspec-run-command (file example with-example-p)
  (let* ((args (if (and example with-example-p) "-e" nil))
         (ex   (if with-example-p example nil))
         (command (concat 
                   "Spec::Runner::CommandLine.run(Spec::Runner::OptionParser.parse([\"" 
                   file "\", " (rspec-quote args) ", " (rspec-quote ex) "], $stderr, $stdout))")))
    (insert command)
    (comint-send-input)))

(defun rspec-quote (str)
  (if str (concat "\"" str "\"") "nil"))

(defun rspec-get-file-info ()
  "get current project directory, file name and spec definition (it '...' block)"
  (with-match-subs "\\(.+\\)/\\(spec/.+\\)"
                   (buffer-file-name)
                   ((dir 1) (file 2))
                   `(,(concat dir "/") ,file ,(rspec-current-it-block))))

(defun rspec-type (filename)
  (progn
    (string-match "spec/\\(unit\\|functional\\)/" filename)
    (match-string 1 filename)))

(defun rspec-current-it-block ()
  (if (re-search-backward "^ +it +\\(['\"]\\)\\(.*?\\)\\1" nil t)
      (match-string 2)
    nil))

(defun spec-server-is-running (port)
  "returns t if there is there a server accepting connections on given port. nil otherwise"
  (condition-case nil 
    (let* ((connection (open-network-stream "specs" nil "localhost" port))
           (status (process-status connection)))
      (delete-process connection)
      (eq 'open status))
    (file-error nil)))

(provide 'spec-runner)