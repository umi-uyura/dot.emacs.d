;; mac-open-folder.el --- Open the folder in a variety of ways

;;; @@ obsolete process-kill-without-query
;;; See: https://gordiustears.net/process-kill-without-query-obsolete/
(make-obsolete
 'process-kill-without-query
 "use `process-query-on-exit-flag' or `set-process-query-on-exit-flag'."
 "22.1")
(defun process-kill-without-query (process &optional flag)
  "Say no query needed if PROCESS is running when Emacs is exited.
Optional second argument if non-nil says to require a query.
Value is t if a query was formerly required."
  (let ((old (process-query-on-exit-flag process)))
    (set-process-query-on-exit-flag process nil)
    old))

;;; Finder でフォルダを開く
(defun open-finder()
  "open -a Finder.app CURRENT-DIRECTORY"
  (interactive)
  (process-query-on-exit-flag
   (start-process-shell-command "open folder in Finder" nil "open .")))

;;; Terminal でフォルダを開く
(defun open-terminal()
  "open -a Terminal.app CURRENT-DIRECTORY"
  (interactive)
  (let* ((cmd "open -a Terminal.app"))
    (process-kill-without-query
     (start-process-shell-command
      "Open directory" nil cmd default-directory))))

;;; iTerm2 でフォルダを開く
(defun open-iterm()
  "open -a iTerm.app CURRENT-DIRECTORY"
  (interactive)
  (let* ((cmd "open -a iTerm.app"))
    (process-kill-without-query
     (start-process-shell-command
      "Open directory" nil cmd default-directory))))
