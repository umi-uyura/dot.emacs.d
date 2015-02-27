;; mac-open-folder.el --- Open the folder in a variety of ways

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
