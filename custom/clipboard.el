(defun pbpaste ()
  "Paste data from clipboard"
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pbcopy ()
  "Copy region to clipboard"
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))

(cond
   ((string-equal system-type "darwin")
        (global-set-key (kbd "C-x C-y") 'pbpaste)
        (global-set-key (kbd "C-x M-w") 'pbcopy)
    )
   ((string-equal system-type "gnu/linux")
        ;; todo
   )
)
