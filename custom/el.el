(defun find-symbol-at-point ()
  "Find directly the function or variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (find-variable-other-window symb)
      (find-function-at-point))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (local-set-key (kbd "C-h C-f") 'find-function-at-point)
            (local-set-key (kbd "C-h C-v") 'find-variable-at-point)
            (local-set-key (kbd "C-c o") 'find-symbol-at-point)
            ))
