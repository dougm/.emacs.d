(defun find-symbol-at-point ()
  "Find directly the function or variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (find-variable-other-window symb)
      (find-function-at-point))))

(defun recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (file-exists-p (byte-compile-dest-file buffer-file-name))
                (emacs-lisp-byte-compile)))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode t)
            (recompile-elc-on-save)
            (local-set-key (kbd "C-h C-f") 'find-function-at-point)
            (local-set-key (kbd "C-h C-v") 'find-variable-at-point)
            (local-set-key (kbd "C-c o") 'find-symbol-at-point)
            ))
