(el-get 'sync
        '(rainbow-delimiters))

(defun find-symbol-at-point ()
  "Find directly the function or variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (find-variable-other-window symb)
      (find-function-at-point))))

(defun file-basedir (&optional file)
  "Base name of the file's directory name."
  (file-name-nondirectory (directory-file-name
                           (file-name-directory (or file buffer-file-name)))))

(defun recompile-elc-on-save ()
  "Recompile elc when saving an elisp file and reload el-get package."
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (emacs-lisp-byte-compile)
    (let ((name (file-basedir buffer-file-name)))
      (when (el-get-read-package-status name)
        (el-get-reload name)))))

(defun run-ert ()
  (interactive)
  (eval-buffer)
  (call-interactively 'ert))

(defun el-flycheck-hook ()
  (unless flycheck-emacs-lisp-load-path
    (if (string= "custom" (file-basedir))
        (setq-local flycheck-checkers '())
      (setq-local flycheck-emacs-lisp-load-path load-path)
      (setq-local flycheck-checkers '(emacs-lisp)))))

(add-hook 'ielm-mode-hook 'smartparens-strict-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode t)
            (smartparens-strict-mode t)
            (rainbow-delimiters-mode t)
            (add-hook 'after-save-hook 'recompile-elc-on-save nil t)
            (add-hook 'flycheck-mode-hook 'el-flycheck-hook t t)
            (local-set-key (kbd "C-h C-f") 'find-function-at-point)
            (local-set-key (kbd "C-h C-v") 'find-variable-at-point)
            (local-set-key (kbd "C-c o") 'find-symbol-at-point)
            (local-set-key (kbd "C-c .") 'run-ert)
            ))
