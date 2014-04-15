(el-get 'sync
        '(paredit
          parenface))

(defun find-symbol-at-point ()
  "Find directly the function or variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (find-variable-other-window symb)
      (find-function-at-point))))

(defun file-basedir (file)
  "Base name of the file's directory name."
  (file-name-nondirectory (directory-file-name (file-name-directory file))))

(defun recompile-elc-on-save ()
  "Recompile elc when saving an elisp file and reload el-get package."
  (when (file-exists-p (byte-compile-dest-file buffer-file-name))
    (emacs-lisp-byte-compile)
    (let ((name (file-basedir buffer-file-name)))
      (when (el-get-read-package-status name)
        (el-get-reload name)))))

(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode t)
            (enable-paredit-mode)
            (add-hook 'after-save-hook 'recompile-elc-on-save nil t)
            (local-set-key (kbd "C-h C-f") 'find-function-at-point)
            (local-set-key (kbd "C-h C-v") 'find-variable-at-point)
            (local-set-key (kbd "C-c o") 'find-symbol-at-point)
            (let ((map paredit-mode-map))
              (define-key map (kbd "C-j") nil)
              (define-key map (kbd "M-<up>") nil)
              (define-key map (kbd "M-<down>") nil)
              )
            ))
