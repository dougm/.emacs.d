(el-get 'sync
        '(python-mode
          jedi
          nose))
(require 'python-mode)
(require 'nose)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflymake" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook 'flymake-mode)

(add-to-list 'auto-mode-alist '("\\.sc$" . python-mode))

(add-hook 'python-mode-hook
(lambda ()
  (run-hooks 'prog-mode-hook)
  (local-set-key (kbd "RET") 'py-newline-and-indent)
  (local-set-key (kbd "C-c a") 'nosetests-all)
  (local-set-key (kbd "C-c m") 'nosetests-module)
  (local-set-key (kbd "C-c .") 'nosetests-one)
))

(setq jedi:key-related-names (kbd "C-c l"))
(setq jedi:key-show-doc (kbd "C-c d"))
(setq jedi:key-goto-definition (kbd "C-c o"))
(setq jedi:goto-definition-pop-marker (kbd "C-c -"))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
