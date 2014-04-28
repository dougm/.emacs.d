(el-get 'sync
        '(python-mode
          jedi
          nose))
(require 'python-mode)
(require 'nose)

(add-to-list 'auto-mode-alist '("\\.sc$" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (run-hooks 'prog-mode-hook)
            (setq jedi:complete-on-dot t)
            (jedi:setup)
            (let ((map python-mode-map))
              (define-key map [(meta i)] 'py-indent-line)
              (define-key map (kbd "TAB") 'py-indent-line-outmost))
            (let ((map jedi-mode-map))
              (define-key map (kbd "C-c .") nil)
              (define-key map (kbd "C-c d") 'jedi:show-doc)
              (define-key map (kbd "C-c o") 'jedi:goto-definition)
              (define-key map (kbd "C-c -") 'jedi:goto-definition-pop-marker))
            (local-set-key (kbd "RET") 'py-newline-and-indent)
            (local-set-key (kbd "C-c a") 'nosetests-all)
            (local-set-key (kbd "C-c m") 'nosetests-module)
            (local-set-key (kbd "C-c .") 'nosetests-one)))
