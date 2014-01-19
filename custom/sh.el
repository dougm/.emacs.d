(el-get 'sync
        '(flymake-shell))

(add-hook 'sh-mode-hook (lambda ()
            (setq sh-tab-width 2)
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)
            (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))
