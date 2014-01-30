(el-get 'sync
        '(flymake-shell))

(add-hook 'sh-mode-hook
          (lambda ()
            (whitespace-mode 0) ;; workaround bug in emacs-HEAD
            (setq sh-tab-width 2
                  sh-basic-offset 2
                  sh-indentation 2)
            (define-key sh-mode-map (kbd "RET") 'reindent-then-newline-and-indent)))
