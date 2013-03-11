(autoload 'thrift-mode "thrift-mode" "Major mode for editing Thrift code." t)
(add-to-list 'auto-mode-alist '("\.thrift$" . thrift-mode))
(add-hook 'thrift-mode-hook (lambda ()
            (setq thrift-indent-level 2)))
