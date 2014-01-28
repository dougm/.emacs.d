(el-get 'sync
        '(ruby-mode
          flymake-ruby))
(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; wtf python-mode has: (defvar ruby-indent-level nil)
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
