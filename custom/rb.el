(el-get 'sync
        '(ruby-mode))
(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;; wtf python-mode has: (defvar ruby-indent-level nil)
(setq ruby-indent-level 2)
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
