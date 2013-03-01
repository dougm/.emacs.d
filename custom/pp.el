(autoload 'puppet-mode "puppet-mode" "Major mode for editing Puppet code." t)
(add-to-list 'auto-mode-alist '("\.pp$" . puppet-mode))
(add-hook 'puppet-mode-hook 'esk-prog-mode-hook)
