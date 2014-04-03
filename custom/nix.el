(el-get 'sync
        '(nix-mode))

(add-hook 'nix-mode-hook
          (lambda ()
            (run-hooks 'prog-mode-hook)))
