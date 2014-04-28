;;; install and load packages via https://github.com/dimitri/el-get
(el-get 'sync
        '(go-autocomplete
          go-def
          go-eldoc
          go-errcheck-el
          go-imports
          go-lint
          go-mode
          go-oracle
          go-projectile
          go-test
          yasnippet-go))

;;; ignore 'go test -c' files
(push ".test" completion-ignored-extensions)

;;; run gofmt before saving a buffer
(add-hook 'before-save-hook 'gofmt-before-save)

;;; key-bindings
(add-hook 'go-mode-hook
(lambda ()
  (local-set-key (kbd "C-c o") 'godef-jump)
  (local-set-key (kbd "C-c -") 'pop-tag-mark)
  (local-set-key (kbd "C-c d") 'godef-describe)
  (local-set-key (kbd "C-c a") 'go-test-current-project)
  (local-set-key (kbd "C-c m") 'go-test-current-file)
  (local-set-key (kbd "C-c .") 'go-test-current-test)
  (setq go-test-verbose t)
))
