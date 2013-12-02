(require 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook 'esk-prog-mode-hook)
(add-hook 'go-mode-hook (lambda ()
            (setq whitespace-line-column 120)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c o") 'godef-jump)))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c d") 'godef-describe)))

(require 'go-autocomplete)
(require 'auto-complete-config)

(require 'go-flymake)
(require 'golint)

(defun go-build ()
  "compile project"
  (interactive)
  (compile "go build"))

(defun go-test ()
  "test project"
  (interactive)
    (compile "go test -v"))

(defun go-chk ()
  "gocheck project"
  (interactive)
    (compile "go test -gocheck.vv"))

(defun go-run ()
  "go run current buffer"
  (interactive)
    (compile (concat "go run " buffer-file-name)))
