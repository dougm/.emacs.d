;;; go-mode adds this to the end of the list, but doesn't work as advertised
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'gotest)
(add-to-list 'compilation-error-regexp-alist-alist
             '(gotest . ("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)))

(require 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook 'esk-prog-mode-hook)
(add-hook 'go-mode-hook (lambda ()
            (setq whitespace-line-column 120)))

(add-hook 'go-mode-hook
(lambda ()
  (local-set-key (kbd "C-c o") 'godef-jump)
  (local-set-key (kbd "C-c d") 'godef-describe)
  (local-set-key (kbd "C-c a") 'go-test-all)
  (local-set-key (kbd "C-c m") 'go-test-module)
  (local-set-key (kbd "C-c .") 'go-test-one)
))

(require 'go-autocomplete)
(require 'auto-complete-config)

(require 'go-flymake)
(require 'golint)

(defun go-build ()
  "compile project"
  (interactive)
  (compile "go build"))

(defun go-test-all ()
  "test project"
  (interactive)
    (compile "go test -v"))

(defun go-test-one ()
  "run a single test, closest to current point"
  (interactive)
    (compile (concat "go test -v -test.run " (go-current-test))))

(defun go-test-module ()
  "test module"
  (interactive)
    (compile (concat "go test -v -test.run '" (go-grep-tests) "'")))

(defun go-chk ()
  "gocheck project"
  (interactive)
    (compile "go test -gocheck.vv"))

(defun go-run ()
  "go run current buffer"
  (interactive)
    (compile (concat "go run " buffer-file-name)))

(defun go-current-test ()
  (save-excursion
    (re-search-backward
     "^func \\(Test[a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

;;; generate regex for all Tests in the current buffer
;;; note that go test has a '-file' flag that does not work as expected
(defun go-grep-tests ()
  (car (process-lines "bash" "-c"
                      (concat "egrep -o 'func Test[^(]+' " buffer-file-name " | awk '{print $2}' | paste -sd '|'"))))
