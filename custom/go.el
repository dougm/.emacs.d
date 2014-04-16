;;; install and load packages via https://github.com/dimitri/el-get
(el-get 'sync
        '(go-autocomplete
          go-def
          go-eldoc
          go-errcheck-el
          go-flymake
          go-imports
          go-lint
          go-mode
          go-oracle
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
  (local-set-key (kbd "C-c a") 'go-test-all)
  (local-set-key (kbd "C-c m") 'go-test-file)
  (local-set-key (kbd "C-c .") 'go-test-one)
))

(defun go-path ()
  "setenv GOPATH relative to current buffer"
  (interactive)
  (let* ((buf (or buffer-file-name default-directory))
         (dir (locate-dominating-file buf "src")))
    (if dir
        (let ((rel (file-relative-name buf dir))
              (env (getenv "GOPATH")))
          (if (and env (locate-file rel (split-string env path-separator t)))
              ;; buffer is in current GOPATH already
              (message (concat "GOPATH=" env))
            (let ((dir (expand-file-name dir)))
              (setenv "GOPATH" dir)
              (message (concat "set GOPATH=" dir)))))
      (message "unable to derive GOPATH"))))

(defvar go-project-files-ignore
  '("third_party")
  "A list of file patterns to ignore.")

(defun go-project-files ()
  (-filter (lambda (file)
             (and (string= (file-name-extension file) "go")
                  (not (-any? (lambda (pat)
                                (string-match pat file))
                              go-project-files-ignore))))
           (projectile-current-project-files)))

(defun go-rewrite--pattern-args (n)
  "Generate function signature pattern for go-rewrite"
  (let ((arg (string-to-char "a")))
    (mapconcat 'identity
               (mapcar (lambda (i)
                         (char-to-string (+ arg i)))
                       (number-sequence 0 (- n 1))) ",")))

(defun go-rewrite--pattern ()
  "Generate default pattern for go-rewrite"
  (let ((fn (go-eldoc--get-funcinfo)))
    (if fn
        (let* ((name (plist-get fn :name))
               (signature (go-eldoc--analyze-signature (plist-get fn :signature)))
               (args (go-eldoc--split-argument-type (plist-get signature :arg-type))))
          (format "x.%s(%s)" name (go-rewrite--pattern-args (length args))))
      (projectile-symbol-at-point))))

(defun go-rewrite (from to)
  "Apply Go rewrite rule to current project"
  (interactive
   (let ((pat (read-from-minibuffer "Pattern: " (go-rewrite--pattern))))
     (list pat (read-from-minibuffer "Replacement: " pat))))
  (projectile-with-default-dir (projectile-project-root)
    (projectile-save-project-buffers)
    (apply 'call-process "gofmt" nil (get-buffer-create "*Go Rewrite*") nil
           "-l" "-w" "-r" (format "%s -> %s" from to)
           (go-project-files))
    (auto-revert-buffers)))

(defun go-build ()
  "compile project"
  (interactive)
  (compile "go build"))

(defun go-run ()
  "go run current buffer"
  (interactive)
    (compile (concat "go run " buffer-file-name)))

(defun go-test-all ()
  "test project"
  (interactive)
    (go-test--run "go test -v"))

(defun go-test-one ()
  "run a single test, closest to current point"
  (interactive)
    (go-test--run (concat "go test -v -test.run " (go-test--current))))

(defun go-test-module ()
  "test module"
  (interactive)
    (go-test--run (concat "go test -v -test.run '" (go-test--find) "'")))

(defun go-test-file ()
  "test module, switching to ${name}_test.go file if needed"
  (interactive)
  (let ((is-test (string-match "_test\.go$" buffer-file-truename)))
    (unless is-test
      (ff-find-other-file))
    (go-test-module)
    (unless is-test
      (ff-find-other-file))))

;;; run 'go test ...' with our compile hook
(defun go-test--run (cmd)
  (add-hook 'compilation-start-hook 'go-test--compilation-hook)
  (compile cmd)
  (remove-hook 'compilation-start-hook 'go-test--compilation-hook))

;;; make local so we only match go test errors
(defun go-test--compilation-hook (p)
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^\t+\\([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2) ;; package testing
         ("^\tLocation:\t\\([^()\t\n]+\\):\\([0-9]+\\):?.*$" 1 2) ;; package testify
         ("^\\([^()\t\n]+.go\\):\\([0-9]+\\):\\([0-9]+\\)?:? .*$" 1 2 3) ;; compile error
         )))

;;; generate regex for all Tests in the current buffer
;;; note that go test has a '-file' flag that does not work as expected
(defun go-test--find ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward "^func \\(Test[a-zA-Z0-9_]+\\)" nil t)
        (let ((test (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
          (setq res (if res (concat res "|" test) test))))
      res)))

;;; find test closest to current point
(defun go-test--current ()
  (save-excursion
    (re-search-backward
     "^func \\(Test[a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))
