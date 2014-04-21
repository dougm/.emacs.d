(el-get 'sync
        '(multi-term))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'term-mode-hook
          (lambda ()
            (setq autopair-dont-activate t
                  show-trailing-whitespace nil
                  whitespace-style nil
                  term-buffer-maximum-size 5000)
            (define-key term-raw-map (kbd "C-'") 'term-line-mode)
            (define-key term-mode-map (kbd "C-'") 'term-char-mode)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (term-set-escape-char ?\C-x)
            ))

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
(autoload 'vagrant-tramp-term "vagrant-tramp")

(setq multi-term-program "bash")

(defun term-cd-current-buffer ()
  "multi-term-next and cd to directory of current-buffer."
  (interactive)
  (let ((dir default-directory))
    (multi-term-next)
    (unless (string= dir default-directory)
      (term-send-raw-string (format "cd %s\n" dir))
      (term-send-raw-string "\C-l"))))

(global-set-key (kbd "C-c !") 'term-cd-current-buffer)
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-c v") 'vagrant-tramp-term)
