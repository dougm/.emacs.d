;; colors
(load-theme 'solarized-dark t)

;; lines
(require 'linum-off)
(setq linum-format " %d ")
(add-hook 'prog-mode-hook 'linum-on)
(column-number-mode t)

;; auto
(require 'autopair)
(autopair-global-mode)

(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)

(require 'compile)
(setq-default compilation-auto-jump-to-first-error t)

(require 'flymake)
(require 'flymake-cursor)

(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e") 'flymake-goto-next-error)))
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; whitespace
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face trailing lines-tail))
(setq-default whitespace-line-column 80)
(setq-default show-trailing-whitespace t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda () (set-fill-column 80)))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; keys
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x f") 'find-grep-dired)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x C-k") 'compile)

(server-start)
