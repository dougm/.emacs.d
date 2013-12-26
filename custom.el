;; colors
(load-theme 'solarized-dark t)
(setq ag-highlight-search t)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(XXX\\|TODO\\|FIXME\\)" 1 font-lock-warning-face t)))))

;; lines
(setq linum-format " %d ")
(add-hook 'prog-mode-hook 'linum-on)
(column-number-mode t)
(global-hl-line-mode t)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

;; auto
(show-paren-mode 1)
(autopair-global-mode)

(require 'auto-complete-config)
(setq ac-dictionary-files (list (concat user-emacs-directory ".dict")))
(ac-config-default)

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
(setq-default whitespace-line-column 120)
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
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x f") 'ag-project)
(global-set-key (kbd "C-x r") 'ag-regexp-project-at-point)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x C-k") 'compile)
(global-set-key (kbd "C-x C-g") 'magit-status)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ido-mode
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; misc
(server-start)
(menu-bar-mode -1)
(require 'uniquify)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
