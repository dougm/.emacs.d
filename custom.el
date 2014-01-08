;; colors
(setq solarized-broken-srgb nil)
(load-theme 'solarized-dark t)
(setq ag-highlight-search t)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(eval-after-load "eldoc"
  '(progn
     (set-face-background 'eldoc-highlight-function-argument (face-background 'default))
     (set-face-foreground 'eldoc-highlight-function-argument "#def")
     (set-face-attribute 'eldoc-highlight-function-argument nil :weight 'extra-bold)))
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(XXX\\|TODO\\|FIXME\\)" 1 font-lock-warning-face t)))))

;; lines
(unless window-system (setq linum-format " %d "))
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
(setq flymake-gui-warnings-enabled nil)

;; whitespace
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face trailing lines-tail))
(setq-default whitespace-line-column 120)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
          '(lambda ()
             (setq show-trailing-whitespace t)
             (set-fill-column 80)))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; keys
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-x f") 'ag-project)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x o") 'ido-select-window)
(global-set-key (kbd "C-x r") 'ag-regexp-project-at-point)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'compile)
(global-set-key (kbd "M-=") 'align-regexp)

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
(windmove-default-keybindings 'meta)
(set-frame-parameter nil 'fullscreen 'fullboth)
(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
(require 'uniquify)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t
      windmove-wrap-around t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      uniquify-buffer-name-style 'forward
      save-place-file "~/.emacs.d/places"
      backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      diff-switches "-u")

(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
