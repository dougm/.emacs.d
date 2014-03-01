;; colors
(setq solarized-broken-srgb nil)
(load-theme 'solarized-dark t)
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
(yas-load-directory (concat user-emacs-directory "snippets"))

(require 'auto-complete-config)
(ac-config-default)

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
            (yas-minor-mode-on)
            (local-set-key (kbd "RET") 'newline-and-indent)))

;; keys
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c C-y") 'yas-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'compile)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "M-=") 'align-regexp)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-_") 'text-scale-decrease)

;; misc
(display-time-mode 1)
(load "server")
(unless (server-running-p) (server-start))
(windmove-default-keybindings 'meta)
(set-frame-parameter nil 'fullscreen 'fullboth)
(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups")))
      color-theme-is-global t
      diff-switches "-u"
      display-time-day-and-date t
      display-time-load-average nil
      enable-local-eval t
      enable-local-variables :all
      gc-cons-threshold 20000000
      global-auto-revert-mode t
      inhibit-startup-message t
      save-place-file "~/.emacs.d/places"
      sentence-end-double-space nil
      windmove-wrap-around t
      x-select-enable-clipboard t
      visible-bell t)

(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
