;; colors

(require 'solarized)

(deftheme solarized-dark "The dark variant of the Solarized colour theme")

(defun cutomize-solarized ()
  "customized solarized theme."

  (custom-theme-set-faces
   theme-name

   `(link ((,class (:foreground ,blue :weight normal :underline t))))

   `(button ((,class (:inherit link))))

   `(match ((,class (:foreground ,orange :weight bold))))

   `(comint-highlight-prompt ((,class (:foreground ,blue))))

   ;; eldoc
   `(eldoc-highlight-function-argument
     ((,class (:foreground ,magenta :background unspecified
                           :weight bold))))

   ;; irony
   `(ac-irony-candidate-face ((,class (:inherit ac-candidate-face))))
   `(ac-irony-selection-face ((,class (:inherit ac-selection-face))))
   ))
(create-solarized-theme 'dark 'solarized-dark 'cutomize-solarized)

(setq-default grep-highlight-matches t)

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
(global-auto-revert-mode)
(global-flycheck-mode)
;; don't enable flycheck unless we modify the buffer
(delq 'mode-enabled flycheck-check-syntax-automatically)

(require 'auto-complete-config)
(ac-config-default)

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
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c C-y") 'yas-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'compile)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "M-=") 'align-regexp)
(global-set-key (kbd "M-+") 'text-scale-increase)
(global-set-key (kbd "M-_") 'text-scale-decrease)
(global-set-key (kbd "M-*") (lambda ()
                              (interactive)
                              (save-some-buffers t)))

(when window-system
  (when (memq window-system '(mac ns))
    (set-face-font 'default "Monaco-13")
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (set-frame-parameter nil 'fullscreen 'fullboth))

  (windmove-default-keybindings 'meta)
  (global-unset-key (kbd "C-j"))
  (global-set-key (kbd "C-j l") 'windmove-left)
  (global-set-key (kbd "C-j r") 'windmove-right)
  (global-set-key (kbd "C-j u") 'windmove-up)
  (global-set-key (kbd "C-j d") 'windmove-down)
  (global-set-key (kbd "C-j C-j") 'switch-to-previous-buffer))

;; misc
(display-time-mode 1)
(load "server")
(unless (server-running-p) (server-start))
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
      inhibit-startup-message t
      save-place-file "~/.emacs.d/places"
      sentence-end-double-space nil
      windmove-wrap-around t
      x-select-enable-clipboard t
      visible-bell t)

(add-to-list 'auto-mode-alist '("\\.vmx$" . conf-mode))
