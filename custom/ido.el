;; ido and friends that find things faster (ag, projectile, smex, etc)
(el-get 'sync
        '(ag
          flx
          ido-select-window
          ido-ubiquitous
          ido-vertical-mode
          projectile
          smex))

(require 'uniquify)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(ido-mode t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(ido-vertical-mode t)

(setq ag-highlight-search t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      uniquify-buffer-name-style 'forward)

;; enable projectile minor mode
(add-hook 'prog-mode-hook 'projectile-on)

;; key bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x f") 'ag-project)
(global-set-key (kbd "C-x o") 'ido-select-window)
(global-set-key (kbd "C-x p") 'projectile-find-file)
(global-set-key (kbd "C-x r") 'ag-regexp-project-at-point)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)))
