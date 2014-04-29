;; ido and friends
(el-get 'sync
        '(flx
          ido-ubiquitous
          ido-vertical-mode
          smex))

(require 'dired-x)
(require 'uniquify)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(ido-mode t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(ido-vertical-mode t)

(setq ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      uniquify-buffer-name-style 'forward)

;; key bindings
(global-set-key (kbd "M-x") 'smex)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
            (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)))
