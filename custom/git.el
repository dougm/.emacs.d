;; git / project stuff
(el-get 'sync
        '(ag
          magit
          projectile))

(setq ag-highlight-search t
      projectile-use-git-grep t)

;; enable projectile minor mode
(add-hook 'prog-mode-hook 'projectile-on)

(defun git-grep-extension ()
  "git grep files in project with the same extension as current buffer"
  (interactive)
  (let ((regexp (grep-read-regexp))
        (ext (file-name-extension buffer-file-name))
        (dir (vc-git-root buffer-file-name)))
    (vc-git-grep regexp (format "'*.%s'" ext) dir)))

(defun toggle-magit-status ()
  "Brings up a magit-status buffer, filling the entire emacs frame"
  (interactive)
  (if (string= "magit-status-mode" (eval 'major-mode))
      (jump-to-register :pre-magit-status-window-configuration)
    (window-configuration-to-register :pre-magit-status-window-configuration)
    (call-interactively 'magit-status)
    (delete-other-windows)))

;; key bindings
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (let ((map git-rebase-mode-map))
              (define-key map (kbd "u") 'git-rebase-move-line-up)
              (define-key map (kbd "d") 'git-rebase-move-line-down)
              (define-key map (kbd "M-p") nil)
              (define-key map (kbd "M-n") nil)
              (define-key map (kbd "M-<up>") nil)
              (define-key map (kbd "M-<down>") nil))))
(global-set-key (kbd "C-x f") 'ag-project)
(global-set-key (kbd "C-x g") 'git-grep-extension)
(global-set-key (kbd "C-x G") 'projectile-grep)
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x M") 'toggle-magit-status)
(global-set-key (kbd "C-x p") 'projectile-find-file)
(global-set-key (kbd "C-x r") 'ag-regexp-project-at-point)
