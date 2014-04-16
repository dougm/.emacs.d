;; git / project stuff
(el-get 'sync
        '(ag
          magit
          projectile))

(autoload 'vc-git-root "vc-git")

(setq ag-highlight-search t
      projectile-use-git-grep t)

;; e.g. ignore .git/COMMIT_EDITMSG opened via magit
(eval-after-load "recentf"
  '(progn
     (add-to-list 'recentf-exclude "/\\.git/")))

;; enable projectile minor mode
(add-hook 'prog-mode-hook 'projectile-on)

(defun git-grep-extension ()
  "git grep files in project with the same extension as current buffer.
If current buffer has not extension, basename of the file is used."
  (interactive)
  (let ((regexp (grep-read-regexp))
        (ext (file-name-extension buffer-file-name))
        (dir (vc-git-root buffer-file-name)))
    (vc-git-grep regexp
                 (if ext (format "'*.%s'" ext)
                   (let ((name (file-name-base)))
                     (format "'%s' '*/%s'" name name)))
                   dir)))

(defun github-update ()
  "Update remotes from github, rebasing current and local master branch.
Assumes a fork, adding remote for $USER if needed.
For el-get packages, reload after update."
  (interactive)
  (let ((project (projectile-project-name))
        (branch (magit-get-current-branch))
        (user (or (getenv "GITHUB_USER")
                  (getenv "USER"))))
    (unless (magit-get "remote" user "url")
      (let ((url (format "git@github.com:%s/%s.git" user project)))
        (message "git remote add %s %s" user url)
        (magit-add-remote user url)))
    (magit-remote-update)
    (message "git rebase origin/master %s" branch)
    (magit-run-git "rebase" "origin/master")
    (unless (string= branch "master")
      (message "git rebase origin/master master")
      (magit-run-git "rebase" "origin/master" "master"))
    (when (el-get-read-package-status project)
      (el-get-byte-compile project)
      (el-get-reload project))
    (unless (string= branch "master")
      (message "git checkout %s" branch)
      (magit-checkout branch))))

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
