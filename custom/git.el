;; git / project stuff
(el-get 'sync
        '(ag
          magit
          magit-gerrit
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

(defun git-run (&rest args)
  "magit-run-git with logging."
  (message "git %s" (mapconcat 'identity args " "))
  (apply 'magit-run-git args))

(defun git-hup ()
  "Update remotes from github, rebasing current and local master branch.
Assumes a fork, adding a remote for $USER and configuring the current branch
remote to $USER if not already set.  With this config, magit will push to
your fork by default, rather than origin.
If current branch has been merged with upstream master, delete it and stay
on the master branch - assumes PR was merged upstream.  Otherwise, stay
checked out on the current topic branch.
For el-get packages, reload after update."
  (interactive)
  (let ((project (projectile-project-name))
        (branch (magit-get-current-branch))
        (user (or (getenv "GITHUB_USER")
                  (getenv "USER"))))
    (magit-save-some-buffers)
    (unless (magit-get "remote" user "url")
      (let* ((upstream (url-generic-parse-url (magit-get "remote" "origin" "url")))
             (repo (file-name-nondirectory (url-filename upstream)))
             (url (format "git@github.com:%s/%s" user repo)))
        (git-run "remote" "add" user url)))
    (git-run "remote" "update")
    (git-run "rebase" "origin/master" branch)
    (unless (string= branch "master")
      (unless (magit-get "branch" branch "remote")
        (message "git config branch.%s.remote %s" branch user)
        (magit-set user "branch" branch "remote"))
      (git-run "rebase" "origin/master" "master")
      (if (string= (magit-rev-parse "master") (magit-rev-parse branch))
          (git-run "branch" "-d" branch)
        (git-run "checkout" branch)))
    (when (el-get-read-package-status project)
      (el-get-byte-compile project)
      (el-get-reload project))))

(defun git-check ()
  "Check for any unpushed branches, unstaged commits, etc."
  (-any? (lambda (cmd)
           (not (= (length (shell-command-to-string cmd)) 0)))
         '("git log --branches --not --remotes"
           "git status -s"
           "git stash list")))

(defun git-status-all ()
  "Magit status for all projects."
  (interactive)
  (let (project)
    (with-current-buffer (get-buffer-create "*git-status-all*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (project (projectile-relevant-known-projects))
        (let ((default-directory (expand-file-name project)))
          (when (and (file-exists-p default-directory)
                     (git-check))
            (lexical-let ((dir default-directory))
             (insert-text-button (file-name-nondirectory (directory-file-name project))
                                 'action (lambda (button)
                                           (magit-status dir))
                                 'follow-link t
                                 'mouse-face magit-item-highlight-face))
            (insert "\n\n"))))
      (let ((w (display-buffer (current-buffer))))
        (balance-windows)
        (shrink-window-if-larger-than-buffer w)
        (set-window-point w (point-min)))
      (setq buffer-read-only t))))

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
