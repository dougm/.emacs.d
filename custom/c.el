(el-get 'sync
        '(cmake-mode
          c-eldoc
          xcscope))

;; irony-mode depends on llvm and clang
;; brew install llvm --with-clang --disable-shared --without-python
(let ((llvm-bin "/usr/local/opt/llvm/bin"))
  (when (file-directory-p llvm-bin)
    (el-get-envpath-prepend "PATH" llvm-bin)
    (add-to-list 'exec-path llvm-bin)))

(when (executable-find "llvm-config")
  (el-get 'sync '(irony-mode))

  (defun enable-clang-ac ()
    (require 'irony)
    (irony-enable 'ac)
    (irony-mode 1))

  (add-hook 'c++-mode-hook 'enable-clang-ac)
  (add-hook 'c-mode-hook 'enable-clang-ac))

(c-add-style "default"
             '("bsd"
               (c-basic-offset . 4)
               (fill-column . 80)
               (indent-tabs-mode . nil)))

(setq c-default-style "default")

(c-add-style "vmware"
             '("bsd"
               (c-basic-offset . 3)
               (fill-column . 80)
               (indent-tabs-mode . nil)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((topmost-intro        . 0)
                                   (topmost-intro-cont   . 0)
                                   (substatement         . +)
                                   (substatement-open    . 0)
                                   (statement-case-open  . +)
                                   (statement-cont       . +)
                                   (access-label         . -)
                                   (inclass              . +)
                                   (inline-open          . 0)
                                   (innamespace          . 0)))))

(defun maybe-vmware-style ()
  (when (and buffer-file-name
             (string-match "bora" buffer-file-name))
    (c-set-style "vmware")
    (set (make-local-variable 'compile-command)
         (concat "iscons " buffer-file-name))))

(add-hook 'c-mode-common-hook 'maybe-vmware-style)

(setq-default cscope-do-not-update-database t)

(defun c-find-tag ()
  "Generate TAGS if needed prior to calling find-tag"
  (interactive)
  (let ((root (projectile-project-root)))
    (unless (file-exists-p (concat root "TAGS"))
      (projectile-regenerate-tags))
    (visit-tags-table root t)
    (call-interactively 'find-tag)))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((root (projectile-project-root)))
              (setq c-eldoc-includes (concat "-I" root "include")))
            (local-set-key (kbd "C-c o") 'c-find-tag)
            (local-set-key (kbd "C-c -") 'pop-tag-mark)
            ))
