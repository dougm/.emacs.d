(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

(el-get 'sync
        '(el-get
          ag
          auto-complete
          autopair
          color-theme-solarized
          flymake-cursor
          flx
          idle-highlight-mode
          ido-select-window
          ido-ubiquitous
          ido-vertical-mode
          json-mode
          linum-off
          lua-mode
          magit
          markdown-mode
          projectile
          puppet-mode
          smex
          thrift-mode
          vagrant-tramp
          xcscope
          yaml-mode
          ))
