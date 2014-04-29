(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path (concat user-emacs-directory "recipes"))

;; setenv PATH early, as other el-get recipes may append to PATH
(when (memq window-system '(mac ns))
  (el-get 'sync '(exec-path-from-shell))
  (exec-path-from-shell-initialize))

(el-get 'sync
        '(el-get
          auto-complete
          dockerfile-mode
          flycheck
          flycheck-cask
          flycheck-color-mode-line
          flycheck-pos-tip
          idle-highlight-mode
          json-mode
          linum-off
          lua-mode
          markdown-mode
          puppet-mode
          scala-mode2
          smartparens
          solarized-emacs
          thrift-mode
          vagrant
          vagrant-tramp
          yaml-mode
          yasnippet
          ))
