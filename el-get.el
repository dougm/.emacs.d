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
          go
          go-autocomplete
          goflymake
          thrift
          xcscope
          ))