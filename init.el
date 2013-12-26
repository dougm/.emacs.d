(push (concat user-emacs-directory "bin")  exec-path)

(defun load-dir (dir)
  (mapc 'load (directory-files dir t "^[^#].*el$")))

(load-file "~/.emacs.d/el-get.el")

(load-file "~/.emacs.d/custom.el")

(load-dir (concat user-emacs-directory "custom/"))
