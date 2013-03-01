(push (concat user-emacs-directory "bin")  exec-path)

(defun load-dir (dir)
  (mapc 'load (directory-files dir t "^[^#].*el$")))

(load-file "~/.emacs.d/elpa.el")
(add-to-list 'load-path (concat user-emacs-directory "vendor/"))

(load-file "~/.emacs.d/custom.el")

(load-dir (concat user-emacs-directory "custom/"))
