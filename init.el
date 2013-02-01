(push (concat user-emacs-directory "bin")  exec-path)

(defun load-dir (dir)
  (add-to-list 'load-path dir)
  (mapc 'load (directory-files dir nil "^[^#].*el$")))

(load-file "~/.emacs.d/elpa.el")
(load-dir (concat user-emacs-directory "vendor/"))

(load-file "~/.emacs.d/custom.el")

(load-dir (concat user-emacs-directory "custom/"))
