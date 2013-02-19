(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar elpa-packages '(auto-complete
                        autopair
                        clojure-mode
                        color-theme-solarized
                        csharp-mode
                        erlang
                        flymake-cursor
                        flymake-ruby
                        gist
                        groovy-mode
                        haskell-mode
                        ido-ubiquitous
                        inf-ruby
                        json
                        linum-off
                        lua-mode
                        magit
                        markdown-mode
                        paredit
                        puppet-mode
                        python-mode
                        pymacs
                        scala-mode
                        smex
                        starter-kit
                        starter-kit-bindings
                        starter-kit-eshell
                        starter-kit-js
                        starter-kit-lisp
                        starter-kit-ruby
                        yaml-mode))

(dolist (p elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))
