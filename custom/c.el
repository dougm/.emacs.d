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
    (c-set-style "vmware")))

(add-hook 'c-mode-common-hook 'maybe-vmware-style)
(add-hook 'c-mode-common-hook 'esk-prog-mode-hook)

(require 'xcscope)
(setq-default cscope-do-not-update-database t)
