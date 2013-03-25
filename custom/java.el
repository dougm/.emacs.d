(add-hook 'java-mode-hook 'esk-prog-mode-hook)
(add-hook 'java-mode-hook (lambda ()
            (setq whitespace-line-column 120)))
