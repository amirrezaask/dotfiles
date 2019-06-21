(setq elpy-rpc-python-command "python3")


(elpy-enable)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)  

(provide 'ext-python)
