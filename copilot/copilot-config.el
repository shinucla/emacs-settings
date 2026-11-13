;;; copilot-config.el --- GitHub Copilot configuration  -*- lexical-binding: nil; -*-

;; Description: inline code completions from GitHub Copilot in
;; `prog-mode' and `text-mode'; TAB accepts a suggestion.
;;
;; Instructions:
;;   M-x copilot-install-server  install or update the local server
;;   M-x copilot-login           authenticate with GitHub (once)
;;   M-x copilot-diagnose        check the server and connection
;;   M-x copilot-mode            toggle Copilot in the current buffer
;;
;; Required packages (MELPA): jsonrpc, f, s, dash, editorconfig.
;; The Copilot implementation stays in ~/emacs-settings/copilot_el/.
;; This file holds all Copilot settings and is loaded from ~/.emacs.

;;; Code:

(load-file "~/emacs-settings/copilot_el/copilot-balancer.el")
(load-file "~/emacs-settings/copilot_el/copilot.el")
(add-to-list 'load-path "~/emacs-settings/copilot_el/copilot.el")
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'text-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; Warning suppression, moved here from `custom-set-variables' in ~/.emacs.
;; The duplicated `copilot-no-mode-indent' entry is kept as it was.
(setq warning-suppress-log-types
      '(((copilot copilot-no-mode-indent))
        ((copilot copilot-no-mode-indent))))
(setq warning-suppress-types '(((copilot copilot-no-mode-indent))))
(add-to-list 'warning-suppress-log-types '(copilot))

(provide 'copilot-config)
;;; copilot-config.el ends here
