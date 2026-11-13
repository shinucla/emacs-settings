;;------------------------------------------------------------------
;; Emacs configuration
;;
;; use "TERM=xterm-256color emacs -nw" in terminal to launch emacs
;; load .emacs without restart emacs:
;;     open .emacs in a buffer
;;     M-x eval-buffer
;;------------------------------------------------------------------

(load-file "~/emacs-settings/packages/packages.el")
(load-file "~/emacs-settings/theme/theme.el")
(load-file "~/emacs-settings/legacy-libs/legacy-libs.el")
(load-file "~/emacs-settings/modes/modes.el")
(load-file "~/emacs-settings/editing/editing.el")
(load-file "~/emacs-settings/code-generation/code-generation.el")
(load-file "~/emacs-settings/diff-mode/diff-appearance.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("118e5a60fbb4e13304bc967d61345a7fbdc956a5976e3e07b2f317b3dff55a46"
     "c756086eccd3d75751b80070eb071759281c34a3b392a9d5d5ca6cf4f5693edd"
     "9ff4eac2fa7eddcffd29fe90ccb68014f3bb1cb4e3e99b2d38d24362fe3646c1"
     "5131545d775f9489cf361f952a8ef31d0f0d4d6ecf9a27383de9df78bacb72e8"
     default))
 '(grep-command "grep --color=auto -nH -e ")
 '(package-selected-packages
   '(xclip vterm markdown-mode compat posframe eglot use-package dash s f json-rpc editorconfig csv-mode yaml-mode lua-mode)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/emacs-settings/git-graph/git-graph.el")

;;------------------------------------------------------------------
;; GitHub Copilot
;;
;; Description: inline code completions from GitHub Copilot in
;; `prog-mode' and `text-mode'; TAB accepts a suggestion.
;;
;; Instructions:
;;   M-x copilot-install-server  install or update the local server
;;   M-x copilot-login           authenticate with GitHub (once)
;;   M-x copilot-diagnose        check the server and connection
;;   M-x copilot-mode            toggle Copilot in the current buffer
;;
;; All settings live in ~/emacs-settings/copilot/copilot-config.el.
;;------------------------------------------------------------------
(load-file "~/emacs-settings/copilot/copilot-config.el")

;;------------------------------------------------------------------
;; Agentic CLI terminals (agent-terminal)
;;
;; Description:
;;   Enhanced terminal experience for Emacs, with support for multiple shells and AI agents.
;;
;; Install:
;;   Eat is required (already installed as eat-0.9.4):
;;     M-x package-install RET eat RET
;;   vterm is optional, only for C-c A:
;;     M-x package-install RET vterm RET
;;     M-x vterm-module-compile
;;   To offer claude in the prompt instead of your shell, set:
;;     (setq my/agent-default-program "claude")
;;   Then restart Emacs, or evaluate this file with M-x eval-buffer.
;;
;; Usage:
;;   C-c a        directly launch in the terminal: bash, zsh, claude, ..
;;   C-u C-c a    start a fresh session instead of reusing the live one
;;   M-x agent-terminal / M-x bash / M-x zsh 
;;
;;   Eat keys:
;;     C-c C-c     toggle terminal input / Emacs scroll-and-copy mode
;;     C-<up> / C-<down>, C-c <up> / C-c <down>   scroll one page
;;     wheel       scroll one line
;;------------------------------------------------------------------
(load-file "~/emacs-settings/agent-terminal/agent-terminal.el")
