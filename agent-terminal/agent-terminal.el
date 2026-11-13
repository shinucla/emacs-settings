;;; agent-terminal.el --- Run agentic CLIs in interactive Emacs buffers -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Full terminal interactivity (PTY + VT emulation) for agentic coding
;; CLIs such as claude, codex, gemini, aider, opencode, ...
;;
;; Why not `shell'/`comint'?  They run a pipe, not a PTY, and do not
;; interpret ANSI screen control, so full-screen TUIs degrade to noise.
;;
;; Launchers:
;;
;;   C-c a   `my/agent'        Eat terminal (pure Elisp, no build step)
;;   C-c A   `my/agent-vterm'  vterm (native libvterm; best fidelity)
;;
;; The same commands run from M-x as `agent-terminal' and
;; `agent-terminal-vterm'.
;;
;; Both run the CLI in the current project root, in a buffer named
;; "*<program>:<project>*".  The buffer and its process are reused while
;; they are alive; a prefix argument (C-u C-c a) forces a fresh session.
;; The prompt offers your shell (zsh, bash, ...) by default.  Set
;; `my/agent-default-program' to offer another program, such as "claude".
;;
;; Keys in an Eat buffer:
;;
;;   C-c C-c  toggle between terminal input and Emacs (scroll/copy) mode
;;   C-c C-i  send C-c (interrupt the agent)
;;   ESC      send ESC at once (cancel in claude); one press is enough
;;   C-y      paste (bracketed paste, so multi-line prompts are safe)
;;   C-<up> / C-<down>      scroll one page; cursor moves with the text
;;   C-c <up> / C-c <down>  same thing if your terminal eats C-<arrows>
;;   S-<up> / S-<down>      go to the CLI (Eat keeps them by default)
;;   wheel                  scroll one line; cursor moves with the text
;;   C-c M-d  char mode: every key goes to the CLI
;;   C-c C-l  line mode
;;
;; In Emacs mode all normal Emacs keys work: C-v/M-v, C-s, M-<, M-w, ...
;; The buffer is read-only there, so copy with M-w (C-w would signal).
;; C-c C-e (enter) and C-c C-j (leave) remain bound by Eat as alternatives.
;;
;; Keys in a vterm buffer:
;;
;;   C-c C-c  send C-c (interrupt the agent)
;;   C-c C-t  copy mode: scroll/select with Emacs keys, RET or C-c C-t
;;            to return to the terminal
;;   C-y      paste (bracketed paste)
;;
;; Optional: notification when the CLI exits.  Uncomment in your init:
;;
;;   (add-hook 'eat-exit-hook
;;             (lambda (_proc) (notifications-notify :title "Agent finished")))
;;
;;; Code:

(require 'project)
(require 'subr-x)

(defgroup my/agent nil
  "Interactive terminals for agentic coding CLIs."
  :group 'tools)

(defcustom my/agent-default-program nil
  "Program offered by default by `my/agent' and `my/agent-vterm'.
When nil, offer the user's shell from `shell-file-name'."
  :type '(choice (const :tag "User's shell" nil) string)
  :group 'my/agent)

(defun my/agent--default-program ()
  (or my/agent-default-program
      (file-name-nondirectory shell-file-name)))

(defcustom my/agent-esc-delay 0.02
  "Seconds to wait after ESC before it counts as a lone ESC key.
A text terminal sends Meta keys and arrow keys as ESC plus more
bytes, all at once.  A lone ESC has no bytes after it."
  :type 'number
  :group 'my/agent)

(defun my/agent--root ()
  "Return the current project root, or `default-directory'."
  (let ((project (project-current)))
    (if project (project-root project) default-directory)))

(defun my/agent--name (program)
  "Return the buffer name for PROGRAM in the current project."
  (format "*%s:%s*" (car (split-string program))
          (file-name-nondirectory
           (directory-file-name (my/agent--root)))))

;;;###autoload
(defun my/agent (program &optional fresh)
  "Run agentic CLI PROGRAM in an Eat terminal for the current project.
With prefix argument FRESH, always start a new session instead of
reusing the live buffer."
  (interactive
   (list (read-shell-command "Run in terminal: "
                             (concat (my/agent--default-program) " "))
         current-prefix-arg))
  (setq program (string-trim program))
  (when (string-empty-p program)
    (user-error "No program given"))
  (unless (require 'eat nil t)
    (user-error "Eat is not installed; run M-x package-install RET eat"))
  (let* ((name (my/agent--name program))
         (buf (if fresh (generate-new-buffer name) (get-buffer-create name)))
         (root (my/agent--root)))
    (with-current-buffer buf
      (setq default-directory root)
      (unless (derived-mode-p 'eat-mode)
        (eat-mode))
      (unless (and eat-terminal
                   (eat-term-parameter eat-terminal 'eat--process))
        (eat-exec buf (buffer-name) "/usr/bin/env" nil
                  (list "sh" "-c" program))))
    (pop-to-buffer buf)))

;;;###autoload
(defun my/agent-vterm (program)
  "Run agentic CLI PROGRAM in a vterm buffer for the current project."
  (interactive
   (list (read-shell-command "Run in vterm: "
                             (concat (my/agent--default-program) " "))))
  (setq program (string-trim program))
  (when (string-empty-p program)
    (user-error "No program given"))
  (unless (require 'vterm nil t)
    (user-error "vterm is not installed; see M-x vterm-module-compile"))
  (let* ((name (my/agent--name program))
         (buf (get-buffer name))
         (root (my/agent--root)))
    (if (and buf (get-buffer-process buf))
        (pop-to-buffer buf)
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (let ((default-directory root)
            (vterm-shell program))
        (pop-to-buffer (vterm name))))))

;;; Scrolling and input mode

(defvar-local my/agent--scrolled nil
  "Non-nil while the window shows scrollback rather than live output.")

(defvar-local my/agent--follow-start nil
  "Window start before the user first scrolled back.
Scrolling forward to this position resumes following the output.")

(defun my/agent--scroll (direction lines &optional event)
  "Scroll the window in DIRECTION by LINES, or `page' for a screenful.
DIRECTION is `backward' (show earlier output) or `forward' (show
later output).  EVENT, when it is a mouse event, names the window
to scroll.

Point keeps its text position and moves on screen with the text;
it is only nudged when it would otherwise leave the window.  While
the user is reading scrollback, `my/agent--scrolled' keeps Eat
from recentering the window when new output arrives."
  (let ((window (if (and event (mouse-event-p event))
                    (posn-window (event-start event))
                  (selected-window))))
    (when (window-live-p window)
      (with-selected-window window
        (let ((n (if (eq lines 'page)
                     (max 1 (- (window-body-height)
                               next-screen-context-lines))
                   lines))
              (before (window-start)))
          (condition-case nil
              (if (eq direction 'backward)
                  (scroll-down n)
                (scroll-up n))
            (error nil))
          (if (eq direction 'backward)
              (progn
                (unless my/agent--scrolled
                  (setq my/agent--follow-start before))
                (setq my/agent--scrolled t))
            (when (and my/agent--scrolled
                       my/agent--follow-start
                       (>= (window-start) my/agent--follow-start))
              (setq my/agent--scrolled nil
                    my/agent--follow-start nil))))))))

(defun my/agent-scroll-backward (&optional event)
  "Scroll the window one page to show earlier terminal output."
  (interactive (list last-command-event))
  (my/agent--scroll 'backward 'page event))

(defun my/agent-scroll-forward (&optional event)
  "Scroll the window one page to show later terminal output."
  (interactive (list last-command-event))
  (my/agent--scroll 'forward 'page event))

(defun my/agent-wheel-backward (&optional event)
  "Scroll the window one line to show earlier terminal output."
  (interactive (list last-command-event))
  (my/agent--scroll 'backward 1 event))

(defun my/agent-wheel-forward (&optional event)
  "Scroll the window one line to show later terminal output."
  (interactive (list last-command-event))
  (my/agent--scroll 'forward 1 event))

(defun my/agent-toggle-input-mode ()
  "Toggle between sending keys to the CLI and Emacs scroll/copy mode."
  (interactive)
  (if (bound-and-true-p eat--semi-char-mode)
      (eat-emacs-mode)
    (setq my/agent--scrolled nil
          my/agent--follow-start nil)
    (eat-semi-char-mode)))

(defun my/agent-send-escape ()
  "Send ESC to the CLI process."
  (interactive)
  (eat-self-input 1 ?\e))

(defun my/agent--esc-filter (map)
  "Return [escape] for a lone ESC in an Eat input buffer, else MAP."
  (if (and (equal (this-single-command-keys) [?\e])
           (derived-mode-p 'eat-mode)
           (or (bound-and-true-p eat--semi-char-mode)
               (bound-and-true-p eat--char-mode))
           (sit-for my/agent-esc-delay))
      [escape]
    map))

(defun my/agent--install-esc-filter (&optional frame)
  "Make a lone ESC an `escape' key on the text terminal of FRAME."
  (let* ((frame (or frame (selected-frame)))
         (terminal (frame-terminal frame)))
    (when (and (eq (terminal-live-p terminal) t)
               (not (terminal-parameter terminal 'my/agent-esc-map)))
      (with-selected-frame frame
        (let ((esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter terminal 'my/agent-esc-map
                                  (or esc-map 'none))
          (define-key input-decode-map [?\e]
                      `(menu-item "" ,esc-map
                                  :filter my/agent--esc-filter)))))))

(defun my/agent-interrupt ()
  "Send C-c to the CLI process."
  (interactive)
  (unless (and (boundp 'eat-terminal) eat-terminal)
    (user-error "No terminal process in this buffer"))
  (eat-self-input 1 ?\C-c))

(defun my/agent--show-nobreak-space-as-space ()
  (setq-local nobreak-char-display nil))

(defun my/agent--cli-hides-cursor-p (window)
  (with-current-buffer (window-buffer window)
    (and (derived-mode-p 'eat-mode)
         (null cursor-type)
         (or (bound-and-true-p eat--semi-char-mode)
             (bound-and-true-p eat--char-mode)))))

(defun my/agent--sync-tty-cursor (&rest _)
  (dolist (frame (frame-list))
    (unless (display-graphic-p frame)
      (dolist (window (window-list frame 'no-minibuf))
        (internal-show-cursor
         window (not (my/agent--cli-hides-cursor-p window)))))))

(defun my/agent--use-system-terminfo ()
  "Point Eat at a terminfo copy that the system ncurses can read."
  (let* ((source (expand-file-name
                  "eat.ti" (file-name-directory (locate-library "eat"))))
         (target (expand-file-name (locate-user-emacs-file "eat-terminfo")))
         (entry (expand-file-name "65/eat-truecolor" target)))
    (when (and (file-exists-p source)
               (file-newer-than-file-p source entry))
      (call-process "tic" nil nil nil "-x" "-o" target source))
    (when (file-exists-p entry)
      (setq eat-term-terminfo-directory target))))

(defvar-local my/agent--focused nil)

(defun my/agent--sync-focus ()
  (let ((active (window-buffer (or (minibuffer-selected-window)
                                   (selected-window)))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'eat-mode) eat-terminal)
          (let ((focused (eq buffer active)))
            (unless (eq focused my/agent--focused)
              (setq my/agent--focused focused)
              (eat-term-input-event
               eat-terminal 1
               (if focused '(eat-focus-in) '(eat-focus-out))))))))))

;; Terminal Emacs only receives wheel events when mouse reporting is on;
;; otherwise the terminal turns the wheel into arrow keys, which the CLI
;; reads as cursor movement.  Graphical frames do not need this.
(when (and (fboundp 'xterm-mouse-mode)
           (not noninteractive)
           (null (display-graphic-p)))
  (xterm-mouse-mode 1))

;; vterm sends most keys straight to the process, but keeps C-c as an
;; Emacs prefix.  Forward it so that C-c interrupts the agent, as it
;; would in a normal terminal.
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-c C-c")
              (lambda () (interactive) (vterm-send-key "c" nil nil t)))
  (define-key vterm-mode-map (kbd "C-c C-t") #'vterm-copy-mode))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'my/agent--show-nobreak-space-as-space)

  ;; Eat ships terminfo in the ncurses 6 format.  macOS has ncurses 5.7,
  ;; which cannot read it, so `clear' and `tput' fail in an Eat buffer.
  (when (eq system-type 'darwin)
    (my/agent--use-system-terminfo))

  ;; Remote hosts have no terminfo for Eat's own TERM names.
  (setq eat-term-name "xterm-256color")

  (advice-add 'eat--set-cursor :after #'my/agent--sync-tty-cursor)
  (dolist (mode '(eat-emacs-mode eat-semi-char-mode eat-char-mode
                  eat-line-mode))
    (advice-add mode :after #'my/agent--sync-tty-cursor))
  (add-hook 'post-command-hook #'my/agent--sync-tty-cursor)
  (add-hook 'post-command-hook #'my/agent--sync-focus)
  (set-face-attribute 'eat-term-faint nil
                      :weight 'unspecified
                      :foreground "gray50")

  ;; C-c C-c toggles.  While the terminal is in semi-char mode its
  ;; buffer-local minor mode map wins; in Emacs mode that map is
  ;; inactive and `eat-mode-map' is used, so one chord goes both ways.
  (define-key eat-semi-char-mode-map (kbd "C-c C-c")
              #'my/agent-toggle-input-mode)
  (define-key eat-mode-map (kbd "C-c C-c")
              #'my/agent-toggle-input-mode)

  ;; C-c C-c is the toggle now, so interrupt needs its own chord.
  (define-key eat-semi-char-mode-map (kbd "C-c C-i")
              #'my/agent-interrupt)

  ;; Page scrolling while keys still go to the CLI.  The cursor keeps its
  ;; position in the text and scrolls with it; it is only nudged when it
  ;; would leave the window.  Plain arrows stay with the CLI.
  (define-key eat-semi-char-mode-map (kbd "C-<up>")
              #'my/agent-scroll-backward)
  (define-key eat-semi-char-mode-map (kbd "C-<down>")
              #'my/agent-scroll-forward)
  (define-key eat-semi-char-mode-map (kbd "C-c <up>")
              #'my/agent-scroll-backward)
  (define-key eat-semi-char-mode-map (kbd "C-c <down>")
              #'my/agent-scroll-forward)

  ;; Eat leaves Shift+arrows to Emacs by default, so Emacs drops the
  ;; Shift and the CLI gets a plain arrow.  Send them to the CLI as is.
  (define-key eat-semi-char-mode-map [S-up] #'eat-self-input)
  (define-key eat-semi-char-mode-map [S-down] #'eat-self-input)

  ;; Eat binds ESC as the Meta prefix, so a lone ESC waits for a second
  ;; key.  The ESC filter turns a lone ESC into `escape', sent at once.
  (define-key eat-semi-char-mode-map [escape] #'my/agent-send-escape)
  (define-key eat-char-mode-map [escape] #'my/agent-send-escape)
  (add-hook 'tty-setup-hook #'my/agent--install-esc-filter)
  (when after-init-time
    (my/agent--install-esc-filter))

  (define-key eat-semi-char-mode-map (kbd "C-x C-x")
              (lambda () (interactive) (eat-input-char ?\C-x 2)))

  ;; Mouse wheel scrolls one line per notch, with the cursor following
  ;; the text, in both input and Emacs mode.  GUI frames use
  ;; wheel-up/down; terminal frames use mouse-4/5.  If the app grabs the
  ;; mouse, Eat lets the app handle it.
  (dolist (key '([wheel-up] [mouse-4]))
    (define-key eat-mode-map key #'my/agent-wheel-backward))
  (dolist (key '([wheel-down] [mouse-5]))
    (define-key eat-mode-map key #'my/agent-wheel-forward))

  ;; Eat recenters a window on its terminal cursor whenever output
  ;; arrives.  While the user is reading scrollback that would undo the
  ;; scroll, so leave the selected window alone until the user scrolls
  ;; back to the bottom or sends input.
  (advice-add 'eat--synchronize-scroll-windows :around
              (lambda (orig &optional force-selected)
                (let ((windows (funcall orig force-selected)))
                  (if (and my/agent--scrolled
                           (derived-mode-p 'eat-mode)
                           (not force-selected))
                      (delq (selected-window) windows)
                    windows))))
  (advice-add 'eat-self-input :before
              (lambda (&rest _)
                (setq my/agent--scrolled nil
                      my/agent--follow-start nil))))

(defalias 'agent-terminal #'my/agent)
(defalias 'agent-terminal-vterm #'my/agent-vterm)

(global-set-key (kbd "C-c a") #'my/agent)
(global-set-key (kbd "C-c A") #'my/agent-vterm)

(provide 'agent-terminal)
;;; agent-terminal.el ends here
