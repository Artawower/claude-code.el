;;; claude-code.el --- Claude Code Emacs integration -*- lexical-binding: t; -*-

;; Author: Stephen Molitor <stevemolitor@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (transient "0.7.5") (eat "0.9.2"))
;; Keywords: tools, ai
;; URL: https://github.com/stevemolitor/claude-code.el

;;; Commentary:

;; An Emacs interface to Claude Code.  This package provides convenient
;; ways to interact with Claude from within Emacs, including sending
;; commands, toggling the Claude window, and accessing slash commands.

;;; Code:

(require 'transient)
(require 'project)

;;;; Customization options
(defgroup claude-code nil
  "Claude AI interface for Emacs."
  :group 'tools)

(defface claude-code-repl-face
  nil
  "Face for Claude REPL."
  :group 'claude-code)

(defcustom claude-code-term-name "xterm-256color"
  "Terminal type to use for Claude REPL."
  :type 'string
  :group 'claude-code)

;;;###autoload (autoload 'claude-code-prefix-key "claude-code")
(defcustom claude-code-prefix-key "C-c c"
  "Prefix key for Claude commands."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-start-hook nil
  "Hook run after Claude is started."
  :type 'hook
  :group 'claude-code)

(defcustom claude-code-startup-delay 0.1
  "Delay in seconds after starting Claude before displaying buffer.

This helps fix terminal layout issues that can occur if the buffer
is displayed before Claude is fully initialized."
  :type 'number
  :group 'claude-code)

(defcustom claude-code-large-buffer-threshold 1000
  "Size threshold in characters above which buffers are considered \"large\".

When sending a buffer to Claude with `claude-code-send-region` and no
region is active, prompt for confirmation if buffer size exceeds this value."
  :type 'integer
  :group 'claude-code)

(defcustom claude-code-program "claude"
  "Program to run when starting Claude.
This is passed as the PROGRAM parameter to `eat-make`."
  :type 'string
  :group 'claude-code)

(defcustom claude-code-program-switches nil
  "List of command line switches to pass to the Claude program.
These are passed as SWITCHES parameters to `eat-make`."
  :type '(repeat string)
  :group 'claude-code)

;; Forward declare variables to avoid compilation warnings
(defvar eat-terminal)
(defvar eat-term-name)
(declare-function eat-term-send-string "eat")
(declare-function eat-kill-process "eat")
(declare-function eat-make "eat")

;;;; Key bindings
;;;###autoload (autoload 'claude-code-command-map "claude-code")
(defvar claude-code-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'claude-code)
    (define-key map "d" 'claude-code-current-directory)
    (define-key map "t" 'claude-code-toggle)
    (define-key map "b" 'claude-code-switch-to-buffer)
    (define-key map "k" 'claude-code-kill)
    (define-key map "s" 'claude-code-send-command)
    (define-key map "x" 'claude-code-send-command-with-context)
    (define-key map "/" 'claude-code-slash-commands)
    (define-key map "m" 'claude-code-transient)
    (define-key map "y" 'claude-code-send-return)
    (define-key map "n" 'claude-code-send-escape)
    (define-key map "r" 'claude-code-send-region)
    map)
  "Keymap for Claude commands.")

;;;; Transient Menus
;;;###autoload
(transient-define-prefix claude-code-transient ()
  "Claude command menu."
  ["Claude Commands"
   ["Manage Claude" ("c" "Start Claude" claude-code)
    ("d" "Start Claude in current directory" claude-code-current-directory)
    ("t" "Toggle claude window" claude-code-toggle)
    ("b" "Switch to Claude buffer" claude-code-switch-to-buffer)
    ("k" "Kill Claude" claude-code-kill)]
   ["Send Commands to Claude" ("s" "Send command" claude-code-send-command)
    ("x" "Send command with context" claude-code-send-command-with-context)
    ("r" "Send region or buffer" claude-code-send-region)
    ("y" "Send <return> to Claude (\"Yes\")" claude-code-send-return)
    ("n" "Send <escape> to Claude (\"No\")" claude-code-send-escape)
    ("/" "Slash Commands" claude-code-slash-commands)]])

;;;###autoload
(transient-define-prefix claude-code-slash-commands ()
  "Claude slash commands menu."
  ["Slash Commands"
   ["Basic Commands"
    ("c" "Clear" (lambda () (interactive) (claude-code--do-send-command "/clear")))
    ("o" "Compact" (lambda () (interactive) (claude-code--do-send-command "/compact")))
    ("f" "Config" (lambda () (interactive) (claude-code--do-send-command "/config")))
    ("t" "Cost" (lambda () (interactive) (claude-code--do-send-command "/cost")))
    ("d" "Doctor" (lambda () (interactive) (claude-code--do-send-command "/doctor")))
    ("x" "Exit" (lambda () (interactive) (claude-code--do-send-command "/exit")))
    ("h" "Help" (lambda () (interactive) (claude-code--do-send-command "/help")))]
   
   ["Special Commands"
    ("i" "Init" (lambda () (interactive) (claude-code--do-send-command "/init")))
    ("p" "PR" (lambda () (interactive) (claude-code--do-send-command "/pr")))
    ("r" "Release" (lambda () (interactive) (claude-code--do-send-command "/release")))
    ("b" "Bug" (lambda () (interactive) (claude-code--do-send-command "/bug")))
    ("v" "Review" (lambda () (interactive) (claude-code--do-send-command "/review")))]
   
   ["Additional Commands"
    ("e" "Terminal" (lambda () (interactive) (claude-code--do-send-command "/terminal")))
    ("m" "Theme" (lambda () (interactive) (claude-code--do-send-command "/theme")))
    ("v" "Vim" (lambda () (interactive) (claude-code--do-send-command "/vim")))
    ("a" "Approved" (lambda () (interactive) (claude-code--do-send-command "/approved")))
    ("l" "Logout" (lambda () (interactive) (claude-code--do-send-command "/logout")))
    ("g" "Login" (lambda () (interactive) (claude-code--do-send-command "/login")))]
   ])

;;;; Private util functions
(defun claude-code--do-send-command (cmd)
  "Send a command CMD to Claude if Claude buffer exists.

After sending the command, move point to the end of the buffer."
  (if-let ((claude-code-buffer (get-buffer "*claude*")))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal cmd)
        (eat-term-send-string eat-terminal (kbd "RET"))
        (display-buffer claude-code-buffer))
    (error "Claude is not running")))

(defun claude-code--setup-repl-faces ()
  "Setup faces for the Claude REPL buffer.

Applies the `claude-code-repl-face' to all terminal-related faces
for consistent appearance."
  (dolist (face '(eat-shell-prompt-annotation-running
                  eat-shell-prompt-annotation-success
                  eat-shell-prompt-annotation-failure
                  eat-term-bold
                  eat-term-faint
                  eat-term-italic
                  eat-term-slow-blink
                  eat-term-fast-blink))
    (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face))
  (dotimes (i 10)
    (let ((face (intern (format "eat-term-font-%d" i))))
      (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face)))
  (dotimes (i 10)
    (let ((face (intern (format "eat-term-font-%d" i))))
      (funcall 'face-remap-add-relative face :inherit 'claude-code-repl-face)))
  (buffer-face-set :inherit 'claude-code-repl-face)
  (face-remap-add-relative 'nobreak-space :underline nil)
  (face-remap-add-relative 'eat-term-faint :foreground "#999999" :weight 'light))

(defun claude-code--start (dir &optional arg)
  "Start Claude in directory DIR.

With non-nil ARG, switch to the Claude buffer after starting."
  ;; Forward declare variables to avoid compilation warnings
  (require 'eat)
  
  (let ((default-directory dir)
        (buffer (get-buffer-create "*claude*")))
    (with-current-buffer buffer
      (cd dir)
      (setq-local eat-term-name claude-code-term-name)
      (let ((process-adaptive-read-buffering nil))
        (apply #'eat-make "claude" claude-code-program nil claude-code-program-switches))
      (setq-local eat-term-name claude-code-term-name)
      (claude-code--setup-repl-faces)
      (run-hooks 'claude-code-start-hook)

      ;; fix wonky initial terminal layout that happens sometimes if we show the buffer before claude is ready
      (sleep-for claude-code-startup-delay)
      (display-buffer buffer))
    (when arg
      (switch-to-buffer buffer))))

;;;; Interactive Commands
;;;###autoload
(defun claude-code (&optional arg)
  "Start Claude in an eat terminal and enable `claude-code-mode'.

With prefix ARG, prompt for the project directory."
  (interactive "P")
  (let* ((dir (if arg
                  (read-directory-name "Project directory: ")
                (funcall #'project-root (project-current t)))))
    (claude-code--start dir arg)))

;;;###autoload
(defun claude-code-send-region (&optional arg)
  "Send the current region to Claude.

If no region is active, send the entire buffer if it's not too large.
For large buffers, ask for confirmation first.

With prefix ARG, switch to the Claude buffer after sending the text."
  (interactive "P")
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (if (> (buffer-size) claude-code-large-buffer-threshold)
                    (when (yes-or-no-p "Buffer is large.  Send anyway? ")
                      (buffer-substring-no-properties (point-min) (point-max)))
                  (buffer-substring-no-properties (point-min) (point-max))))))
    (when text
      (claude-code--do-send-command text)
      (when arg
        (switch-to-buffer "*claude*")))))

;;;###autoload
(defun claude-code-current-directory (&optional arg)
  "Start Claude in the current directory.

With non-nil ARG, switch to the Claude buffer after starting."
  (interactive "P")
  (claude-code--start default-directory arg))

;;;###autoload
(defun claude-code-toggle ()
  "Show or hide the Claude window.

If the Claude buffer doesn't exist, create it."
  (interactive)
  (let ((claude-code-buffer (get-buffer "*claude*")))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (display-buffer claude-code-buffer))
      (error "Claude is not running"))))

;;;###autoload
(defun claude-code-switch-to-buffer ()
  "Switch to the Claude buffer if it exists."
  (interactive)
  (if-let ((claude-code-buffer (get-buffer "*claude*")))
      (switch-to-buffer claude-code-buffer)
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-kill ()
  "Kill Claude process and close its window."
  (interactive)
  (if-let ((claude-code-buffer (get-buffer "*claude*")))
      (progn (with-current-buffer claude-code-buffer
               (eat-kill-process)
               (kill-buffer claude-code-buffer))
             (message "Claude killed"))
    (error "Claude is not running")))

;;;###autoload
(defun claude-code-send-command (cmd &optional arg)
  "Read a Claude command from the minibuffer and send it.

With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (claude-code--do-send-command cmd)
  (when arg
    (switch-to-buffer "*claude*")))

;;;###autoload
(defun claude-code-send-command-with-context (cmd &optional arg)
  "Read a Claude command and send it with current file and line context.

If region is active, include region line numbers.
With prefix ARG, switch to the Claude buffer after sending CMD."
  (interactive "sClaude command: \nP")
  (let* ((file-name (when (buffer-file-name)
                        (file-relative-name (buffer-file-name) (project-root (project-current t)))))
         (line-info (if (use-region-p)
                        (format "Lines: %d-%d"
                                (line-number-at-pos (region-beginning))
                                (line-number-at-pos (region-end)))
                      (format "Line: %d" (line-number-at-pos))))
         (cmd-with-context (if file-name
                               (format "%s\nContext: File: %s, %s"
                                       cmd
                                       file-name
                                       line-info)
                             cmd)))
    (claude-code--do-send-command cmd-with-context)
    (when arg
      (switch-to-buffer "*claude*"))))

;;;###autoload
(defun claude-code-send-return ()
  "Send <return> to the Claude Code REPL.

This is useful for saying Yes when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (claude-code--do-send-command ""))

;;;###autoload
(defun claude-code-send-escape ()
  "Send <escape> to the Claude Code REPL.

This is useful for saying \"No\" when Claude asks for confirmation without
having to switch to the REPL buffer."
  (interactive)
  (if-let ((claude-code-buffer (get-buffer "*claude*")))
      (with-current-buffer claude-code-buffer
        (eat-term-send-string eat-terminal (kbd "ESC"))
        (display-buffer claude-code-buffer))
    (error "Claude is not running")))

;;;; Mode definition
;;;###autoload
(define-minor-mode claude-code-mode
  "Minor mode for interacting with Claude AI CLI.

When enabled, provides keybindings for starting, sending commands to,
and managing Claude sessions."
  :init-value nil
  :lighter " Claude"
  :global t
  :group 'claude-code
  (if claude-code-mode
      (global-set-key (kbd claude-code-prefix-key) claude-code-command-map)
    (global-unset-key (kbd claude-code-prefix-key))))

;;;; Provide the feature
(provide 'claude-code)

;;; claude-code.el ends here
