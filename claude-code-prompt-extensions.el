;;; claude-code-prompt-extensions.el --- Prompt buffer and file sending extensions for claude-code.el -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0") (project "0.10.0"))
;; Keywords: tools, ai, prompt, files
;; URL: https://github.com/your-username/claude-code-prompt-extensions

;;; Commentary:
;; This package extracts the prompt buffer functionality and file sending
;; features that were added to claude-code.el. It provides:
;; - Prompt buffer for composing Claude commands with file insertion
;; - Functions to send current file or selected files to Claude
;; - Integration with project.el for file completion

;;; Code:

(require 'claude-code)
(require 'project)

;;;; Variables
(defvar claude-code--prompt-buffer nil
  "Buffer for composing prompts before sending to Claude.")

;;;; Claude Prompt Mode
(define-derived-mode claude-code-prompt-mode text-mode "Claude-Prompt"
  "Major mode for composing Claude prompts.

This mode provides:
- File insertion with @ key
- Easy sending with C-c C-c
- Truncated lines for better display
- Syntax highlighting for file paths

\\{claude-code-prompt-mode-map}"
  :group 'claude-code-extensions
  
  ;; Enable truncate lines by default
  (setq truncate-lines t)
  
  ;; Fix window height to prevent zoom-mode interference
  (setq window-size-fixed 'height)
  
  ;; Set up key bindings
  (local-set-key (kbd "C-c C-c") 'claude-code-send-prompt)
  (local-set-key (kbd "C-c C-k") 'claude-code--close-prompt-buffer)
  (local-set-key (kbd "@") 'claude-code--handle-at-key)
  
  ;; Add some basic font-lock for file paths
  (font-lock-add-keywords nil
                          '(("@\\([^ \t\n]+\\)" 1 'font-lock-string-face)))
  
  ;; Set up the buffer
  (setq claude-code--prompt-buffer (current-buffer)))

;;;; Auto-show related windows
(defun claude-code-prompt-extensions--force-side-window-arrangement ()
  "Force Claude and prompt buffers to be in side windows properly."
  (let ((claude-buffers (claude-code--find-all-claude-buffers))
        (prompt-buffer (claude-code--get-prompt-buffer)))
    
    (when claude-buffers
      (let ((claude-buffer (car claude-buffers)))
        ;; Close any existing windows for Claude buffers
        (dolist (buffer claude-buffers)
          (dolist (window (get-buffer-window-list buffer))
            (when (window-live-p window)
              (delete-window window))))
        
        ;; Close any existing prompt windows
        (dolist (window (get-buffer-window-list prompt-buffer))
          (when (window-live-p window)
            (delete-window window)))
        
        ;; Force Claude to side window on right
        (display-buffer claude-buffer '((display-buffer-in-side-window)
                                       (side . right)
                                       (window-width . 90)))
        
        ;; Force prompt buffer to side window below Claude
        (display-buffer prompt-buffer '((display-buffer-in-side-window)
                                       (side . right)
                                       (slot . 1)
                                       (window-height . 0.15)))))))

(defun claude-code-prompt-extensions--auto-show-related-windows ()
  "Automatically show prompt buffer when Claude buffer is shown, and vice versa."
  (let ((claude-buffers (claude-code--find-all-claude-buffers))
        (prompt-buffers (cl-remove-if-not
                         (lambda (buf)
                           (eq (buffer-local-value 'major-mode buf) 'claude-code-prompt-mode))
                         (buffer-list))))
    
    ;; If Claude buffer is visible but no prompt buffer, show prompt
    (when (and (cl-some #'get-buffer-window claude-buffers)
               prompt-buffers
               (not (cl-some #'get-buffer-window prompt-buffers)))
      (let ((claude-buffer (cl-find-if #'get-buffer-window claude-buffers))
            (prompt-buffer (car prompt-buffers)))
        (when (and claude-buffer prompt-buffer)
          ;; Close any existing Claude windows and force to side window
          (dolist (buffer claude-buffers)
            (dolist (window (get-buffer-window-list buffer))
              (when (window-live-p window)
                (delete-window window))))
          ;; Force Claude buffer to side window on right
          (display-buffer claude-buffer '((display-buffer-in-side-window)
                                         (side . right)
                                         (window-width . 90)))
          ;; Force prompt buffer to side window below Claude
          (display-buffer prompt-buffer '((display-buffer-in-side-window)
                                          (side . right)
                                          (slot . 1)
                                          (window-height . 0.15))))))
    
    ;; If prompt buffer is visible but no Claude buffer, show Claude
    (when (and (cl-some #'get-buffer-window prompt-buffers)
               claude-buffers
               (not (cl-some #'get-buffer-window claude-buffers)))
      (let ((prompt-buffer (cl-find-if #'get-buffer-window prompt-buffers))
            (claude-buffer (car claude-buffers)))
        (when (and prompt-buffer claude-buffer)
          ;; Force Claude buffer to side window on right
          (display-buffer claude-buffer '((display-buffer-in-side-window)
                                          (side . right)
                                          (window-width . 90)))
          ;; Close existing prompt windows and force to side window below Claude
          (dolist (buffer prompt-buffers)
            (dolist (window (get-buffer-window-list buffer))
              (when (window-live-p window)
                (delete-window window))))
          (display-buffer prompt-buffer '((display-buffer-in-side-window)
                                         (side . right)
                                         (slot . 1)
                                         (window-height . 0.15))))))))

;;;; Window management hooks
(defun claude-code-prompt-extensions--close-related-windows (window)
  "Close related Claude windows when one is closed."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (cond
       ;; If closing a Claude prompt buffer, close Claude code windows
       ((eq (buffer-local-value 'major-mode buffer) 'claude-code-prompt-mode)
        (dolist (claude-buffer (claude-code--find-all-claude-buffers))
          (dolist (claude-window (get-buffer-window-list claude-buffer))
            (when (and (window-live-p claude-window)
                       (not (eq claude-window window)))
              (delete-window claude-window)))))
       ;; If closing a Claude code buffer, close prompt windows
       ((string-match-p "^\\*claude-code" (buffer-name buffer))
        (dolist (frame (frame-list))
          (dolist (win (window-list frame))
            (when (and (window-live-p win)
                       (not (eq win window))
                       (with-current-buffer (window-buffer win                         (eq major-mode 'claude-code-prompt-mode)))
                       (delete-window win))))))))))

(defvar claude-code-prompt-extensions--last-windows nil
  "Track windows in last configuration to detect deletions.")

(defun claude-code-prompt-extensions--check-window-deletions ()
  "Check for deleted windows and close related ones."
  (let* ((current-windows (mapcar (lambda (w) (cons w (window-buffer w))) (window-list)))
         (current-window-objects (mapcar #'car current-windows))
         (deleted-windows (cl-set-difference claude-code-prompt-extensions--last-windows 
                                             current-window-objects)))
    
    ;; Process deleted windows
    (dolist (deleted-window deleted-windows)
      (when (and deleted-window 
                 (bufferp (cdr (assoc deleted-window claude-code-prompt-extensions--last-windows))))
        (let ((deleted-buffer (cdr (assoc deleted-window claude-code-prompt-extensions--last-windows))))
          (cond
           ;; If deleted window had a Claude prompt buffer, close Claude code windows
           ((eq (buffer-local-value 'major-mode deleted-buffer) 'claude-code-prompt-mode)
            (dolist (claude-buffer (claude-code--find-all-claude-buffers))
              (dolist (claude-window (get-buffer-window-list claude-buffer))
                (when (window-live-p claude-window)
                  (delete-window claude-window)))))
           ;; If deleted window had a Claude code buffer, close prompt windows
           ((string-match-p "^\\*claude-code" (buffer-name deleted-buffer))
            ;; More aggressive approach - check all windows in all frames
            (dolist (frame (frame-list))
              (dolist (win (window-list frame))
                (when (and (window-live-p win)
                           (with-current-buffer (window-buffer win)
                             (eq major-mode 'claude-code-prompt-mode)))
                  (condition-case nil
                      (delete-window win)
                    (error nil))))))))))
    
    ;; Update tracked windows
    (setq claude-code-prompt-extensions--last-windows current-windows)))

(defun claude-code-prompt-extensions--buffer-killed (buffer)
  "Handle buffer being killed - close related windows."
  (cond
   ;; If killing a Claude prompt buffer, close Claude code windows
   ((eq (buffer-local-value 'major-mode buffer) 'claude-code-prompt-mode)
    (dolist (claude-buffer (claude-code--find-all-claude-buffers))
      (dolist (claude-window (get-buffer-window-list claude-buffer))
        (when (window-live-p claude-window)
          (delete-window claude-window)))))
   ;; If killing a Claude code buffer, close prompt windows
   ((string-match-p "^\\*claude-code" (buffer-name buffer))
    (dolist (frame (frame-list))
      (dolist (win (window-list frame))
        (when (and (window-live-p win)
                   (with-current-buffer (window-buffer win)
                     (eq major-mode 'claude-code-prompt-mode)))
          (condition-case nil
              (delete-window win)
            (error nil))))))))

(defun claude-code-prompt-extensions--setup-window-hooks ()
  "Set up window deletion hooks for automatic cleanup."
  ;; Initialize window tracking
  (setq claude-code-prompt-extensions--last-windows 
        (mapcar (lambda (w) (cons w (window-buffer w))) (window-list)))
  
  ;; Hook into window configuration changes
  (add-hook 'window-configuration-change-hook 
            #'claude-code-prompt-extensions--check-window-deletions)
  
  ;; Disable auto-show for now as it causes issues
  ;; (add-hook 'window-configuration-change-hook
  ;;           #'claude-code-prompt-extensions--auto-show-related-windows)
  
  ;; Hook into buffer killing
  (add-hook 'kill-buffer-hook
            (lambda ()
              (claude-code-prompt-extensions--buffer-killed (current-buffer))))
  
  ;; Advice on delete-window with direct cleanup
  (advice-add 'delete-window :before 
              (lambda (&optional window)
                (let* ((target-window (or window (selected-window)))
                       (target-buffer (and (window-live-p target-window) 
                                           (window-buffer target-window)))
                       (buffer-name (and target-buffer (buffer-name target-buffer))))
                  
                  ;; If deleting Claude code window, close all prompt windows
                  (when (and buffer-name (string-match-p "^\\*claude:" buffer-name))
                    (let ((prompt-windows (cl-remove-if-not
                                           (lambda (win)
                                             (and (window-live-p win)
                                                  (not (eq win target-window))
                                                  (condition-case nil
                                                      (eq (buffer-local-value 'major-mode (window-buffer win))
                                                          'claude-code-prompt-mode)
                                                    (error nil))))
                                           (window-list))))
                      (dolist (win prompt-windows)
                        ;; Avoid recursion by using run-with-timer with window validation
                        (run-with-timer 0.01 nil (lambda () 
                                                   (when (and (window-live-p win)
                                                             (window-valid-p win))
                                                     (delete-window win)))))))
                  
                  ;; If deleting prompt window, close all Claude code windows  
                  (when (and target-buffer
                             (condition-case nil
                                 (eq (buffer-local-value 'major-mode target-buffer) 'claude-code-prompt-mode)
                               (error nil)))
                    (dolist (claude-buffer (claude-code--find-all-claude-buffers))
                      (dolist (claude-window (get-buffer-window-list claude-buffer))
                        (when (and (window-live-p claude-window)
                                   (not (eq claude-window target-window)))
                          (run-with-timer 0.01 nil (lambda () 
                                                     (when (and (window-live-p claude-window)
                                                               (window-valid-p claude-window))
                                                       (delete-window claude-window)))))))))))
  
  ;; Add advice on quit-window which is often used to close side windows
  (advice-add 'quit-window :before
              (lambda (&optional kill window)
                (claude-code-prompt-extensions--close-related-windows
                 (or window (selected-window)))))
  
  ;; Add advice on delete-side-window specifically
  (when (fboundp 'delete-side-window)
    (advice-add 'delete-side-window :before
                (lambda (&optional window)
                  (claude-code-prompt-extensions--close-related-windows
                   (or window (selected-window))))))
  
  ;; Add advice on window-toggle-side-windows
  (when (fboundp 'window-toggle-side-windows)
    (advice-add 'window-toggle-side-windows :before
                (lambda (&optional frame)
                  ;; Close all Claude-related windows before toggling
                  (dolist (window (window-list frame))
                    (when (window-parameter window 'window-side)
                      (claude-code-prompt-extensions--close-related-windows window))))))
  
  ;; Add advice on claude-code function to auto-arrange windows
  (advice-add 'claude-code :after
              (lambda (&rest args)
                (run-with-timer 0.5 nil 
                               (lambda ()
                                 (claude-code-prompt-extensions--force-side-window-arrangement))))))

;;;; Prompt buffer functionality
(defun claude-code--prompt-buffer-name ()
  "Generate the prompt buffer name based on current project directory."
  (let ((dir (claude-code--directory)))
    (format "*claude-prompt:%s*" (abbreviate-file-name (file-truename dir)))))

(defun claude-code--get-prompt-buffer ()
  "Get or create the prompt buffer."
  (let ((buffer-name (claude-code--prompt-buffer-name)))
    (let ((buffer (get-buffer buffer-name)))
      (if buffer
          ;; Buffer exists, ensure it's in the correct mode
          (with-current-buffer buffer
            (unless (eq major-mode 'claude-code-prompt-mode)
              (claude-code-prompt-mode))
            buffer)
        ;; Create new buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-prompt-mode)
          (current-buffer))))))

(defun claude-code--prompt-buffer-p (buffer)
  "Return non-nil if BUFFER is a Claude prompt buffer."
  (let ((name (if (stringp buffer)
                  buffer
                (buffer-name buffer))))
    (and name (string-match-p "^\\*claude-prompt:" name))))

(defun claude-code--prompt-buffer-empty-p ()
  "Return non-nil if the prompt buffer is empty."
  (let ((buffer (claude-code--get-prompt-buffer)))
    (with-current-buffer buffer
      (= (buffer-size) 0))))

(defun claude-code--clear-prompt-buffer ()
  "Clear the contents of the prompt buffer."
  (let ((buffer (claude-code--get-prompt-buffer)))
    (with-current-buffer buffer
      (erase-buffer))))

(defun claude-code--close-prompt-buffer ()
  "Close the prompt buffer window if it exists."
  (let ((buffer (claude-code--get-prompt-buffer)))
    (when-let* ((window (get-buffer-window buffer)))
      (delete-window window))))

;;;; File insertion for prompt buffer
(defun claude-code--insert-file-at-point ()
  "Insert a file path at point using `project-find-file' interface."
  (interactive)
  (let ((project (project-current)))
    (if project
        ;; Use project completion table like project-find-file does
        (let* ((project-root (project-root project))
               (all-files (project-files project))
               (completion-table (project--file-completion-table all-files))
               (file (completing-read "Choose file: " completion-table nil t)))
          (when file
            (let ((relative-path (file-relative-name file project-root)))
              (insert relative-path))))
      ;; Fallback to regular find-file if no project
      (let ((file (read-file-name "Choose file: ")))
        (when file
          (insert (file-name-nondirectory file)))))))

(defun claude-code--handle-at-key ()
  "Handle @ key press - insert @ and prompt for file if not escaped."
  (interactive)
  (let ((preceding-char (char-before)))
    (if (eq preceding-char ?\\)
        ;; If preceded by backslash, just insert @
        (insert "@")
      ;; Otherwise, insert @ and prompt for file
      (insert "@")
      (claude-code--insert-file-at-point))))

;;;###autoload
(defun claude-code-show-prompt-buffer ()
  "Show the prompt buffer for composing Claude commands.

The prompt buffer supports:
- File insertion: Type @ to open `project-find-file' and insert selected file
- Literal @: Type \\@ to insert @ without file selection
- Key bindings: \\[claude-code-send-prompt] to send, \\[claude-code--close-prompt-buffer] to close
- Truncate lines enabled by default
- Automatic Claude startup if not running"
  (interactive)
  ;; Set flag to disable zoom mode completely during Claude operations
  (setq claude-code-prompt-extensions--zoom-disabled t)
  
  ;; Temporarily disable zoom mode FIRST to prevent interference
  (let ((zoom-mode-was-active (and (boundp 'zoom-mode) zoom-mode)))
    (when zoom-mode-was-active
      (zoom-mode -1)
      ;; Give time for zoom mode to fully disable
      (sit-for 0.05))
    
    ;; Start Claude if not running and force it to the right side
    (unless (claude-code--find-all-claude-buffers)
      ;; Start Claude first
      (claude-code)
      ;; Wait for Claude buffer to be created
      (sit-for 0.1)
      ;; Close any existing Claude windows and force to side window
      (when-let ((claude-buffer (claude-code--get-or-prompt-for-buffer)))
        ;; Close existing Claude windows
        (dolist (window (get-buffer-window-list claude-buffer))
          (when (window-live-p window)
            (delete-window window)))
        ;; Force Claude buffer to side window on right
        (display-buffer claude-buffer '((display-buffer-in-side-window)
                                        (side . right)
                                        (window-width . 90)))
        (sit-for 0.1)))
    
    (let* ((prompt-buffer (claude-code--get-prompt-buffer))
           (prompt-window (get-buffer-window prompt-buffer))
           (claude-buffer (claude-code--get-or-prompt-for-buffer))
           (claude-window (and claude-buffer (get-buffer-window claude-buffer))))
      
      ;; If prompt buffer is already visible, just switch to it
      (if prompt-window
          (progn
            (select-window prompt-window)
            (goto-char (point-max)))
        ;; If Claude window exists, force prompt to side window below it
        (if claude-window
            (progn
              ;; Force prompt buffer to side window below Claude
              (display-buffer prompt-buffer '((display-buffer-in-side-window)
                                              (side . right)
                                              (slot . 1)
                                              (window-height . 0.15)))
              (when-let ((new-prompt-window (get-buffer-window prompt-buffer)))
                (select-window new-prompt-window)))
          ;; No Claude window - force Claude to side window first
          (if claude-buffer
              (progn
                ;; Close any existing Claude windows and force to side window
                (dolist (window (get-buffer-window-list claude-buffer))
                  (when (window-live-p window)
                    (delete-window window)))
                ;; Force Claude buffer to side window on right
                (display-buffer claude-buffer '((display-buffer-in-side-window)
                                                (side . right)
                                                (window-width . 90)))
                (sit-for 0.1)
                ;; Update claude-window after forcing to side
                (setq claude-window (get-buffer-window claude-buffer))
                (if claude-window
                    ;; Force prompt buffer to side window below Claude
                    (progn
                      (display-buffer prompt-buffer '((display-buffer-in-side-window)
                                                      (side . right)
                                                      (slot . 1)
                                                      (window-height . 0.15)))
                      (when-let ((new-prompt-window (get-buffer-window prompt-buffer)))
                        (select-window new-prompt-window)))
                  ;; Still no Claude window, show at bottom
                  (display-buffer prompt-buffer '((display-buffer-at-bottom)
                                                  (window-height . 0.15)))
                  (when-let ((new-prompt-window (get-buffer-window prompt-buffer)))
                    (select-window new-prompt-window))))
            ;; No Claude buffer at all, show at bottom
            (display-buffer prompt-buffer '((display-buffer-at-bottom)
                                            (window-height . 0.15)))
            (when-let ((new-prompt-window (get-buffer-window prompt-buffer)))
              (select-window new-prompt-window)))))
      
      ;; Ensure truncate lines is enabled
      (with-current-buffer prompt-buffer
        (setq truncate-lines t)
        (goto-char (point-max)))
      
      ;; Force correct window size for prompt buffer (15% of frame height)
      (when-let ((prompt-window (get-buffer-window prompt-buffer)))
        (let* ((frame-height (frame-height))
               (desired-height (max 3 (round (* frame-height 0.15))))
               (current-height (window-height prompt-window))
               (delta (- desired-height current-height)))
          (when (not (zerop delta))
            (condition-case nil
                (window-resize prompt-window delta)
              (error nil)))
          ;; Set window parameters to prevent resizing
          (set-window-parameter prompt-window 'window-size-fixed 'height)
          (set-window-parameter prompt-window 'no-other-window t)))
      
      ;; Wait a bit before re-enabling zoom mode to ensure sizes are set
      (sit-for 0.1))
    
    ;; Re-enable zoom mode if it was active, but keep it disabled while Claude buffers are visible
    (when zoom-mode-was-active
      (run-with-timer 0.5 nil (lambda () 
                                ;; Only clear flag and re-enable if no Claude buffers are visible
                                (unless (cl-some (lambda (window)
                                                   (with-current-buffer (window-buffer window)
                                                     (or (eq major-mode 'claude-code-prompt-mode)
                                                         (string-match-p "^\\*claude" (buffer-name)))))
                                                 (window-list))
                                  (setq claude-code-prompt-extensions--zoom-disabled nil))
                                (zoom-mode 1))))
    ;; If not re-enabling zoom mode, don't clear the flag - let it stay disabled while Claude buffers are visible
    (unless zoom-mode-was-active
      ;; Don't clear the flag immediately - let the predicate handle it based on visible buffers
      nil)))

;;;###autoload
(defun claude-code-send-prompt ()
  "Send the contents of the prompt buffer to Claude.

After sending, clear the prompt buffer, close it, and show the Claude buffer."
  (interactive)
  ;; Get or create Claude buffer for current project
  (if-let* ((claude-buffer (claude-code--get-or-prompt-for-buffer)))
      (let ((buffer (claude-code--get-prompt-buffer)))
        (if (claude-code--prompt-buffer-empty-p)
            (message "Prompt buffer is empty")
          (let ((content (with-current-buffer buffer
                           (buffer-substring-no-properties (point-min) (point-max)))))
            (claude-code--clear-prompt-buffer)
            ;; Don't close prompt buffer, keep it open for next use
            ;; Send to the correct Claude buffer for this project
            (with-current-buffer claude-buffer
              (claude-code--term-send-string claude-code-terminal-backend content)
              (claude-code--term-send-string claude-code-terminal-backend (kbd "RET")))
            ;; Focus Claude buffer after sending, but keep prompt buffer open
            (when-let* ((claude-window (get-buffer-window claude-buffer)))
              (select-window claude-window))))))
  (message "Could not find or create Claude buffer for current project"))

;;;; File sending functions
;;;###autoload
(defun claude-code-send-current-file (&optional arg)
  "Send the current file path to Claude with @ prefix.

Gets the current file path relative to the project root (or absolute if
no project) and sends it to Claude with @ prefix for file operations.
If prompt buffer is not empty, sends to prompt buffer instead.

With prefix ARG, switch to the Claude buffer after sending."
  (interactive "P")
  (let* ((file-name (claude-code--get-buffer-file-name))
         (project-root (when (project-current)
                         (project-root (project-current))))
         (relative-path (if (and file-name project-root)
                            (file-relative-name file-name project-root)
                          file-name)))
    (if relative-path
        (if (not (claude-code--prompt-buffer-empty-p))
            ;; Send to prompt buffer
            (let ((prompt-buffer (claude-code--get-prompt-buffer)))
              (with-current-buffer prompt-buffer
                (goto-char (point-max))
                (when (and (> (point) (point-min))
                           (not (bolp)))
                  (insert " "))
                (insert (format "@%s " relative-path))))
          ;; Send to Claude buffer
          (if-let* ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
              (progn
                (with-current-buffer claude-code-buffer
                  ;; Check if current input line ends with space
                  (let* ((current-line (buffer-substring-no-properties
                                        (line-beginning-position) (point)))
                         (needs-prefix-space (not (string-match-p "\\s-$" current-line)))
                         (command (if needs-prefix-space
                                      (format " @%s " relative-path)
                                    (format "@%s " relative-path))))
                    (claude-code--term-send-string claude-code-terminal-backend command))
                  (display-buffer claude-code-buffer))
                (when arg
                  (pop-to-buffer claude-code-buffer)))
            (claude-code--show-not-running-message)))
      (message "No file associated with current buffer"))))

;;;###autoload
(defun claude-code-send-file (&optional arg)
  "Prompt for a file and send its path to Claude with @ prefix.

Uses completion to select a file from the project or current directory,
then sends it to Claude with @ prefix for file operations.
If prompt buffer is not empty, sends to prompt buffer instead.

With prefix ARG, switch to the Claude buffer after sending."
  (interactive "P")
  (let* ((project-root (when (project-current)
                         (project-root (project-current))))
         (default-directory (or project-root default-directory))
         (file-path (read-file-name "Send file to Claude: " default-directory))
         (relative-path (if project-root
                            (file-relative-name file-path project-root)
                          file-path)))
    (when file-path
      (if (not (claude-code--prompt-buffer-empty-p))
          ;; Send to prompt buffer
          (let ((prompt-buffer (claude-code--get-prompt-buffer)))
            (with-current-buffer prompt-buffer
              (goto-char (point-max))
              (when (and (> (point) (point-min))
                         (not (bolp)))
                (insert " "))
              (insert (format "@%s " relative-path))
              (goto-char (point-max)))
            ;; Temporarily switch to prompt buffer to set cursor position, then return
            (let ((original-buffer (current-buffer)))
              (switch-to-buffer prompt-buffer)
              (goto-char (point-max))
              (switch-to-buffer original-buffer))
            ;; Show quick feedback that file was added
            (message "Added @%s to prompt buffer" relative-path))
        ;; Send to Claude buffer
        (if-let* ((claude-code-buffer (claude-code--get-or-prompt-for-buffeikjir)))
            (progn
              (with-current-buffer claude-code-buffer
                ;; Check if current input line ends with space
                (let* ((current-line (buffer-substring-no-properties
                                      (line-beginning-position) (point)))
                       (needs-prefix-space (not (string-match-p "\\s-$" current-line)))
                       (command (if needs-prefix-space
                                    (format " @%s " relative-path)
                                  (format "@%s " relative-path))))
                  (claude-code--term-send-string claude-code-terminal-backend command))
                (display-buffer claude-code-buffer))
              (when arg
                (pop-to-buffer claude-code-buffer)))
          (claude-code--show-not-running-message))))))

;;;; Key bindings integration
;;;###autoload
(defun claude-code-prompt-extensions-setup ()
  "Set up key bindings for prompt extensions in claude-code.
This should be called after claude-code is loaded."
  (when (boundp 'claude-code-command-map)
    (define-key claude-code-command-map (kbd "p") 'claude-code-show-prompt-buffer)
    (define-key claude-code-command-map (kbd "P") 'claude-code-send-prompt)
    (define-key claude-code-command-map (kbd "F") 'claude-code-send-current-file)
    (define-key claude-code-command-map (kbd "S") 'claude-code-send-file)))

;;;; Transient menu integration
;;;###autoload
(defun claude-code-prompt-extensions-add-to-transient ()
  "Add prompt extensions to claude-code transient menu."
  (when (fboundp 'transient-append-suffix)
    (condition-case nil
        (progn
          ;; Add to "Send Commands to Claude" group
          (transient-append-suffix 'claude-code-transient '(0 1 -1)
            '("F" "Send current file" claude-code-send-current-file))
          (transient-append-suffix 'claude-code-transient '(0 1 -1)
            '("S" "Send file" claude-code-send-file))
          ;; Add new group for prompt buffer
          (transient-append-suffix 'claude-code-transient '(0 -1)
            '["Prompt Buffer"
              ("p" "Show prompt buffer" claude-code-show-prompt-buffer)
              ("P" "Send prompt to Claude" claude-code-send-prompt)]))
      (error 
       (message "Claude Code Prompt Extensions: Could not automatically add to transient menu")))))

;; Set up integration after claude-code is loaded
(with-eval-after-load 'claude-code
  (claude-code-prompt-extensions-setup)
  (claude-code-prompt-extensions-add-to-transient)
  (claude-code-prompt-extensions--setup-window-hooks))

;;;; Zoom mode integration
(defvar claude-code-prompt-extensions--zoom-disabled nil
  "Flag to track if we've temporarily disabled zoom mode.")

(defun claude-code-prompt-extensions--check-and-clear-zoom-flag ()
  "Check if Claude buffers are still visible and clear zoom flag if not."
  (when claude-code-prompt-extensions--zoom-disabled
    (unless (cl-some (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (or (eq major-mode 'claude-code-prompt-mode)
                             (string-match-p "^\\*claude" (buffer-name)))))
                     (window-list))
      (setq claude-code-prompt-extensions--zoom-disabled nil))))

(defun claude-code-prompt-extensions--zoom-ignore-p ()
  "Predicate function for zoom-mode to ignore Claude prompt buffers."
  ;; Auto-clear the flag if no Claude buffers are visible
  (claude-code-prompt-extensions--check-and-clear-zoom-flag)
  (or 
   ;; Always ignore when we've flagged zoom as disabled
   claude-code-prompt-extensions--zoom-disabled
   ;; Ignore if current buffer is a Claude prompt buffer
   (eq major-mode 'claude-code-prompt-mode)
   ;; Ignore if buffer name starts with *claude-prompt:
   (string-match-p "^\\*claude-prompt:" (buffer-name))
   ;; Ignore if buffer name starts with *claude-code
   (string-match-p "^\\*claude-code" (buffer-name))
   ;; Ignore if any visible window contains a Claude prompt buffer
   (cl-some (lambda (window)
              (with-current-buffer (window-buffer window)
                (eq major-mode 'claude-code-prompt-mode)))
            (window-list))
   ;; Ignore if any visible window contains a Claude code buffer
   (cl-some (lambda (window)
              (with-current-buffer (window-buffer window)
                (string-match-p "^\\*claude-code" (buffer-name))))
            (window-list))))

;;;###autoload
(defun claude-code-prompt-extensions-setup-zoom-integration ()
  "Set up zoom mode integration to ignore Claude buffers."
  (when (boundp 'zoom-ignore-predicates)
    (add-to-list 'zoom-ignore-predicates 'claude-code-prompt-extensions--zoom-ignore-p)))

;; Set up zoom mode integration when this file is loaded
(when (and (boundp 'zoom-mode) zoom-mode)
  (claude-code-prompt-extensions-setup-zoom-integration))

;; Also set up zoom integration when zoom mode is loaded later
(with-eval-after-load 'zoom
  (claude-code-prompt-extensions-setup-zoom-integration))

;;;; Provide the feature
(provide 'claude-code-prompt-extensions)

;;; claude-code-prompt-extensions.el ends here
