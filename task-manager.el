;;; task-manager2.el --- Enhanced task manager -*- lexical-binding: t -*-

;; Author: Assistant
;; Keywords: task management, todo
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (org "9.3"))

;;; Commentary:
;; A comprehensive task manager with sections for Inbox, Today, Week, etc.
;; Added functionalities: search, recurring tasks, priorities, reminders, tags, and export/import.
;; 
;; Key commands:
;;  a: Add a single task
;;  A: Add multiple tasks
;;  k: Delete selected tasks (move to Archive)
;;  K: Delete all tasks in a section
;;  m: Move selected tasks to another section
;;  RET: Edit task at current position
;;  z: Toggle expansion of all sections
;;  S: Search tasks
;;  s: Focus on Someday section
;;  r: Set task as recurring (daily, weekly, monthly)
;;  R: Clear recurring status
;;  V: View all recurring tasks
;;  F: Focus on Archive section
;;  p: Move to previous task
;;  n: Move to next task
;;  d: Set due date
;;  D: Clear due date
;;  T: Add tags
;;  t: Focus on Today section
;;  w: Focus on Week section
;;  W: Move all tasks from Inbox and Today to Week
;;  o: Focus on Monday section
;;  c: Focus on Calendar section
;;  C: Toggle commands visibility
;;  f: Filter tasks by properties
;;  X: Set reminders
;;  b: Bulk edit selected tasks
;;  x: Export tasks (org, json, csv)
;;  I: Import tasks
;;  i: Focus on Inbox section
;;  v: Save tasks
;;  L: Load tasks
;;  SPC: Toggle task selection
;;  u: Undo (up to 15 operations)
;;  n: Next task
;; 
;; Features:
;;  - URLs and file paths in tasks are automatically clickable
;; 
;; To start: M-x task-manager2-init

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'calendar)
(require 'autorevert)
(require 'org)

(defvar task-manager-sections
  '("Inbox" "Today" "Week" "Monday" "Calendar" "Someday" "Archive")
  "List of sections for organizing tasks.")

(defvar task-manager-tasks (make-hash-table :test 'equal)
  "Hash table storing tasks for each section.")

(defvar-local task-manager-selected-tasks nil
  "List of currently selected tasks.")

(defvar-local task-manager-expanded-sections (make-hash-table :test 'equal)
  "Hash table tracking expanded/collapsed state of sections.")

(defvar task-manager-save-file 
  (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/tasks.org")
  "File where task manager data is saved.")

;; Add commands visibility toggle variable
(defvar-local task-manager-show-commands nil
  "Whether to show commands in the task manager buffer.")

;; Add undo system - history of task states
(defvar task-manager-undo-history nil
  "Stack to keep track of task states for undo functionality.")

(defvar task-manager-undo-history-size 15
  "Maximum number of states to keep in the undo history.")

;; Define button types for links
(define-button-type 'task-url-link
  'action 'task-manager-browse-url-button
  'follow-link t
  'help-echo "Click to open this URL in browser"
  'face 'link)

(define-button-type 'task-file-link
  'action 'task-manager-find-file-button
  'follow-link t
  'help-echo "Click to open this file"
  'face 'link)

(defvar task-manager-backup-directory
  (expand-file-name "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/backups/")
  "Directory to store task manager backups.")

(defvar task-manager-backup-interval (* 2 60 60)  ; 2 hours in seconds
  "Interval between automatic backups in seconds.")

(defvar task-manager-last-backup-time 0
  "Timestamp of the last backup.")

(defvar task-manager-max-backups 3
  "Maximum number of backup files to keep.")

(defun task-manager-get-task-at-point ()
  "Get the task at the current point, returning a cons of (section . task)."
  (let ((task-found nil)
        (task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    
    ;; Check if we're on a task line
    (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)$" task-line)
      (let ((task-text (match-string 2 task-line))
            (section nil)
            (original-task nil))
        
        ;; Find the section and original task
        (dolist (sec task-manager-sections)
          (let ((tasks (gethash sec task-manager-tasks)))
            (dolist (full-task tasks)
              (when (string-match-p (regexp-quote task-text) full-task)
                (setq section sec)
                (setq original-task full-task)
                (setq task-found t)))))
        
        ;; Also check for "Due Today" tasks with section info in parenthesis
        (unless task-found
          (when (string-match "\\[\\([X ]\\)\\] \\(.*\\) (in \\(.*\\))" task-line)
            (setq task-text (match-string 2 task-line))
            (setq section (match-string 3 task-line))
            
            (let ((tasks (gethash section task-manager-tasks)))
              (dolist (full-task tasks)
                (when (string-match-p (regexp-quote task-text) full-task)
                  (setq original-task full-task)
                  (setq task-found t))))))
        
        ;; Return a cons of (section . task) if found
        (when task-found
          (cons section original-task))))))

(defun task-manager-find-task-section (task)
  "Find the section that contains TASK."
  (let ((section-found nil))
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks)))
        (when (and (not section-found) (member task tasks))
          (setq section-found section))))
    section-found))

(defun task-manager-all-tasks ()
  "Get a list of all tasks from all sections."
  (let ((all-tasks nil))
    (dolist (section task-manager-sections)
      (setq all-tasks (append all-tasks (gethash section task-manager-tasks))))
    all-tasks))

(defun task-manager-clear-recurring ()
  "Clear the recurring status from a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to clear recurring: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         ;; Remove any existing recurring tag
         (new-task (replace-regexp-in-string "\\[Recurring: [^]]*\\]" "" task))
         (new-task (string-trim new-task)))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (message "Recurring status cleared."))))

;;;###autoload
(defun task-manager2-init ()
  "Initialize the task manager."
  (interactive)
  ;; Initialize sections in hash table if needed
  (dolist (section task-manager-sections)
    (unless (gethash section task-manager-tasks)
      (puthash section '() task-manager-tasks)
      (puthash section t task-manager-expanded-sections)))
  
  ;; Try to load existing tasks
  (when (fboundp 'task-manager-load-tasks)
    (task-manager-load-tasks))
  
  ;; Open buffer
  (let ((buffer (get-buffer-create "*Task Manager*")))
    (with-current-buffer buffer
      (task-manager-mode)
      ;; Ensure commands are hidden on startup
      (setq-local task-manager-show-commands nil)
      (task-manager-refresh))
    (switch-to-buffer buffer)))

(define-derived-mode task-manager-mode special-mode "Task Manager"
  "Major mode for task management."
  (buffer-disable-undo)
  (setq buffer-read-only t)
  ;; Set up key bindings
  (define-key task-manager-mode-map (kbd "a") 'task-manager-add-task)
  (define-key task-manager-mode-map (kbd "A") 'task-manager-add-multiple-tasks)
  (define-key task-manager-mode-map (kbd "k") 'task-manager-delete-tasks)
  (define-key task-manager-mode-map (kbd "K") 'task-manager-delete-section-tasks)
  (define-key task-manager-mode-map (kbd "m") 'task-manager-move-tasks)
  (define-key task-manager-mode-map (kbd "RET") 'task-manager-edit-task-at-point)
  (define-key task-manager-mode-map (kbd "z") 'task-manager-toggle-all-sections)
  (define-key task-manager-mode-map (kbd "S") 'task-manager-search-tasks)
  (define-key task-manager-mode-map (kbd "s") 'task-manager-focus-someday)
  (define-key task-manager-mode-map (kbd "r") 'task-manager-set-recurring)
  (define-key task-manager-mode-map (kbd "R") 'task-manager-clear-recurring)
  (define-key task-manager-mode-map (kbd "V") 'task-manager-view-recurring)
  (define-key task-manager-mode-map (kbd "F") 'task-manager-focus-archive)
  (define-key task-manager-mode-map (kbd "p") 'task-manager-previous-task)
  (define-key task-manager-mode-map (kbd "n") 'task-manager-next-task)
  (define-key task-manager-mode-map (kbd "d") 'task-manager-set-due-date)
  (define-key task-manager-mode-map (kbd "D") 'task-manager-clear-due-date)
  (define-key task-manager-mode-map (kbd "T") 'task-manager-add-tags)
  (define-key task-manager-mode-map (kbd "t") 'task-manager-focus-today)
  (define-key task-manager-mode-map (kbd "w") 'task-manager-focus-week)
  (define-key task-manager-mode-map (kbd "W") 'task-manager-move-to-week)
  (define-key task-manager-mode-map (kbd "o") 'task-manager-focus-monday)
  (define-key task-manager-mode-map (kbd "c") 'task-manager-focus-calendar)
  (define-key task-manager-mode-map (kbd "C") 'task-manager-toggle-commands)
  (define-key task-manager-mode-map (kbd "f") 'task-manager-filter-tasks)
  (define-key task-manager-mode-map (kbd "X") 'task-manager-setting-reminders)
  (define-key task-manager-mode-map (kbd "b") 'task-manager-bulk-edit)
  (define-key task-manager-mode-map (kbd "x") 'task-manager-export)
  (define-key task-manager-mode-map (kbd "I") 'task-manager-import)
  (define-key task-manager-mode-map (kbd "i") 'task-manager-focus-inbox)
  (define-key task-manager-mode-map (kbd "v") 'task-manager-save-tasks)
  (define-key task-manager-mode-map (kbd "L") 'task-manager-load-tasks)
  (define-key task-manager-mode-map (kbd "SPC") 'task-manager-toggle-task-at-point))

;; Add the undo keybinding explicitly after mode definition
(define-key task-manager-mode-map (kbd "u") 'task-manager-undo)

;; Add the clear due date keybinding explicitly after mode definition
(define-key task-manager-mode-map (kbd "D") 'task-manager-clear-due-date)

;; Add the next task keybinding
(define-key task-manager-mode-map (kbd "n") 'task-manager-next-task)

(defun task-manager-toggle-commands ()
  "Toggle the visibility of the commands section."
  (interactive)
  (setq task-manager-show-commands (not task-manager-show-commands))
  (task-manager-refresh)
  (message "Commands %s" (if task-manager-show-commands "shown" "hidden")))

(defun task-manager-browse-url-button (button)
  "Action function for URL buttons that works in read-only buffers."
  (let ((url (button-get button 'url)))
    (when url
      (browse-url url))))

(defun task-manager-find-file-button (button)
  "Action function for file path buttons that works in read-only buffers."
  (let ((file-path (button-get button 'file-path)))
    (when file-path
      (find-file file-path))))

(defun task-manager-linkify-text (text)
  "Make URLs and file paths in TEXT clickable."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    
    ;; Detect and linkify URLs
    (while (re-search-forward "\\b\\(https?://\\|www\\.\\)[^ \t\n\r,;()\"']+" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (match (match-string 0))
             (url (if (string-match-p "^www\\." match)
                      (concat "http://" match)
                    match)))
        ;; Use make-button which works better in read-only buffers
        (delete-region start end)
        (insert-button match
                       :type 'task-url-link
                       'url url)))
    
    ;; Detect and linkify file paths
    (goto-char (point-min))
    (while (re-search-forward "\\b\\(file://\\|~/\\|/\\)[^ \t\n\r,;()\"']+" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (match (match-string 0))
             (file-path (if (string-match-p "^file:" match)
                           (substring match 5)
                         match)))
        ;; Use make-button which works better in read-only buffers
        (delete-region start end)
        (insert-button match 
                       :type 'task-file-link
                       'file-path file-path)))
    
    (buffer-string)))

(defun task-manager-refresh ()
  "Refresh the display of tasks in the task manager."
  (let* ((inhibit-read-only t)
         (today (format-time-string "%Y-%m-%d"))
         ;; Save cursor position information before refresh
         (old-line (line-number-at-pos))
         (old-column (current-column))
         (old-point (point))
         ;; Try to identify what we're looking at for better position recovery
         (line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (on-task-line (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line-content))
         (on-section-header nil)
         (task-text nil)
         (section-name nil)
         (marker (point-marker)))
    
    ;; Identify what we're looking at
    (when on-task-line
      (setq task-text (match-string 2 line-content)))
    
    ;; Check if we're on a section header line
    (dolist (section task-manager-sections)
      (when (string-match-p (concat "^" (regexp-quote section) " (") line-content)
        (setq on-section-header t)
        (setq section-name section)))
    
    ;; Refresh buffer content
    (erase-buffer)
    (insert "GTD + Emacs\n")
    (insert "============\n\n")
    
    ;; Display tasks due today at the top
    (insert (propertize "Due Today" 'face '(:inherit font-lock-keyword-face :weight bold)))
    (insert " [-]\n")
    (let ((due-today-tasks nil))
      ;; Find tasks due today or with daily recurring
      (dolist (section task-manager-sections)
        (let ((tasks (gethash section task-manager-tasks)))
          (dolist (task tasks)
            (when (or 
                   ;; Task with today's due date
                   (and (string-match "\\[Due: \\([^]]+\\)\\]" task)
                        (string= (match-string 1 task) today))
                   ;; Daily recurring task
                   (string-match-p "\\[Recurring: daily\\]" task))
              (push (cons section task) due-today-tasks)))))
      
      (if due-today-tasks
          (progn
            (dolist (task-pair due-today-tasks)
              (let* ((section (car task-pair))
                     (task (cdr task-pair))
                     (selected (member task task-manager-selected-tasks))
                     (display-task (replace-regexp-in-string "^TODAY " "" task)))
                ;; Insert checkbox marker
                (insert (format "  [%s] " (if selected "X" " ")))
                
                ;; Process and insert the task with active links
                (let ((link-start 0)
                      (processed-text ""))
                  ;; Find URLs and insert as buttons
                  (while (string-match "\\(https?://[^ \t\n\r,;()\"']+\\)" display-task link-start)
                    (let ((start (match-beginning 1))
                          (end (match-end 1))
                          (url (match-string 1 display-task)))
                      ;; Add text before link
                      (insert (substring display-task link-start start))
                      ;; Add button for link
                      (insert-button url
                                    :type 'task-url-link
                                    'url url)
                      (setq link-start end)))
                  
                  ;; Add any remaining text after last link
                  (insert (substring display-task link-start)))
                
                (insert (format " (in %s)\n" section))))
            (insert "\n"))
        (insert "  No tasks due today\n\n")))
    
    ;; Display ALL sections with tasks
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks))
            (expanded (gethash section task-manager-expanded-sections)))
        (insert (propertize (format "%s (%d)" section (length tasks))
                            'face 'font-lock-keyword-face))
        (if expanded
            (insert " [-]\n")
          (insert " [+]\n"))
        
        ;; Show tasks if section is expanded
        (when expanded
          (dolist (task tasks)
            (let ((selected (member task task-manager-selected-tasks))
                  (display-task (replace-regexp-in-string "^TODAY " "" task)))
              ;; Insert checkbox marker
              (insert (format "  [%s] " (if selected "X" " ")))
              
              ;; Process and insert the task with active links
              (let ((link-start 0))
                ;; Find URLs and insert as buttons
                (while (string-match "\\(https?://[^ \t\n\r,;()\"']+\\)" display-task link-start)
                  (let ((start (match-beginning 1))
                        (end (match-end 1))
                        (url (match-string 1 display-task)))
                    ;; Add text before link
                    (insert (substring display-task link-start start))
                    ;; Add button for link
                    (insert-button url
                                  :type 'task-url-link
                                  'url url)
                    (setq link-start end)))
                
                ;; Add any remaining text after last link
                (insert (substring display-task link-start)))
              
              (insert "\n"))))))
    
    ;; Display commands if enabled, otherwise show a hint
    (if task-manager-show-commands
        (progn
          (insert "\nCommands:\n")
          (insert "  a: Add a single task\n")
          (insert "  A: Add multiple tasks\n")
          (insert "  k: Delete selected tasks (move to Archive)\n")
          (insert "  K: Delete all tasks in a section\n")
          (insert "  m: Move selected tasks\n")
          (insert "  RET: Edit task\n")
          (insert "  z: Expand/collapse all sections\n")
          (insert "  S: Search tasks\n")
          (insert "  s: Focus on Someday section\n")
          (insert "  r: Set recurring task\n")
          (insert "  R: Clear recurring attribute\n")
          (insert "  V: View all recurring tasks\n")
          (insert "  F: Focus on Archive section\n")
          (insert "  p: Previous task\n")
          (insert "  n: Next task\n")
          (insert "  d: Set due date\n")
          (insert "  D: Clear due date\n")
          (insert "  T: Add or modify tags\n")
          (insert "  t: Focus on Today section\n")
          (insert "  w: Focus on Week section\n")
          (insert "  o: Focus on Monday section\n")
          (insert "  c: Focus on Calendar section\n")
          (insert "  C: Toggle commands help\n")
          (insert "  f: Filter tasks\n")
          (insert "  X: Set reminder\n")
          (insert "  b: Bulk edit selected tasks\n")
          (insert "  x: Export tasks\n")
          (insert "  I: Import tasks\n")
          (insert "  i: Focus on Inbox section\n")
          (insert "  v: Save tasks\n")
          (insert "  L: Load tasks\n")
          (insert "  SPC: Toggle task selection\n")
          (insert "  u: Undo last action\n"))
      (insert "\nPress 'C' to show commands"))
    
    ;; Now restore cursor position intelligently
    (goto-char (point-min))
    (cond
     ;; If we were on a task line, try to find that task again
     ((and on-task-line task-text)
      (let ((found nil))
        (while (and (not found) (< (point) (point-max)))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                     (string-match-p (regexp-quote task-text) (match-string 2 line)))
              (setq found t)
              (beginning-of-line))
            (unless found
              (forward-line 1))))))
     
     ;; If we were on a section header, try to find that section again
     ((and on-section-header section-name)
      (let ((found nil))
        (while (and (not found) (< (point) (point-max)))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (string-match-p (concat "^" (regexp-quote section-name) " (") line)
              (setq found t)
              (beginning-of-line))
            (unless found
              (forward-line 1))))))
     
     ;; Otherwise try to restore by line number
     (t
      (forward-line (1- old-line))
      (move-to-column old-column)))
    
    ;; Set buffer read-only
    (setq buffer-read-only t)))

;; Task Functions
(defun task-manager-add-task (section)
  "Add a task to SECTION."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (let ((task (read-string "Enter new task: ")))
    (unless (string-empty-p task)
      ;; Add the task to the specified section
      (push task (gethash section task-manager-tasks))
      ;; Save and refresh the view
      (task-manager-save-tasks)
      (task-manager-refresh)
      ;; Position cursor at the new task
      (when (get-buffer "*Task Manager*")
        (switch-to-buffer "*Task Manager*")
        ;; Navigate to the section
        (goto-char (point-min))
        (when (search-forward (format "%s (" section) nil t)
          ;; Make sure section is expanded
          (let ((expanded (gethash section task-manager-expanded-sections)))
            (unless expanded
              (task-manager-toggle-section-at-point)
              (task-manager-refresh))
            ;; Navigate to the first task in section (which should be our new task)
            (forward-line 1)
            (beginning-of-line)))
        (message "Task added to %s" section))
      
      ;; Return to Task Manager buffer if we somehow left it
      (when (and (not (string= (buffer-name) "*Task Manager*"))
               (get-buffer "*Task Manager*"))
        (switch-to-buffer "*Task Manager*")))))

(defun task-manager-add-multiple-tasks (section)
  "Add multiple tasks to SECTION."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  
  ;; Create a new buffer for task entry
  (let ((buffer-name "*Task Manager Multi-Add*")
        (target-section section))
    (with-current-buffer (get-buffer-create buffer-name)
      (text-mode)
      (erase-buffer)
      
      ;; Display instructions
      (insert "# Enter each task on a separate line\n")
      (insert "# When done, press C-c C-c to add tasks and exit\n")
      (insert "# Press C-c C-k to cancel\n")
      (insert "# ----------------------------------------\n\n")
      
      ;; Create a new keymap for this buffer
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map text-mode-map)
        
        ;; Add tasks and exit
        (define-key map (kbd "C-c C-c") 
                    `(lambda () 
                      (interactive)
                      ;; Capture tasks from buffer
                      (let ((tasks (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))
                            (count 0))
                        ;; Filter out comment lines
                        (setq tasks (seq-filter (lambda (line) (not (string-prefix-p "#" (string-trim line)))) tasks))
                        
                        ;; Add the tasks directly inside this function
                        (dolist (task tasks)
                          (unless (string-empty-p task)
                            (push task (gethash ',target-section task-manager-tasks))
                            (setq count (1+ count))))
                        
                        ;; Save and update display
                        (when (> count 0)
                          (task-manager-save-tasks)
                          (message "Added %d task(s) to %s section" count ',target-section)))
                      
                      ;; Kill the temp buffer
                      (kill-buffer ,buffer-name)
                      
                      ;; Switch to task manager buffer
                      (when (get-buffer "*Task Manager*")
                        (switch-to-buffer "*Task Manager*")
                        (task-manager-refresh)
                        ;; Position cursor at the section with new tasks
                        (goto-char (point-min))
                        (when (search-forward (format "%s (" ',target-section) nil t)
                          ;; Make sure section is expanded
                          (let ((expanded (gethash ',target-section task-manager-expanded-sections)))
                            (unless expanded
                              (task-manager-toggle-section-at-point)
                              (task-manager-refresh))
                            ;; Navigate to the first task in section
                            (forward-line 1)
                            (beginning-of-line))))))
        
        ;; Cancel without adding tasks
        (define-key map (kbd "C-c C-k")
                    `(lambda ()
                      (interactive)
                      (message "Task addition cancelled")
                      (kill-buffer ,buffer-name)
                      
                      ;; Switch back to task manager buffer
                      (when (get-buffer "*Task Manager*")
                        (switch-to-buffer "*Task Manager*"))))
        
        ;; Install the keymap
        (use-local-map map)
        (message "Type your tasks (one per line). Press C-c C-c when done, C-c C-k to cancel."))
      
      ;; Display the buffer for the user to edit
      (switch-to-buffer buffer-name))))

(defun task-manager-delete-tasks ()
  "Move selected tasks to Archive section if they're not in Archive,
otherwise permanently delete them."
  (interactive)
  (when task-manager-selected-tasks
    (let ((archive-tasks (gethash "Archive" task-manager-tasks))
          (tasks-to-archive nil)
          (tasks-to-delete nil)
          (current-line (line-number-at-pos))
          (current-column (current-column))
          (current-section nil)
          (current-line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (next-task-marker nil)
          (previous-task-marker nil))
      
      ;; Try to determine the current section and find adjacent tasks for cursor positioning
      (save-excursion
        (beginning-of-line)
        ;; If we're on a task line, remember it and find adjacent tasks
        (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" current-line-content)
          ;; Try to find next task for positioning after deletion
          (forward-line 1)
          (let ((next-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" next-line)
              (setq next-task-marker (match-string 2 next-line))))
          
          ;; Try to find previous task for positioning
          (forward-line -2) ;; Go back two lines
          (when (not (bobp))
            (let ((prev-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" prev-line)
                (setq previous-task-marker (match-string 2 prev-line))))))
        
        ;; Find current section
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            ;; Try to find the current section
            (dolist (section task-manager-sections)
              (when (string-match-p (concat "^" (regexp-quote section) " (") line)
                (when (< (line-number-at-pos) current-line)
                  (setq current-section section))))
            (forward-line 1))))
      
      ;; Save current state to undo history
      (task-manager-push-to-undo-history)
      
      ;; Separate tasks between those to archive and those to delete
      (dolist (task task-manager-selected-tasks)
        (let ((found nil))
          ;; Find the tasks in their sections
          (dolist (section task-manager-sections)
            (let ((tasks (gethash section task-manager-tasks)))
              (when (and (member task tasks) (not found))
                (setq found t)
                (if (string= section "Archive")
                    ;; If task is in Archive, mark for deletion
                    (push task tasks-to-delete)
                  ;; Otherwise mark for archiving
                  (push (cons section task) tasks-to-archive)))))))
      
      ;; Process tasks to archive
      (let ((archive-count 0))
        (dolist (section-task tasks-to-archive)
          (let ((section (car section-task))
                (task (cdr section-task)))
            ;; Remove from current section
            (setf (gethash section task-manager-tasks)
                  (remove task (gethash section task-manager-tasks)))
            ;; Add to Archive
            (push task archive-tasks)
            (setq archive-count (1+ archive-count))))
        
        ;; Update Archive with newly archived tasks
        (when (> archive-count 0)
          (puthash "Archive" archive-tasks task-manager-tasks)
          (message "%d task(s) moved to Archive section." archive-count)))
      
      ;; Process tasks to delete (from Archive)
      (when tasks-to-delete
        (if (y-or-n-p (format "Permanently delete %d task(s) from Archive?" 
                             (length tasks-to-delete)))
            (progn
              ;; Remove tasks from Archive
              (setf (gethash "Archive" task-manager-tasks)
                    (seq-filter (lambda (task) 
                                 (not (member task tasks-to-delete)))
                               (gethash "Archive" task-manager-tasks)))
              (message "%d task(s) permanently deleted." (length tasks-to-delete)))
          (message "Deletion cancelled.")))
      
      ;; Clear selected tasks
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Smart cursor positioning after task deletion
      (cond
       ;; If we have next task marker, try to find it and position there
       ((and next-task-marker (not (string-empty-p next-task-marker)))
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (< (point) (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                       (string-match-p (regexp-quote next-task-marker) (match-string 2 line)))
                (setq found t)
                (beginning-of-line))
              (unless found
                (forward-line 1))))
          ;; If next task not found, try previous task
          (when (and (not found) previous-task-marker (not (string-empty-p previous-task-marker)))
            (goto-char (point-min))
            (while (and (not found) (< (point) (point-max)))
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                         (string-match-p (regexp-quote previous-task-marker) (match-string 2 line)))
                  (setq found t)
                  (beginning-of-line))
                (unless found
                  (forward-line 1)))))))
       
       ;; If we know the section, go there
       (current-section
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (< (point) (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (string-match-p (concat "^" (regexp-quote current-section) " (") line)
                (setq found t)
                (forward-line 1))
              (unless found
                (forward-line 1))))))
       
       ;; Otherwise try to restore position by line number
       (t
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column)))))))

(defun task-manager-delete-section-tasks ()
  "Delete all tasks in a chosen section permanently."
  (interactive)
  (let* ((section (completing-read "Delete all tasks from section: " 
                                  task-manager-sections)))
    (when (and section
               (y-or-n-p (format "Permanently delete ALL tasks from %s? " section)))
      ;; Empty the section
      (puthash section nil task-manager-tasks)
      ;; Clear any of these tasks from selection
      (setq task-manager-selected-tasks 
            (seq-filter (lambda (task)
                          (not (member task (gethash section task-manager-tasks))))
                        task-manager-selected-tasks))
      (task-manager-save-tasks)
      (task-manager-refresh)
      (message "All tasks from %s permanently deleted." section))))

(defun task-manager-move-tasks ()
  "Move selected tasks to another section."
  (interactive)
  (if (null task-manager-selected-tasks)
      (message "No tasks selected. Select tasks with SPC first.")
    (let ((target (completing-read "Move to section: " task-manager-sections))
          (current-line (line-number-at-pos))
          (current-column (current-column))
          (current-section nil)
          (current-line-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (next-task-marker nil)
          (previous-task-marker nil)
          (first-moved-task nil))
      
      ;; Try to determine the current section and find adjacent tasks for cursor positioning
      (save-excursion
        (beginning-of-line)
        ;; If we're on a task line, remember it and find adjacent tasks
        (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" current-line-content)
          ;; Remember this task if it will be moved
          (let ((task-text (match-string 2 current-line-content)))
            (dolist (task task-manager-selected-tasks)
              (when (string-match-p (regexp-quote task-text) task)
                (setq first-moved-task task))))
          
          ;; Try to find next task for positioning after move
          (forward-line 1)
          (let ((next-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" next-line)
              (setq next-task-marker (match-string 2 next-line))))
          
          ;; Try to find previous task for positioning
          (forward-line -2) ;; Go back two lines
          (when (not (bobp))
            (let ((prev-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" prev-line)
                (setq previous-task-marker (match-string 2 prev-line))))))
        
        ;; Find current section
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            ;; Try to find the current section
            (dolist (section task-manager-sections)
              (when (string-match-p (concat "^" (regexp-quote section) " (") line)
                (when (< (line-number-at-pos) current-line)
                  (setq current-section section))))
            (forward-line 1))))
      
      ;; Save current state to undo history
      (task-manager-push-to-undo-history)
      
      ;; Move tasks
      (dolist (task task-manager-selected-tasks)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash target task-manager-tasks))))))
      
      ;; Clear selected tasks
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Smart cursor positioning after task move
      (cond
       ;; If we were moving a task we were on, try to find it in target section
       ((and first-moved-task target)
        (goto-char (point-min))
        (let ((found-section nil))
          (while (and (not found-section) (< (point) (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (string-match-p (concat "^" (regexp-quote target) " (") line)
                (setq found-section t)
                (forward-line 1) ;; Move to first task line after section header
                ;; Now try to find the moved task
                (let ((found-task nil))
                  (while (and (not found-task) (< (point) (point-max)))
                    (let ((task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                      (if (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" task-line)
                               (string-match-p (regexp-quote first-moved-task) task-line))
                          (setq found-task t)
                        (forward-line 1))))))
              (unless found-section
                (forward-line 1))))))
       
       ;; If we have next task marker, try to find it and position there
       ((and next-task-marker (not (string-empty-p next-task-marker)))
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (< (point) (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                        (string-match-p (regexp-quote next-task-marker) (match-string 2 line)))
                (setq found t)
                (beginning-of-line))
              (unless found
                (forward-line 1))))
          ;; If next task not found, try previous task
          (when (and (not found) previous-task-marker (not (string-empty-p previous-task-marker)))
            (goto-char (point-min))
            (while (and (not found) (< (point) (point-max)))
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                (when (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                          (string-match-p (regexp-quote previous-task-marker) (match-string 2 line)))
                  (setq found t)
                  (beginning-of-line))
                (unless found
                  (forward-line 1)))))))
       
       ;; If we know the section, go there
       (current-section
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (< (point) (point-max)))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (string-match-p (concat "^" (regexp-quote current-section) " (") line)
                (setq found t)
                (forward-line 1))
              (unless found
                (forward-line 1))))))
       
       ;; Otherwise try to restore position by line number
       (t
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column)))
      
      (message "Tasks moved to %s" target))))

(defun task-manager-toggle-all-sections ()
  "Toggle expansion of all sections."
  (interactive)
  (let ((all-expanded t))
    ;; Check if all sections are expanded
    (dolist (section task-manager-sections)
      (when (not (gethash section task-manager-expanded-sections))
        (setq all-expanded nil)))
    
    ;; Toggle all sections to the opposite state
    (dolist (section task-manager-sections)
      (puthash section (not all-expanded) task-manager-expanded-sections))
    
    (task-manager-refresh)))

(defun task-manager-toggle-task-at-point ()
  "Toggle selection of task at point."
  (interactive)
  (let ((task-found nil)
        (current-line (line-number-at-pos))
        (current-column (current-column))
        (task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    ;; Check if we're on a task line (which may now contain text properties for links)
    (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*?\\)\\(?: (in \\(.*\\))?\\)?$" task-line)
      (let ((raw-task (match-string 2 task-line))
            (section-info (match-string 3 task-line)) ; for "Due Today" section
            (found-section nil)
            (found-full-task nil))
        
        ;; Find the original task with metadata
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (dolist (full-task tasks)
              ;; Remove metadata for plain text comparison
              (let* ((stripped-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" full-task))
                     (stripped-task (string-trim stripped-task)))
                (when (or (string-match-p (regexp-quote raw-task) stripped-task)
                          (string-match-p (regexp-quote stripped-task) raw-task))
                  (setq task-found t)
                  (setq found-section section)
                  (setq found-full-task full-task)
                  (if (member full-task task-manager-selected-tasks)
                      (setq task-manager-selected-tasks 
                            (remove full-task task-manager-selected-tasks))
                    (push full-task task-manager-selected-tasks)))))))
        
        ;; If it's a "Due Today" task with section info, use that
        (when (and (not task-found) section-info)
          (let ((tasks (gethash section-info task-manager-tasks)))
            (dolist (full-task tasks)
              (let* ((stripped-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" full-task))
                     (stripped-task (string-trim stripped-task)))
                (when (or (string-match-p (regexp-quote raw-task) stripped-task)
                          (string-match-p (regexp-quote stripped-task) raw-task))
                  (if (member full-task task-manager-selected-tasks)
                      (setq task-manager-selected-tasks 
                            (remove full-task task-manager-selected-tasks))
                    (push full-task task-manager-selected-tasks))
                  (setq found-section section-info)
                  (setq found-full-task full-task)
                  (setq task-found t))))))
        
        ;; Refresh and position cursor
        (when task-found
          (task-manager-save-tasks)
          (task-manager-refresh)))))
    
    (unless task-found
      (message "No task found at point or unable to toggle selection."))))

;; Persistence Functions
(defun task-manager-push-to-undo-history ()
  "Save current task state to undo history."
  (let ((current-state (make-hash-table :test 'equal)))
    ;; Deep copy the tasks hash table
    (maphash (lambda (section tasks)
               (puthash section (copy-sequence tasks) current-state))
             task-manager-tasks)
    (push current-state task-manager-undo-history)
    ;; Limit history size
    (when (> (length task-manager-undo-history) task-manager-undo-history-size)
      (setq task-manager-undo-history (butlast task-manager-undo-history)))))

(defun task-manager-undo ()
  "Undo the last operation in task manager."
  (interactive)
  (if (null task-manager-undo-history)
      (message "Nothing to undo")
    (let ((previous-state (pop task-manager-undo-history)))
      ;; Create a fresh hash table
      (setq task-manager-tasks (make-hash-table :test 'equal))
      ;; Copy contents from previous state to current state
      (maphash (lambda (section tasks)
                 (puthash section (copy-sequence tasks) task-manager-tasks))
               previous-state)
      (task-manager-refresh)
      (message "Undid last operation"))))

(defun task-manager-ensure-backup-directory ()
  "Ensure the backup directory exists."
  (unless (file-exists-p task-manager-backup-directory)
    (make-directory task-manager-backup-directory t)))

(defun task-manager-cleanup-backups ()
  "Keep only the maximum number of latest backup files."
  (when (file-exists-p task-manager-backup-directory)
    (let* ((backup-files (directory-files task-manager-backup-directory t "tasks-backup-.*\\.org$"))
           (sorted-backups (sort backup-files (lambda (a b)
                                                (let ((time-a (file-attribute-modification-time (file-attributes a)))
                                                      (time-b (file-attribute-modification-time (file-attributes b))))
                                                  (time-less-p time-b time-a))))))
      ;; Delete old backups exceeding the maximum
      (when (> (length sorted-backups) task-manager-max-backups)
        (dolist (old-backup (nthcdr task-manager-max-backups sorted-backups))
          (when (file-exists-p old-backup)
            (delete-file old-backup)
            (message "Deleted old backup: %s" old-backup)))))))

(defun task-manager-create-backup ()
  "Create a backup of the tasks.org file."
  (task-manager-ensure-backup-directory)
  (when (file-exists-p task-manager-save-file)
    (let* ((current-time (current-time))
           (timestamp (format-time-string "%Y%m%d-%H%M%S" current-time))
           (backup-file (expand-file-name (format "tasks-backup-%s.org" timestamp)
                                         task-manager-backup-directory)))
      ;; Instead of copying the file directly, we'll recreate it with consistent section names
      (with-temp-buffer
        ;; Add header
        (insert "#+TITLE: Task Manager Data\n\n")
        
        ;; Write sections with consistent names
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when tasks
              (insert (format "* %s\n" section))
              (dolist (task (reverse tasks)) ;; reverse to maintain order
                (insert (format "** %s\n" task))))))
        
        ;; Write to backup file
        (write-region (point-min) (point-max) backup-file))
      
      (setq task-manager-last-backup-time (float-time current-time))
      (message "Created backup of tasks.org at %s" backup-file)
      
      ;; Clean up old backups
      (task-manager-cleanup-backups))))

(defun task-manager-backup-if-needed ()
  "Create a backup if enough time has passed since the last backup."
  (let ((current-time (float-time)))
    (when (> (- current-time task-manager-last-backup-time) task-manager-backup-interval)
      (task-manager-create-backup))))

(defun task-manager-save-tasks ()
  "Save tasks to the save file using the same section names."
  (interactive)
  ;; Only push to history when saving as a result of a change, not on startup
  (when (called-interactively-p 'any)
    (task-manager-push-to-undo-history))
  
  ;; Check if backup is needed
  (task-manager-backup-if-needed)
  
  ;; Read existing file to preserve structure and format
  (let ((existing-content ""))
    
    ;; Try to read existing content to preserve formatting
    (when (file-exists-p task-manager-save-file)
      (setq existing-content (with-temp-buffer
                               (insert-file-contents task-manager-save-file)
                               (buffer-string))))
    
    ;; Create new content
    (with-temp-buffer
      ;; If there's no existing content, add standard header
      (if (string-empty-p existing-content)
          (insert "#+TITLE: Task Manager\n\n")
        ;; Otherwise preserve header until first heading
        (string-match "^\\* " existing-content)
        (let ((header (substring existing-content 0 (match-beginning 0))))
          (insert header)))
      
      ;; Write sections and tasks directly with the original names
      (dolist (section task-manager-sections)
        (let ((tasks (gethash section task-manager-tasks)))
          (when tasks
            (insert (format "* %s\n" section))
            (dolist (task (reverse tasks)) ;; reverse to maintain order
              (insert (format "** %s\n" task))))))
      
      ;; Write to file
      (write-region (point-min) (point-max) task-manager-save-file))
    
    (message "Tasks saved to %s" task-manager-save-file)))

(defun task-manager-load-tasks ()
  "Load tasks from the save file."
  (interactive)
  ;; Initialize empty lists for all sections
  (dolist (section task-manager-sections)
    (puthash section nil task-manager-tasks))
  
  (when (file-exists-p task-manager-save-file)
    ;; Read and parse the org file
    (with-temp-buffer
      (insert-file-contents task-manager-save-file)
      (org-mode)
      (goto-char (point-min))
      (let ((current-section nil))
        (while (not (eobp))
          (cond
           ;; Section header (level 1 heading)
           ((looking-at "^\\* \\(.+\\)$")
            (setq current-section (match-string 1))
            ;; Make sure section exists (handle custom sections)
            (unless (member current-section task-manager-sections)
              (add-to-list 'task-manager-sections current-section)
              (puthash current-section nil task-manager-tasks)))
           
           ;; Task (level 2 heading)
           ((and current-section
                 (looking-at "^\\*\\* \\(.+\\)$"))
            (let ((task (match-string 1)))
              (when (and task (not (string-empty-p (string-trim task))))
                (push task (gethash current-section task-manager-tasks))))))
          (forward-line)))
      
      ;; Custom tasks processing after loading
      (message "Tasks loaded from %s" task-manager-save-file))))

;; New Functions

(defun task-manager-search-tasks ()
  "Search for tasks across all sections."
  (interactive)
  (let ((search-term (read-string "Search for: "))
        (results-buffer (get-buffer-create "*Task Search Results*")))
    (with-current-buffer results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Search results for \"%s\":\n\n" search-term))
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks))
                (found nil))
            (dolist (task tasks)
              (when (string-match-p search-term task)
                (setq found t)
                (let ((display-task (replace-regexp-in-string "^TODAY " "" task)))
                  ;; Insert clickable task result
                  (insert "  ")
                  (insert-text-button
                   (format "%s: %s" section display-task)
                   'action (lambda (_)
                            (task-manager-goto-task section task))
                   'follow-link t
                   'help-echo "Click to go to this task")
                  (insert "\n"))))
            (when found
              (insert "\n"))))
        (goto-char (point-min)))
      (special-mode)
      ;; Add key to go back to task manager
      (local-set-key (kbd "q") 'kill-this-buffer))
    (switch-to-buffer results-buffer)))

(defun task-manager-goto-task (section task)
  "Jump to TASK in SECTION."
  (let ((buffer (get-buffer "*Task Manager*")))
    (when buffer
      (switch-to-buffer buffer)
      ;; Make sure the section is expanded
      (puthash section t task-manager-expanded-sections)
      (task-manager-refresh)
      
      ;; Find and position at the task
      (goto-char (point-min))
      (search-forward section nil t)
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (forward-line)
          (when (and (not (eolp))
                     (string-match-p (regexp-quote task) (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (setq found t)
            (beginning-of-line)
            (recenter)))
        (when found
          (message "Found task in %s" section))))))

(defun task-manager-set-recurring ()
  "Set a task to be recurring. If cursor is on a task, use that task.
Selecting 'None' will clear recurring status."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to set as recurring: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         ;; Add None as first option to allow clearing recurring status
         (frequency (completing-read "Frequency: " 
                                    '("None" "daily" "weekly" "monthly")))
         ;; Remove any existing recurring tag
         (task-without-recurring (replace-regexp-in-string "\\[Recurring: [^]]*\\]" "" task))
         (task-without-recurring (string-trim task-without-recurring))
         (new-task (if (string= frequency "None")
                       task-without-recurring
                     (format "%s [Recurring: %s]" task-without-recurring frequency))))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one that includes recurring info
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (if (string= frequency "None")
          (message "Recurring status cleared.")
        (message "Task set as recurring %s." frequency)))))

(defun task-manager-set-priority ()
  "Set priority for a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to set priority: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (priority (completing-read "Set priority (high, medium, low): " 
                                   '("high" "medium" "low")))
         ;; Remove any existing priority tag
         (task-without-priority (replace-regexp-in-string "\\[Priority: [^]]*\\]" "" task))
         (task-without-priority (string-trim task-without-priority))
         (new-task (format "%s [Priority: %s]" task-without-priority priority)))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one that includes priority info
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (message "Priority for task set to %s." priority))))

(defun task-manager-set-due-date ()
  "Set or clear a due date for a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to set due date: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (has-due-date (and task (string-match "\\[Due: \\([^]]+\\)\\]" task)))
         (options '("Set date" "Clear date"))
         (action (completing-read "Action: " options nil t))
         (new-task nil))
    
    (if (string= action "Clear date")
        (progn
          ;; Remove any existing due date tag
          (setq new-task (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
          (setq new-task (string-trim new-task)))
      ;; Set a new due date
      (let* ((date (calendar-read-date))
             (due-date (format "%04d-%02d-%02d" 
                             (nth 2 date)   ; year
                             (nth 0 date)   ; month
                             (nth 1 date)))
             ;; Remove any existing due date tag
             (task-without-due (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
             (task-without-due (string-trim task-without-due)))
        (setq new-task (format "%s [Due: %s]" task-without-due due-date))))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (if (string= action "Clear date")
          (message "Due date cleared.")
        (message "Due date set.")))))

(defun task-manager-clear-due-date ()
  "Clear the due date from a task at point."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to clear due date: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         ;; Remove any existing due date tag
         (new-task (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
         (new-task (string-trim new-task)))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (message "Due date cleared."))))

(defun task-manager-get-all-tags ()
  "Extract all unique tags from all tasks."
  (let ((all-tags '()))
    (dolist (section task-manager-sections)
      (dolist (task (gethash section task-manager-tasks))
        (when (string-match "\\[Tags: \\([^]]+\\)\\]" task)
          (let* ((tags-string (match-string 1 task))
                 (tags-list (split-string tags-string "," t "\\s-*")))
            (dolist (tag tags-list)
              (unless (member tag all-tags)
                (push tag all-tags)))))))
    all-tags))

(defun task-manager-add-tags ()
  "Add tags to a task. If cursor is on a task, use that task.
Shows a list of existing tags for selection and offers an option to delete all tags."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to tag: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (existing-tags (and (string-match "\\[Tags: \\([^]]+\\)\\]" task)
                            (match-string 1 task)))
         (all-tags (task-manager-get-all-tags))
         (all-tags-with-options (append '("Delete all tags" "Enter new tags") all-tags))
         (choice (completing-read 
                  (if existing-tags
                      (format "Choose option or tag (current tags: %s): " existing-tags)
                    "Choose option or tag: ")
                  all-tags-with-options))
         (tags "")
         (task-without-tags (replace-regexp-in-string "\\[Tags: [^]]*\\]" "" task))
         (task-without-tags (string-trim task-without-tags))
         (new-task ""))
    
    (cond
     ;; Delete all tags
     ((string= choice "Delete all tags")
      (setq new-task task-without-tags)
      (message "All tags removed."))
     
     ;; Enter new tags manually
     ((string= choice "Enter new tags")
      (setq tags (read-string (if existing-tags
                                 (format "Enter new tags (current: %s): " existing-tags)
                               "Enter new tags (comma separated): ")))
      (if (string-empty-p tags)
          (setq new-task task-without-tags)
        (setq new-task (format "%s [Tags: %s]" task-without-tags tags))))
     
     ;; Selected an existing tag
     (t
      (setq tags (if existing-tags
                     (if (string-match-p (regexp-quote choice) existing-tags)
                         ;; Remove tag if it already exists
                         (let ((tag-list (split-string existing-tags "," t "\\s-*")))
                           (string-join (remove choice tag-list) ", "))
                       ;; Add tag if it doesn't exist
                       (concat existing-tags ", " choice))
                   choice))
      (if (string-empty-p tags)
          (setq new-task task-without-tags)
        (setq new-task (format "%s [Tags: %s]" task-without-tags tags)))))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                               (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one that includes tags info
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (message "Tags updated."))))

(defun task-manager-find-and-position-on-task (task section)
  "Find TASK in SECTION and position cursor at beginning of line."
  (goto-char (point-min))
  (let ((found nil))
    ;; First find the section
    (while (and (not found) (< (point) (point-max)))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (when (string-match-p (concat "^" (regexp-quote section) " (") line)
          (setq found t)
          (forward-line 1))
        (unless found
          (forward-line 1))))
    
    ;; Now find the task within the section
    (when found
      (setq found nil)
      (while (and (not found) (< (point) (point-max)))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (if (and (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)" line)
                   (string-match-p (regexp-quote task) line))
              (setq found t)
            (forward-line 1))))
      (when found
        (beginning-of-line)))))

(defun task-manager-setting-reminders ()
  "Set reminders for tasks. If cursor is on a task, use that task."
  (interactive)
  (let* ((current-pos (point))
         (current-line (line-number-at-pos))
         (current-column (current-column))
         (task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to set reminder: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (existing-reminder (and (string-match "\\[Reminder: \\([^]]+\\)\\]" task)
                               (match-string 1 task)))
         (reminder-prompt (if existing-reminder
                             (format "Enter reminder time (currently: %s): " existing-reminder)
                           "Enter reminder time (YYYY-MM-DD HH:MM): "))
         (reminder-time (read-string reminder-prompt))
         ;; Remove any existing reminder tag
         (task-without-reminder (replace-regexp-in-string "\\[Reminder: [^]]*\\]" "" task))
         (task-without-reminder (string-trim task-without-reminder))
         (new-task (format "%s [Reminder: %s]" task-without-reminder reminder-time)))
    
    ;; Remember if we're on this task for cursor positioning
    (let ((on-updated-task (and task-at-point 
                              (string= task (cdr task-at-point)))))
      
      ;; Replace old task with new one that includes reminder info
      (when section
        (setf (gethash section task-manager-tasks)
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      (gethash section task-manager-tasks))))
      
      ;; Update selected tasks if needed
      (when (member task task-manager-selected-tasks)
        (setq task-manager-selected-tasks
              (mapcar (lambda (t)
                        (if (string= t task) new-task t))
                      task-manager-selected-tasks)))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (if on-updated-task
          (task-manager-find-and-position-on-task new-task section)
        (goto-char (point-min))
        (forward-line (1- current-line))
        (move-to-column current-column))
      
      (message "Reminder for task set at %s." reminder-time))))

(defun task-manager-filter-tasks ()
  "Filter tasks by priority, due date, or tags."
  (interactive)
  (let* ((filter-type (completing-read "Filter by: " '("priority" "due date" "tags")))
         (filter-value (cond
                        ;; Use calendar for due date selection
                        ((string= filter-type "due date")
                         (let ((date (calendar-read-date)))
                           (format "%04d-%02d-%02d" 
                                   (nth 2 date)   ; year
                                   (nth 0 date)   ; month
                                   (nth 1 date)))) ; day
                        ;; Use regular read-string for other types
                        (t (read-string (format "Enter %s to filter by: " filter-type)))))
         (results-buffer (get-buffer-create "*Task Filter Results*")))
    (with-current-buffer results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Tasks filtered by %s = \"%s\":\n\n" filter-type filter-value))
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks))
                (found nil))
            (dolist (task tasks)
              (let ((pattern (format "\\[%s: [^]]*%s[^]]*\\]" 
                                     (cond
                                      ((string= filter-type "priority") "Priority")
                                      ((string= filter-type "due date") "Due")
                                      ((string= filter-type "tags") "Tags"))
                                     (regexp-quote filter-value))))
                (when (string-match-p pattern task)
                  (setq found t)
                  (let ((display-task (replace-regexp-in-string "^TODAY " "" task)))
                    ;; Insert clickable task result
                    (insert "  ")
                    (insert-text-button
                     (format "%s: %s" section display-task)
                     'action (lambda (_)
                              (task-manager-goto-task section task))
                     'follow-link t
                     'help-echo "Click to go to this task")
                    (insert "\n")))))
            (when found
              (insert "\n"))))
        (goto-char (point-min)))
      (special-mode)
      ;; Add key to go back to task manager
      (local-set-key (kbd "q") 'kill-this-buffer))
    (switch-to-buffer results-buffer)))

(defun task-manager-bulk-edit ()
  "Perform bulk edits on selected tasks."
  (interactive)
  (when task-manager-selected-tasks
    (let* ((current-pos (point))
           (current-line (line-number-at-pos))
           (current-column (current-column))
           (action (completing-read "Bulk action: " 
                                    '("add tag" "set priority" "set due date" 
                                      "set reminder" "mark as recurring")))
           (value (read-string (format "Enter %s value: " action)))
           (updated-tasks nil))
      (dolist (task task-manager-selected-tasks)
        (let ((new-task (format "%s [%s: %s]" task
                                (cond
                                 ((string= action "add tag") "Tags")
                                 ((string= action "set priority") "Priority")
                                 ((string= action "set due date") "Due")
                                 ((string= action "set reminder") "Reminder")
                                 ((string= action "mark as recurring") "Recurring"))
                                value)))
          (push (cons task new-task) updated-tasks)
          ;; Replace old task with new one that includes the new info
          (dolist (section task-manager-sections)
            (let ((tasks (gethash section task-manager-tasks)))
              (when (member task tasks)
                (setf (gethash section task-manager-tasks)
                      (mapcar (lambda (t)
                                (if (string= t task) new-task t))
                              tasks)))))))
      
      ;; Update selected tasks list with new task text
      (setq task-manager-selected-tasks
            (mapcar (lambda (task)
                      (let ((updated (assoc task updated-tasks)))
                        (if updated (cdr updated) task)))
                    task-manager-selected-tasks))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Try to restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (move-to-column current-column)
      
      (message "Bulk edited %d tasks." (length task-manager-selected-tasks)))))

(defun task-manager-export ()
  "Export tasks to a file."
  (interactive)
  (let* ((format (completing-read "Export format: " '("org" "json" "csv")))
         (file (read-file-name (format "Export to %s file: " format))))
    (with-temp-buffer
      (cond
       ((string= format "org")
        (insert "#+TITLE: Task Manager Export\n\n")
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (insert (format "* %s\n" section))
            (dolist (task tasks)
              (insert (format "** %s\n" task))))))
       
       ((string= format "json")
        (let ((json-object '()))
          (dolist (section task-manager-sections)
            (let ((tasks (gethash section task-manager-tasks)))
              (push (cons section tasks) json-object)))
          (insert (json-encode json-object))))
       
       ((string= format "csv")
        (insert "Section,Task\n")
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (dolist (task tasks)
              (insert (format "%s,%s\n" 
                              (replace-regexp-in-string "," "\\," section)
                              (replace-regexp-in-string "," "\\," task))))))))
      
      (write-region (point-min) (point-max) file))
    (message "Tasks exported to %s" file)))

(defun task-manager-import ()
  "Import tasks from a file."
  (interactive)
  (let* ((format (completing-read "Import format: " '("org" "json" "csv" "plain")))
         (file (read-file-name (format "Import from %s file: " format))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (cond
         ;; Import org file content as plain tasks to Inbox
         ((string= format "org")
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                         (line-beginning-position) (line-end-position))))
              ;; Skip empty lines and org properties
              (when (and (not (string-empty-p line))
                         (not (string-prefix-p "#+" line))
                         (not (string-prefix-p ":" line)))
                ;; Remove org markers like *, **, etc.
                (setq line (replace-regexp-in-string "^\\*+ " "" line))
                ;; Add each line as a task to Inbox
                (push line (gethash "Inbox" task-manager-tasks))))
            (forward-line 1)))
         
         ((string= format "json")
          (let ((json-object (json-read)))
            (dolist (section-pair json-object)
              (let ((section (car section-pair))
                    (tasks (cdr section-pair)))
                (dolist (task tasks)
                  (push task (gethash section task-manager-tasks)))))))
         
         ((string= format "csv")
          (goto-char (point-min))
          (forward-line 1) ;; Skip header
          (while (not (eobp))
            (when (looking-at "\\([^,]+\\),\\(.+\\)")
              (let ((section (match-string 1))
                    (task (match-string 2)))
                (push task (gethash section task-manager-tasks))))
            (forward-line 1)))
         
         ;; Plain text - each line is a task in Inbox
         ((string= format "plain")
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                         (line-beginning-position) (line-end-position))))
              (unless (string-empty-p line)
                (push line (gethash "Inbox" task-manager-tasks))))
            (forward-line 1)))))
      
      (task-manager-save-tasks)
      (task-manager-refresh)
      (message "Tasks imported from %s" file))))

(defun task-manager-focus-inbox ()
  "Focus on the Inbox section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Inbox is expanded
  (puthash "Inbox" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Inbox")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Inbox section and first task
  (goto-char (point-min))
  (search-forward "Inbox" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Inbox section"))

(defun task-manager-focus-today ()
  "Focus on the Today section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Today is expanded
  (puthash "Today" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Today")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Today section and first task
  (goto-char (point-min))
  (search-forward "Today" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Today section"))

(defun task-manager-focus-someday ()
  "Focus on the Someday section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Someday is expanded
  (puthash "Someday" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Someday")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Someday section and first task
  (goto-char (point-min))
  (search-forward "Someday" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Someday section"))

(defun task-manager-focus-week ()
  "Focus on the Week section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Week is expanded
  (puthash "Week" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Week")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Week section and first task
  (goto-char (point-min))
  (search-forward "Week" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Week section"))

(defun task-manager-focus-monday ()
  "Focus on the Monday section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Monday is expanded
  (puthash "Monday" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Monday")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Monday section and first task
  (goto-char (point-min))
  (search-forward "Monday" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Monday section"))

(defun task-manager-focus-calendar ()
  "Focus on the Calendar section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Calendar is expanded
  (puthash "Calendar" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Calendar")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Calendar section and first task
  (goto-char (point-min))
  (search-forward "Calendar" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Calendar section"))

(defun task-manager-focus-archive ()
  "Focus on the Archive section, expanding it if it's collapsed."
  (interactive)
  ;; Make sure Archive is expanded
  (puthash "Archive" t task-manager-expanded-sections)
  
  ;; Collapse all other sections for clarity
  (dolist (section task-manager-sections)
    (unless (string= section "Archive")
      (puthash section nil task-manager-expanded-sections)))
  
  (task-manager-refresh)
  
  ;; Navigate to the Archive section and first task
  (goto-char (point-min))
  (search-forward "Archive" nil t)
  (forward-line 1)  ; Move to the first task line
  (message "Focused on Archive section"))

(defun task-manager-view-recurring ()
  "View all recurring tasks across all sections."
  (interactive)
  (let ((results-buffer (get-buffer-create "*Recurring Tasks*")))
    (with-current-buffer results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "All Recurring Tasks:\n\n")
        (let ((found-any nil))
          (dolist (section task-manager-sections)
            (let ((tasks (gethash section task-manager-tasks))
                  (found nil))
              (dolist (task tasks)
                (when (string-match-p "\\[Recurring:" task)
                  (setq found t)
                  (setq found-any t)
                  (let ((display-task (replace-regexp-in-string "^TODAY " "" task))
                        (frequency (if (string-match "\\[Recurring: \\([^]]+\\)\\]" task)
                                      (match-string 1 task)
                                    "unknown")))
                    ;; Insert clickable task
                    (insert "  ")
                    (insert-text-button
                     (format "%s: %s" section display-task)
                     'action (lambda (_)
                              (task-manager-goto-task section task))
                     'follow-link t
                     'help-echo "Click to go to this task")
                    (insert "\n"))))
              (when found
                (insert "\n"))))
          
          (unless found-any
            (insert "  No recurring tasks found.\n")))
        
        (goto-char (point-min)))
      (special-mode)
      ;; Add key to go back to task manager
      (local-set-key (kbd "q") 'kill-this-buffer))
    (switch-to-buffer results-buffer)
    (message "Viewing all recurring tasks.")))

(defun task-manager-edit-task-at-point ()
  "Edit the task at point."
  (interactive)
  (let ((task-found nil)
        (current-line (line-number-at-pos))
        (task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    
    ;; Check if we're on a task line
    (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*\\)$" task-line)
      (let ((task-text (match-string 2 task-line))
            (section nil)
            (original-task nil))
        
        ;; Find the section and original task
        (dolist (sec task-manager-sections)
          (let ((tasks (gethash sec task-manager-tasks)))
            (dolist (full-task tasks)
              (when (string-match-p (regexp-quote task-text) full-task)
                (setq section sec)
                (setq original-task full-task)
                (setq task-found t)))))
        
        ;; Also check for "Due Today" tasks with section info in parenthesis
        (unless task-found
          (when (string-match "\\[\\([X ]\\)\\] \\(.*\\) (in \\(.*\\))" task-line)
            (setq task-text (match-string 2 task-line))
            (setq section (match-string 3 task-line))
            
            (let ((tasks (gethash section task-manager-tasks)))
              (dolist (full-task tasks)
                (when (string-match-p (regexp-quote task-text) full-task)
                  (setq original-task full-task)
                  (setq task-found t))))))
        
        (when task-found
          ;; Extract metadata from original task for preserving
          (let ((due-date (and (string-match "\\[Due: \\([^]]+\\)\\]" original-task)
                              (match-string 1 original-task)))
                (priority (and (string-match "\\[Priority: \\([^]]+\\)\\]" original-task)
                              (match-string 1 original-task)))
                (recurring (and (string-match "\\[Recurring: \\([^]]+\\)\\]" original-task)
                               (match-string 1 original-task)))
                (tags (and (string-match "\\[Tags: \\([^]]+\\)\\]" original-task)
                          (match-string 1 original-task)))
                (reminder (and (string-match "\\[Reminder: \\([^]]+\\)\\]" original-task)
                              (match-string 1 original-task))))
            
            ;; Get the base task text without any metadata
            (let* ((base-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" original-task))
                   (base-task (string-trim base-task)))
              
              ;; Prompt for new task text
              (let ((new-base-task (read-string "Edit task: " base-task)))
                
                ;; Reconstruct the task with its metadata
                (let ((new-task new-base-task))
                  (when due-date
                    (setq new-task (format "%s [Due: %s]" new-task due-date)))
                  (when priority
                    (setq new-task (format "%s [Priority: %s]" new-task priority)))
                  (when recurring
                    (setq new-task (format "%s [Recurring: %s]" new-task recurring)))
                  (when tags
                    (setq new-task (format "%s [Tags: %s]" new-task tags)))
                  (when reminder
                    (setq new-task (format "%s [Reminder: %s]" new-task reminder)))
                  
                  ;; Replace the old task with the new one
                  (setf (gethash section task-manager-tasks)
                        (mapcar (lambda (t)
                                  (if (string= t original-task) new-task t))
                                (gethash section task-manager-tasks)))
                  
                  ;; Also update in selected tasks if it was selected
                  (when (member original-task task-manager-selected-tasks)
                    (setq task-manager-selected-tasks
                          (mapcar (lambda (t)
                                    (if (string= t original-task) new-task t))
                                  task-manager-selected-tasks)))
                  
                  (task-manager-save-tasks)
                  (task-manager-refresh)
                  
                  ;; Try to position at the same task line
                  (goto-char (point-min))
                  (forward-line (1- current-line))
                  (beginning-of-line))))))))))

(defun task-manager-previous-task ()
  "Move to the previous task in the buffer."
  (interactive)
  (let ((current-pos (point))
        (found nil))
    ;; Move up one line to start searching from the line above current position
    (forward-line -1)
    ;; Search backward for a task line (which will have a checkbox)
    (while (and (not found) (not (bobp)))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (if (string-match-p "^\\s-*\\[\\([X ]\\)\\]" line)
            (setq found t)
          (forward-line -1))))
    
    ;; If no task found above, go back to original position
    (unless found
      (goto-char current-pos)
      (message "No previous task found"))
    
    ;; If found, position cursor at beginning of line
    (when found
      (beginning-of-line))))

(defun task-manager-move-to-week ()
  "Move all tasks from Inbox and Today sections to Week section."
  (interactive)
  (let ((inbox-tasks (gethash "Inbox" task-manager-tasks))
        (today-tasks (gethash "Today" task-manager-tasks))
        (week-tasks (gethash "Week" task-manager-tasks))
        (count 0))
    
    ;; Add all Inbox tasks to Week
    (dolist (task inbox-tasks)
      (push task week-tasks)
      (setq count (1+ count)))
    
    ;; Add all Today tasks to Week
    (dolist (task today-tasks)
      (push task week-tasks)
      (setq count (1+ count)))
    
    ;; Update the section hash tables
    (puthash "Week" week-tasks task-manager-tasks)
    (puthash "Inbox" nil task-manager-tasks)
    (puthash "Today" nil task-manager-tasks)
    
    ;; Remove any of these tasks from selection if they were selected
    (setq task-manager-selected-tasks 
          (seq-filter (lambda (task)
                        (not (or (member task inbox-tasks)
                                (member task today-tasks))))
                      task-manager-selected-tasks))
    
    (task-manager-save-tasks)
    (task-manager-refresh)
    
    ;; Focus on Week section
    (puthash "Week" t task-manager-expanded-sections)
    (goto-char (point-min))
    (search-forward "Week" nil t)
    (forward-line 1)
    
    (message "Moved %d tasks from Inbox and Today to Week." count)))

;; Function to manually create a backup
(defun task-manager-manual-backup ()
  "Manually create a backup of the tasks.org file."
  (interactive)
  (task-manager-create-backup)
  (message "Manual backup created."))

;; Setup a timer for periodic backups
(defvar task-manager-backup-timer nil
  "Timer object for automatic backups.")

(defun task-manager-start-backup-timer ()
  "Start the backup timer."
  (when task-manager-backup-timer
    (cancel-timer task-manager-backup-timer))
  (setq task-manager-backup-timer
        (run-with-timer 
         task-manager-backup-interval 
         task-manager-backup-interval 
         'task-manager-backup-if-needed)))

;; Start the backup timer when the package is loaded
(eval-after-load 'task-manager2
  '(task-manager-start-backup-timer))

;; Add key binding for manual backup
(define-key task-manager-mode-map (kbd "B") 'task-manager-manual-backup)

(defun task-manager-next-task ()
  "Move to the next task in the buffer."
  (interactive)
  (let ((current-pos (point))
        (found nil))
    ;; Move down one line to start searching from the line below current position
    (forward-line 1)
    ;; Search forward for a task line (which will have a checkbox)
    (while (and (not found) (not (eobp)))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (if (string-match-p "^\\s-*\\[\\([X ]\\)\\]" line)
            (setq found t)
          (forward-line 1))))
    
    ;; If no task found below, go back to original position
    (unless found
      (goto-char current-pos)
      (message "No next task found"))
    
    ;; If found, position cursor at beginning of line
    (when found
      (beginning-of-line))))

;; Add the missing toggle section function
(defun task-manager-toggle-section-at-point ()
  "Toggle the expansion state of the section at point."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (dolist (section task-manager-sections)
      (when (string-match-p (concat "^" (regexp-quote section) " (") line)
        (let ((current-state (gethash section task-manager-expanded-sections)))
          (puthash section (not current-state) task-manager-expanded-sections)
          (message "%s section %s" 
                   section 
                   (if (not current-state) "expanded" "collapsed"))
          (task-manager-refresh))))))

;; Add this debugging function before the provide statement
(defun task-manager-debug-tasks ()
  "Display the contents of the task-manager-tasks hash table for debugging."
  (interactive)
  (let ((debug-buffer (get-buffer-create "*Task Manager Debug*")))
    (with-current-buffer debug-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Task Manager Data Structure Contents:\n\n")
        
        ;; Show hash table stats
        (insert (format "Hash table stats: %d sections, %s\n\n" 
                       (length task-manager-sections)
                       (if (hash-table-p task-manager-tasks)
                           (format "%d entries, test: %s" 
                                  (hash-table-count task-manager-tasks)
                                  (hash-table-test task-manager-tasks))
                         "NOT A VALID HASH TABLE")))
        
        ;; Show expanded sections state
        (insert "Section expansion state:\n")
        (dolist (section task-manager-sections)
          (insert (format "  %s: %s\n" section 
                         (if (gethash section task-manager-expanded-sections)
                             "expanded" "collapsed"))))
        (insert "\n")
        
        ;; Show tasks by section
        (insert "Tasks by section:\n")
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (insert (format "  %s (%d tasks):\n" section (length tasks)))
            (if tasks
                (dolist (task tasks)
                  (insert (format "    - %s\n" task)))
              (insert "    (empty)\n"))
            (insert "\n"))))
      
      ;; Set up the buffer for viewing
      (special-mode)
      (local-set-key (kbd "q") 'kill-this-buffer)
      (local-set-key (kbd "g") 
                    (lambda () 
                      (interactive)
                      (task-manager-debug-tasks))))
    
    ;; Show the debug buffer
    (switch-to-buffer debug-buffer)
    (message "Showing task manager internal data structure for debugging")))

;; Add key binding for debug function
(define-key task-manager-mode-map (kbd "M-d") 'task-manager-debug-tasks)

(provide 'task-manager2)

;;; task-manager2.el ends here

