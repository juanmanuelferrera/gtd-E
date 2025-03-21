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
  (let* ((task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to clear recurring: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         ;; Remove any existing recurring tag
         (new-task (replace-regexp-in-string "\\[Recurring: [^]]*\\]" "" task))
         (new-task (string-trim new-task)))
    
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
    (message "Recurring status cleared.")))

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

(defun task-manager-toggle-commands ()
  "Toggle the visibility of the commands section."
  (interactive)
  (setq task-manager-show-commands (not task-manager-show-commands))
  (task-manager-refresh)
  (message "Commands %s" (if task-manager-show-commands "shown" "hidden")))

(defun task-manager-linkify-text (text)
  "Make URLs and file paths in TEXT clickable."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Detect and linkify URLs - use a more robust pattern
    (while (re-search-forward "\\b\\(https?://\\|www\\.\\)[^ \t\n\r,;()\"']+" nil t)
      (let* ((match (match-string 0))
             (url (if (string-match-p "^www\\." match)
                     (concat "http://" match)
                   match))
             (bounds (match-data)))
        (set-text-properties (car bounds) (cadr bounds)
                             `(face link
                               mouse-face highlight
                               help-echo ,(format "mouse-1: visit %s" url)
                               keymap ,button-map
                               follow-link t
                               action (lambda (_) (browse-url ,url))))))
    
    ;; Detect and linkify file paths - make pattern more selective
    (goto-char (point-min))
    (while (re-search-forward "\\b\\(file://\\|~/\\|/\\)[^ \t\n\r,;()\"']+" nil t)
      (let* ((match (match-string 0))
             (file-path (if (string-match-p "^file:" match)
                           (substring match 5)
                         match))
             (bounds (match-data)))
        (set-text-properties (car bounds) (cadr bounds)
                             `(face link
                               mouse-face highlight
                               help-echo ,(format "mouse-1: open %s" file-path)
                               keymap ,button-map
                               follow-link t
                               action (lambda (_) (find-file ,file-path))))))
    
    (buffer-string)))

(defun task-manager-refresh ()
  "Refresh the display of tasks in the task manager."
  (let ((inhibit-read-only t)
        (today (format-time-string "%Y-%m-%d")))
    (erase-buffer)
    (insert "GTD + E - Task Manager\n")
    (insert "====================\n\n")
    
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
                ;; Make links clickable in display-task
                (insert (format "  [%s] " (if selected "X" " ")))
                (insert (task-manager-linkify-text display-task))
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
              ;; Make links clickable in display-task
              (insert (format "  [%s] " (if selected "X" " ")))
              (insert (task-manager-linkify-text display-task))
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
          (insert "  R: Clear recurring status\n")
          (insert "  V: View all recurring tasks\n")
          (insert "  F: Focus on Archive section\n")
          (insert "  p: Move to previous task\n")
          (insert "  d: Set due date\n")
          (insert "  D: Clear due date\n")
          (insert "  T: Add tags\n")
          (insert "  t: Focus on Today section\n")
          (insert "  w: Focus on Week section\n")
          (insert "  W: Move all tasks from Inbox and Today to Week\n")
          (insert "  o: Focus on Monday section\n")
          (insert "  c: Focus on Calendar section\n")
          (insert "  C: Toggle commands visibility\n")
          (insert "  f: Filter tasks\n")
          (insert "  X: Set reminders\n")
          (insert "  b: Bulk edit tasks\n")
          (insert "  x: Export tasks\n")
          (insert "  i: Focus on Inbox section\n")
          (insert "  I: Import tasks\n")
          (insert "  v: Save tasks\n")
          (insert "  L: Load tasks\n")
          (insert "  u: Undo (up to 15 operations)\n"))
      ;; Show reminder when commands are hidden
      (insert "\nC to show commands\n"))
    
    (goto-char (point-min))))

;; Task Functions
(defun task-manager-add-task (section)
  "Add a task to SECTION."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (let ((task (read-string "Enter new task: ")))
    (unless (string-empty-p task)
      (push task (gethash section task-manager-tasks))
      (task-manager-save-tasks)
      (task-manager-refresh))))

(defun task-manager-add-multiple-tasks (section)
  "Add multiple tasks to SECTION."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (let ((tasks (split-string
                (read-string "Enter tasks (one per line): ")
                "\n" t)))
    (dolist (task tasks)
      (unless (string-empty-p task)
        (push task (gethash section task-manager-tasks))))
    (task-manager-save-tasks)
    (task-manager-refresh)))

(defun task-manager-delete-tasks ()
  "Delete selected tasks permanently."
  (interactive)
  (when (and task-manager-selected-tasks
             (y-or-n-p "Permanently delete selected tasks?"))
    (dolist (task task-manager-selected-tasks)
      ;; Find and remove the task from its current section
      (dolist (section task-manager-sections)
        (let ((tasks (gethash section task-manager-tasks)))
          (when (member task tasks)
            ;; Remove from current section
            (setf (gethash section task-manager-tasks)
                  (remove task tasks))))))
    ;; Clear selected tasks
    (setq task-manager-selected-tasks nil)
    (task-manager-save-tasks)
    (task-manager-refresh)
    (message "Tasks permanently deleted.")))

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
  (let ((target (completing-read "Move to section: " task-manager-sections)))
    (dolist (task task-manager-selected-tasks)
      (dolist (section task-manager-sections)
        (let ((tasks (gethash section task-manager-tasks)))
          (when (member task tasks)
            (setf (gethash section task-manager-tasks)
                  (remove task tasks))
            (push task (gethash target task-manager-tasks))))))
    (setq task-manager-selected-tasks nil)
    (task-manager-save-tasks)
    (task-manager-refresh)))

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
  "Toggle selection of task at point and move to next task if possible."
  (interactive)
  (let ((task-found nil)
        (current-line (line-number-at-pos))
        (task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    ;; Check if we're on a task line (which may now contain text properties for links)
    (when (string-match "^\\s-*\\[\\([X ]\\)\\] \\(.*?\\)\\(?: (in \\(.*\\))?\\)?$" task-line)
      (let ((raw-task (match-string 2 task-line))
            (is-selected (string= (match-string 1 task-line) "X"))
            (section-info (match-string 3 task-line))) ; for "Due Today" section
        
        ;; Find the original task with metadata
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (dolist (full-task tasks)
              ;; Remove metadata for plain text comparison
              (let* ((stripped-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" full-task))
                     (stripped-task (string-trim stripped-task)))
                (when (or (string-match-p (regexp-quote raw-task) stripped-task)
                          (string-match-p (regexp-quote stripped-task) raw-task))
                  (setq task-found full-task)
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
                  (setq task-found t))))))
        
        ;; Refresh and position cursor
        (when task-found
          (task-manager-save-tasks)
          (task-manager-refresh)
          
          ;; Try to position at the next task (current line + 1)
          (goto-char (point-min))
          (forward-line (1- current-line)) ;; Get back to our line
          
          ;; Check if there's a next task line (simple check for "[" on next line)
          (forward-line 1)
          (let ((next-line (buffer-substring-no-properties 
                            (line-beginning-position) 
                            (min (+ (line-beginning-position) 5) (point-max)))))
            (unless (string-match-p "\\[" next-line)
              ;; No next task, go back to current task
              (forward-line -1)))
          
          (beginning-of-line))))
    
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

(defun task-manager-save-tasks ()
  "Save tasks to the save file, preserving beorg formatting."
  (interactive)
  ;; Only push to history when saving as a result of a change, not on startup
  (when (called-interactively-p 'any)
    (task-manager-push-to-undo-history))
  
  ;; Read existing file to preserve structure and format
  (let ((existing-content "")
        (section-map '(("Inbox" . "Inbox")
                       ("Today" . "Next Actions")
                       ("Week" . "Projects")
                       ("Monday" . "Monday")
                       ("Calendar" . "Calendar")
                       ("Someday" . "Someday")
                       ("Archive" . "Archive"))))
    
    ;; Try to read existing content to preserve formatting
    (when (file-exists-p task-manager-save-file)
      (setq existing-content (with-temp-buffer
                               (insert-file-contents task-manager-save-file)
                               (buffer-string))))
    
    ;; Create new content
    (with-temp-buffer
      ;; If there's no existing content, add standard header
      (if (string-empty-p existing-content)
          (insert "#+TITLE: beorg Tasks\n\n")
        ;; Otherwise preserve header until first heading
        (string-match "^\\* " existing-content)
        (let ((header (substring existing-content 0 (match-beginning 0))))
          (insert header)))
      
      ;; Write sections and tasks
      (dolist (section-pair section-map)
        (let* ((our-section (car section-pair))
               (beorg-section (cdr section-pair))
               (tasks (gethash our-section task-manager-tasks)))
          (when tasks
            (insert (format "* %s\n" beorg-section))
            (dolist (task (reverse tasks)) ;; reverse to maintain order
              (insert (format "** %s\n" task))))))
      
      ;; Write to file
      (write-region (point-min) (point-max) task-manager-save-file))
    
    (message "Tasks saved to %s" task-manager-save-file)))

(defun task-manager-load-tasks ()
  "Load tasks from the save file."
  (interactive)
  (when (file-exists-p task-manager-save-file)
    ;; Reset the tasks hash table
    (setq task-manager-tasks (make-hash-table :test 'equal))
    (dolist (section task-manager-sections)
      (puthash section '() task-manager-tasks))
    
    ;; Parse the file
    (with-temp-buffer
      (insert-file-contents task-manager-save-file)
      (org-mode)
      (goto-char (point-min))
      (let ((current-section nil))
        (while (not (eobp))
          (cond
           ;; Section header (level 1 heading)
           ((looking-at "^\\* \\(.+\\)")
            (let ((heading (match-string 1)))
              ;; Map beorg headings to our sections
              (cond
               ((string-match-p "Inbox" heading)
                (setq current-section "Inbox"))
               ((string-match-p "Next Actions" heading)
                (setq current-section "Today"))
               ((string-match-p "Projects" heading)
                (setq current-section "Week"))
               ((string-match-p "Someday" heading)
                (setq current-section "Someday"))
               ((string-match-p "Archive" heading)
                (setq current-section "Archive"))
               ((string-match-p "Monday" heading)
                (setq current-section "Monday"))
               ((string-match-p "Calendar" heading)
                (setq current-section "Calendar"))
               (t (setq current-section "Inbox")))))
           
           ;; Task entry (level 2 heading)
           ((and current-section (looking-at "^\\*\\* \\(.+\\)"))
            (let ((task (match-string 1)))
              (push task (gethash current-section task-manager-tasks))))
           
           ;; Task entry with deeper level (levels 3+)
           ((and current-section (looking-at "^\\*\\*\\* \\(.+\\)"))
            (let ((task (match-string 1)))
              (push task (gethash current-section task-manager-tasks)))))
          
          (forward-line 1))))
    
    (message "Tasks loaded from %s" task-manager-save-file)
    (when (called-interactively-p 'any)
      (task-manager-refresh))))

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
  (let* ((task-at-point (task-manager-get-task-at-point))
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
    (if (string= frequency "None")
        (message "Recurring status cleared.")
      (message "Task set as recurring %s." frequency))))

(defun task-manager-set-priority ()
  "Set priority for a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
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
    (message "Priority for task set to %s." priority)))

(defun task-manager-set-due-date ()
  "Set or clear a due date for a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
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
                             (nth 1 date))) ; day
             ;; Remove any existing due date tag
             (task-without-due (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
             (task-without-due (string-trim task-without-due)))
        (setq new-task (format "%s [Due: %s]" task-without-due due-date))))
    
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
    (if (string= action "Clear date")
        (message "Due date cleared.")
      (message "Due date set."))))

(defun task-manager-clear-due-date ()
  "Clear the due date from a task at point."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to clear due date: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         ;; Remove any existing due date tag
         (new-task (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
         (new-task (string-trim new-task)))
    
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
    (message "Due date cleared.")))

(defun task-manager-add-tags ()
  "Add tags to a task. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to tag: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (existing-tags (and (string-match "\\[Tags: \\([^]]+\\)\\]" task)
                            (match-string 1 task)))
         (tags-prompt (if existing-tags
                         (format "Enter tags (currently: %s): " existing-tags)
                       "Enter tags (comma separated): "))
         (tags (read-string tags-prompt))
         ;; Remove any existing tags
         (task-without-tags (replace-regexp-in-string "\\[Tags: [^]]*\\]" "" task))
         (task-without-tags (string-trim task-without-tags))
         (new-task (format "%s [Tags: %s]" task-without-tags tags)))
    
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
    (message "Tags for task set to %s." tags)))

(defun task-manager-setting-reminders ()
  "Set reminders for tasks. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
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
    (message "Reminder for task set at %s." reminder-time)))

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
    (let* ((action (completing-read "Bulk action: " 
                                    '("add tag" "set priority" "set due date" 
                                      "set reminder" "mark as recurring")))
           (value (read-string (format "Enter %s value: " action))))
      (dolist (task task-manager-selected-tasks)
        (let ((new-task (format "%s [%s: %s]" task
                                (cond
                                 ((string= action "add tag") "Tags")
                                 ((string= action "set priority") "Priority")
                                 ((string= action "set due date") "Due")
                                 ((string= action "set reminder") "Reminder")
                                 ((string= action "mark as recurring") "Recurring"))
                                value)))
          ;; Replace old task with new one that includes the new info
          (dolist (section task-manager-sections)
            (let ((tasks (gethash section task-manager-tasks)))
              (when (member task tasks)
                (setf (gethash section task-manager-tasks)
                      (mapcar (lambda (t)
                                (if (string= t task) new-task t))
                              tasks)))))))
      (task-manager-save-tasks)
      (task-manager-refresh)
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

(provide 'task-manager2)

;;; task-manager2.el ends here

