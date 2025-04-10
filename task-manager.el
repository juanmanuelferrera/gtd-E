;;; task-manager2.el --- Enhanced task manager -*- lexical-binding: t -*-

;; Author: Assistant
;; Keywords: task management, todo
;; Version: 2.0
;; Package-Requires: ((emacs "25.1") (org "9.3"))

;;; Commentary:
;; A comprehensive task manager with sections for Inbox, Today, Week, etc.
;; Added functionalities: search, recurring tasks, priorities, reminders, tags, and export/import.
;; 
Commands:
;;  a i: Add task to Inbox
;;  a t: Add task to Today
;;  a w: Add task to Week
;;  a m: Add task to Monday
;;  a s: Add task to Someday
;;  a c: Add task to Calendar
;;  A i: Add multiple tasks to Inbox
;;  A t: Add multiple tasks to Today
;;  A w: Add multiple tasks to Week
;;  A m: Add multiple tasks to Monday
;;  A s: Add multiple tasks to Someday
;;  A c: Add multiple tasks to Calendar
;;  b: Bulk edit tasks
;;  B: Create manual backup
;;  c: Focus on Calendar section
;;  C: Toggle commands visibility
;;  d: Set due date
;;  D: Clear due date
;;  f: Filter tasks
;;  g: Refresh buffer
;;  i: Focus on Inbox section
;;  I: Import tasks
;;  k: Delete task
;;  K: Delete all tasks in a section
;;  L: Load tasks
;;  m: Focus on Monday section
;;  n: Move to next task
;;  o i: Open Inbox section
;;  o t: Open Today section
;;  o w: Open Week section
;;  o m: Open Monday section
;;  o s: Open Someday section
;;  o c: Open Calendar section
;;  p: Move to previous task
;;  q: Quit buffer
;;  r: Set recurring task
;;  R: Clear recurring status
;;  s: Move task to Someday
;;  S: Search tasks
;;  t: Focus on Today section
;;  T: Add/delete tags
;;  u: Undo
;;  v: View all recurring tasks
;;  w: Move task to Week
;;  W: Move all Inbox and Today tasks to Week
;;  x: Export tasks
;;  X: Set reminders
;;  z: Collapse/expand all sections
;;  RET: Edit task
;;  SPC: Toggle task selection
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
  
  ;; Create prefix keymaps for 'a', 'A', and 'o'
  (let ((a-map (make-sparse-keymap))
        (A-map (make-sparse-keymap))
        (o-map (make-sparse-keymap)))
    
    ;; Define keys in the 'a' prefix map
    (define-key a-map (kbd "i") 'task-manager-add-task-inbox)
    (define-key a-map (kbd "t") 'task-manager-add-task-today)
    (define-key a-map (kbd "w") 'task-manager-add-task-week)
    (define-key a-map (kbd "m") 'task-manager-add-task-monday)
    (define-key a-map (kbd "s") 'task-manager-add-task-someday)
    (define-key a-map (kbd "c") 'task-manager-add-task-calendar)
    
    ;; Define keys in the 'A' prefix map
    (define-key A-map (kbd "i") 'task-manager-add-multiple-tasks-inbox)
    (define-key A-map (kbd "t") 'task-manager-add-multiple-tasks-today)
    (define-key A-map (kbd "w") 'task-manager-add-multiple-tasks-week)
    (define-key A-map (kbd "m") 'task-manager-add-multiple-tasks-monday)
    (define-key A-map (kbd "s") 'task-manager-add-multiple-tasks-someday)
    (define-key A-map (kbd "c") 'task-manager-add-multiple-tasks-calendar)
    
    ;; Define keys in the 'o' prefix map
    (define-key o-map (kbd "i") 'task-manager-focus-inbox)
    (define-key o-map (kbd "t") 'task-manager-focus-today)
    (define-key o-map (kbd "w") 'task-manager-focus-week)
    (define-key o-map (kbd "m") 'task-manager-focus-monday)
    (define-key o-map (kbd "s") 'task-manager-focus-someday)
    (define-key o-map (kbd "c") 'task-manager-focus-calendar)
    
    ;; Bind the prefix maps to 'a', 'A', and 'o'
    (define-key task-manager-mode-map (kbd "a") a-map)
    (define-key task-manager-mode-map (kbd "A") A-map)
    (define-key task-manager-mode-map (kbd "o") o-map))
  
  ;; Set up other key bindings
  (define-key task-manager-mode-map (kbd "RET") 'task-manager-edit-task-at-point)
  (define-key task-manager-mode-map (kbd "b") 'task-manager-bulk-edit)
  (define-key task-manager-mode-map (kbd "B") 'task-manager-manual-backup)
  (define-key task-manager-mode-map (kbd "c") 'task-manager-move-to-calendar)
  (define-key task-manager-mode-map (kbd "C") 'task-manager-toggle-commands)
  (define-key task-manager-mode-map (kbd "d") 'task-manager-set-due-date)
  (define-key task-manager-mode-map (kbd "D") 'task-manager-clear-due-date)
  (define-key task-manager-mode-map (kbd "f") 'task-manager-filter-tasks)
  (define-key task-manager-mode-map (kbd "g") 'task-manager-refresh)
  (define-key task-manager-mode-map (kbd "i") 'task-manager-move-to-inbox)
  (define-key task-manager-mode-map (kbd "I") 'task-manager-import)
  (define-key task-manager-mode-map (kbd "k") 'task-manager-delete-tasks)
  (define-key task-manager-mode-map (kbd "K") 'task-manager-delete-section-tasks)
  (define-key task-manager-mode-map (kbd "L") 'task-manager-load-tasks)
  (define-key task-manager-mode-map (kbd "m") 'task-manager-move-to-monday)
  (define-key task-manager-mode-map (kbd "n") 'task-manager-next-task)
  (define-key task-manager-mode-map (kbd "p") 'task-manager-previous-task)
  (define-key task-manager-mode-map (kbd "q") 'kill-this-buffer)
  (define-key task-manager-mode-map (kbd "r") 'task-manager-set-recurring)
  (define-key task-manager-mode-map (kbd "R") 'task-manager-clear-recurring)
  (define-key task-manager-mode-map (kbd "s") 'task-manager-move-to-someday)
  (define-key task-manager-mode-map (kbd "S") 'task-manager-search-tasks)
  (define-key task-manager-mode-map (kbd "t") 'task-manager-move-to-today)
  (define-key task-manager-mode-map (kbd "T") 'task-manager-add-tags)
  (define-key task-manager-mode-map (kbd "u") 'task-manager-undo)
  (define-key task-manager-mode-map (kbd "v") 'task-manager-view-recurring)
  (define-key task-manager-mode-map (kbd "w") 'task-manager-move-to-week)
  (define-key task-manager-mode-map (kbd "W") 'task-manager-move-all-to-week)
  (define-key task-manager-mode-map (kbd "x") 'task-manager-export)
  (define-key task-manager-mode-map (kbd "X") 'task-manager-setting-reminders)
  (define-key task-manager-mode-map (kbd "z") 'task-manager-toggle-all-sections)
  (define-key task-manager-mode-map (kbd "SPC") 'task-manager-toggle-task-at-point))

;; Add the undo keybinding explicitly after mode definition
(define-key task-manager-mode-map (kbd "u") 'task-manager-undo)

;; Add the clear due date keybinding explicitly after mode definition
(define-key task-manager-mode-map (kbd "D") 'task-manager-clear-due-date)

;; Add the next task keybinding
(define-key task-manager-mode-map (kbd "n") 'task-manager-next-task)

;; Add key binding for manual backup
(define-key task-manager-mode-map (kbd "B") 'task-manager-manual-backup)

;; Add new functions for section-specific task addition
(defun task-manager-add-task-inbox ()
  "Add a task to the Inbox section."
  (interactive)
  (task-manager-add-task "Inbox"))

(defun task-manager-add-task-today ()
  "Add a task to the Today section."
  (interactive)
  (task-manager-add-task "Today"))

(defun task-manager-add-task-week ()
  "Add a task to the Week section."
  (interactive)
  (task-manager-add-task "Week"))

(defun task-manager-add-task-monday ()
  "Add a task to the Monday section."
  (interactive)
  (task-manager-add-task "Monday"))

(defun task-manager-add-task-someday ()
  "Add a task to the Someday section."
  (interactive)
  (task-manager-add-task "Someday"))

(defun task-manager-add-task-calendar ()
  "Add a task to the Calendar section."
  (interactive)
  (task-manager-add-task "Calendar"))

(defun task-manager-add-multiple-tasks-inbox ()
  "Add multiple tasks to the Inbox section."
  (interactive)
  (task-manager-add-multiple-tasks "Inbox"))

(defun task-manager-add-multiple-tasks-today ()
  "Add multiple tasks to the Today section."
  (interactive)
  (task-manager-add-multiple-tasks "Today"))

(defun task-manager-add-multiple-tasks-week ()
  "Add multiple tasks to the Week section."
  (interactive)
  (task-manager-add-multiple-tasks "Week"))

(defun task-manager-add-multiple-tasks-monday ()
  "Add multiple tasks to the Monday section."
  (interactive)
  (task-manager-add-multiple-tasks "Monday"))

(defun task-manager-add-multiple-tasks-someday ()
  "Add multiple tasks to the Someday section."
  (interactive)
  (task-manager-add-multiple-tasks "Someday"))

(defun task-manager-add-multiple-tasks-calendar ()
  "Add multiple tasks to the Calendar section."
  (interactive)
  (task-manager-add-multiple-tasks "Calendar"))

(defun task-manager-move-to-someday ()
  "Move selected tasks to Someday section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Someday" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Someday." (length tasks-to-move)))))

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
  (let ((inhibit-read-only t)
        (today (format-time-string "%Y-%m-%d"))
        (current-date (format-time-string "%A, %d %B %Y"))
        (current-time (format-time-string "%H:%M")))
    (erase-buffer)
    (insert "GTD + Emacs\n")
    (insert "============\n")
    (insert (format "%s [%s]\n\n" current-date current-time))
    
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
          (insert "  a i: Add task to Inbox\n")
          (insert "  a t: Add task to Today\n")
          (insert "  a w: Add task to Week\n")
          (insert "  a m: Add task to Monday\n")
          (insert "  a s: Add task to Someday\n")
          (insert "  a c: Add task to Calendar\n")
          (insert "  A i: Add multiple tasks to Inbox\n")
          (insert "  A t: Add multiple tasks to Today\n")
          (insert "  A w: Add multiple tasks to Week\n")
          (insert "  A m: Add multiple tasks to Monday\n")
          (insert "  A s: Add multiple tasks to Someday\n")
          (insert "  A c: Add multiple tasks to Calendar\n")
          (insert "  b: Bulk edit tasks\n")
          (insert "  B: Create manual backup\n")
          (insert "  c: Focus on Calendar section\n")
          (insert "  C: Toggle commands visibility\n")
          (insert "  d: Set due date\n")
          (insert "  D: Clear due date\n")
          (insert "  f: Filter tasks\n")
          (insert "  g: Refresh buffer\n")
          (insert "  i: Focus on Inbox section\n")
          (insert "  I: Import tasks\n")
          (insert "  k: Delete task\n")
          (insert "  K: Delete all tasks in a section\n")
          (insert "  L: Load tasks\n")
          (insert "  m: Focus on Monday section\n")
          (insert "  n: Move to next task\n")
          (insert "  o i: Open Inbox section\n")
          (insert "  o t: Open Today section\n")
          (insert "  o w: Open Week section\n")
          (insert "  o m: Open Monday section\n")
          (insert "  o s: Open Someday section\n")
          (insert "  o c: Open Calendar section\n")
          (insert "  p: Move to previous task\n")
          (insert "  q: Quit buffer\n")
          (insert "  r: Set recurring task\n")
          (insert "  R: Clear recurring status\n")
          (insert "  s: Move task to Someday\n")
          (insert "  S: Search tasks\n")
          (insert "  t: Focus on Today section\n")
          (insert "  T: Add/delete tags\n")
          (insert "  u: Undo\n")
          (insert "  v: View all recurring tasks\n")
          (insert "  w: Move task to Week\n")
          (insert "  W: Move all Inbox and Today tasks to Week\n")
          (insert "  x: Export tasks\n")
          (insert "  X: Set reminders\n")
          (insert "  z: Collapse/expand all sections\n")
          (insert "  RET: Edit task\n")
          (insert "  SPC: Toggle task selection\n"))
      ;; Show reminder when commands are hidden
      (insert "\nC to show commands\n"))
    
    (goto-char (point-min))))

;; Task Functions
(defun task-manager-add-task (section)
  "Add a task to SECTION. If called with a section key (i, t, w, o, c, s), add directly to that section."
  (interactive
   (list (let ((key (read-char "Press section key (i,t,w,o,c,s): ")))
           (cond
            ((eq key ?i) "Inbox")
            ((eq key ?t) "Today")
            ((eq key ?w) "Week")
            ((eq key ?o) "Monday")
            ((eq key ?c) "Calendar")
            ((eq key ?s) "Someday")
            (t (message "Invalid section key. Use i,t,w,o,c,s")
               (keyboard-quit))))))
  (let ((task (if (string= section "Calendar")
                  ;; For Calendar section, first get the date
                  (let ((date (task-manager-select-date-with-calendar)))
                    (if date
                        (format "%s [Due: %s]" 
                                (read-string "Enter new task: ")
                                date)
                      (message "Date selection cancelled.")
                      (keyboard-quit)))
                ;; For other sections, just get the task
                (read-string "Enter new task: "))))
    (unless (string-empty-p task)
      (push task (gethash section task-manager-tasks))
      (task-manager-save-tasks)
      (task-manager-refresh))))

(defun task-manager-add-multiple-tasks (section)
  "Add multiple tasks to SECTION using a temporary buffer for multiline input."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (let ((temp-buffer (get-buffer-create "*Task Input*")))
    (with-current-buffer temp-buffer
      (erase-buffer)
      (insert ";; Enter your tasks below, one per line.\n")
      (insert ";; Press C-c C-c when done, or C-c C-k to cancel.\n\n")
      (text-mode)
      (local-set-key (kbd "C-c C-c") 'exit-recursive-edit)
      (local-set-key (kbd "C-c C-k") 'abort-recursive-edit))
    
    (switch-to-buffer temp-buffer)
    (recursive-edit)
    
    ;; Only process if we didn't abort
    (when (buffer-live-p temp-buffer)
      (let ((tasks (with-current-buffer temp-buffer
                    (split-string (buffer-substring-no-properties 
                                 (point-min) (point-max))
                                "\n" t))))
        ;; Filter out comment lines and empty lines
        (setq tasks (seq-filter (lambda (task)
                                 (and (not (string-empty-p task))
                                      (not (string-match-p "^;;" task))))
                               tasks))
        
        ;; Add tasks to the section
        (dolist (task tasks)
          (unless (string-empty-p task)
            (push task (gethash section task-manager-tasks))))
        
        (task-manager-save-tasks)
        (task-manager-refresh)
        (message "Added %d tasks to %s section." (length tasks) section)))
    
    (kill-buffer temp-buffer)))

(defun task-manager-delete-tasks ()
  "Move selected tasks to Archive section if they're not in Archive,
otherwise permanently delete them. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-process (if task-at-point
                              (list (cdr task-at-point))
                            task-manager-selected-tasks))
         (archive-tasks (gethash "Archive" task-manager-tasks))
         (tasks-to-archive nil)
         (tasks-to-delete nil))
    
    (when tasks-to-process
      ;; Save current state to undo history
      (task-manager-push-to-undo-history)
      
      ;; Separate tasks between those to archive and those to delete
      (dolist (task tasks-to-process)
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
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line))))

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
  "Move selected tasks to another section. If cursor is on a task, use that task.
If called with a section key (i, t, w, o, c, s), move directly to that section."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks))
         (target (let ((key (read-char "Press section key (i,t,w,o,c,s): ")))
                  (cond
                   ((eq key ?i) "Inbox")
                   ((eq key ?t) "Today")
                   ((eq key ?w) "Week")
                   ((eq key ?o) "Monday")
                   ((eq key ?c) "Calendar")
                   ((eq key ?s) "Someday")
                   (t (message "Invalid section key. Use i,t,w,o,c,s")
                      (keyboard-quit))))))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash target task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to %s." (length tasks-to-move) target))))

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

(defun task-manager-select-date-with-calendar ()
  "Use org's calendar to select a date."
  (let* ((org-read-date-prefer-future nil)
         (org-read-date-analyze-future-p nil)
         (org-read-date-completion-format 'iso)
         (org-read-date-force-compatible-dates t)
         (org-read-date-display-live t)
         (org-read-date-show-calendar t)
         (org-read-date-with-time nil)
         (org-read-date-allow-time nil)
         (org-read-date-inactive nil)
         (org-read-date-prompt "Select date: ")
         ;; Set default date to today
         (org-read-date-default-time (encode-time 0 0 0 (nth 1 (decode-time)) 
                                                (nth 2 (decode-time)) 
                                                (nth 5 (decode-time))))
         (date (org-read-date)))
    (if date
        (let ((parsed-date (parse-time-string date)))
          (format "%04d-%02d-%02d" 
                  (nth 5 parsed-date)   ; year
                  (nth 4 parsed-date)   ; month
                  (nth 3 parsed-date))) ; day
      nil)))

(defun task-manager-set-due-date ()
  "Set the due date for a task using org's calendar.
If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (task (if task-at-point
                  (cdr task-at-point)
                (completing-read "Select task to set due date: " (task-manager-all-tasks))))
         (section (if task-at-point
                     (car task-at-point)
                   (task-manager-find-task-section task)))
         (existing-date (and (string-match "\\[Due: \\([^]]+\\)\\]" task)
                           (match-string 1 task)))
         (date (task-manager-select-date-with-calendar))
         ;; Remove any existing due date tag
         (task-without-date (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
         (task-without-date (string-trim task-without-date))
         (new-task (if date
                      (format "%s [Due: %s]" task-without-date date)
                    task-without-date)))
    
    ;; Replace old task with new one that includes due date info
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
    (if date
        (message "Due date set to %s." date)
      (message "Due date setting cancelled."))))

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
  (let* ((task-at-point (task-manager-get-task-at-point))
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
    (message "Tags updated.")))

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
                        ;; Use org calendar for due date selection
                        ((string= filter-type "due date")
                         (let ((date (task-manager-select-date-with-calendar)))
                           (if date
                               date
                             (message "Date selection cancelled.")
                             (keyboard-quit))))
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
  "Move selected tasks to Week section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Week" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Week." (length tasks-to-move)))))

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

(defun task-manager-move-to-today ()
  "Move selected tasks to Today section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Today" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Today." (length tasks-to-move)))))

(defun task-manager-move-to-inbox ()
  "Move selected tasks to Inbox section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Inbox" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Inbox." (length tasks-to-move)))))

(defun task-manager-move-to-monday ()
  "Move selected tasks to Monday section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Monday" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Monday." (length tasks-to-move)))))

(defun task-manager-move-to-calendar ()
  "Move selected tasks to Calendar section. If cursor is on a task, use that task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (tasks-to-move (if task-at-point
                           (list (cdr task-at-point))
                         task-manager-selected-tasks)))
    
    (when tasks-to-move
      (dolist (task tasks-to-move)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash "Calendar" task-manager-tasks))))))
      
      (setq task-manager-selected-tasks nil)
      (task-manager-save-tasks)
      (task-manager-refresh)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Calendar." (length tasks-to-move)))))

(defun task-manager-move-all-to-week ()
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

