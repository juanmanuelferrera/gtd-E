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
;;  l: Duplicate task at cursor
;;  L: Load tasks
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
;;  w: Move task to Week section
;;  W: Move all tasks from Inbox and Today to Week
;;  o: Focus on Monday section
;;  c: Focus on Calendar section
;;  C: Toggle commands visibility
;;  f: Filter tasks by properties
;;  X: Set reminders
;;  b: Bulk edit tasks
;;  E: Export tasks (org, json, csv)
;;  I: Import tasks
;;  i: Focus on Inbox section
;;  v: Save tasks
;;  L: Load tasks
;;  SPC: Toggle task selection
;;  u: Undo (up to 15 operations)
;;  n: Next task
;;  O: Import tasks from org-agenda in a directory
;;  e: List tasks with dates
;;  M: Move all tasks from a section
;;  N: Manage notes for a task
;;  n: View notes for a task
;; 
;; Features:
;;  - URLs and file paths in tasks are automatically clickable
;;  - Automatic daily task migration from Today to Week at 3 AM
;;  - Import tasks from org-agenda in a directory
;; 
;; To start: M-x task-manager2-init

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'calendar)
(require 'autorevert)
(require 'org)
(require 'browse-url)  ; Add this line to require browse-url

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

;; Add a hash table to store task notes
(defvar task-manager-task-notes (make-hash-table :test 'equal)
  "Hash table storing notes for each task.")

(defun task-manager-get-task-at-point ()
  "Get the task at the current point, returning a cons of (section . task)."
  (let ((task-found nil)
        (task-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    
    ;; Check if we're on a task line, accounting for possible note icon and section info
    (when (string-match "^\\s-*\\[\\([X ]\\)\\]\\s-*\\(?:📝\\s-*\\)?\\(.*?\\)\\(?:\\s-*(in \\(.*\\))?\\)?$" task-line)
      (let* ((task-text (match-string 2 task-line))
             (section-info (match-string 3 task-line))
             (section nil)
             (original-task nil))
        
        ;; If we have section info from "Due Today", use that directly
        (when section-info
          (setq section section-info)
          (let ((tasks (gethash section task-manager-tasks)))
            (dolist (full-task tasks)
              (let ((stripped-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" full-task))
                    (stripped-task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task-text)))
                (when (string-match-p (regexp-quote (string-trim stripped-task-text)) (string-trim stripped-task))
                  (setq original-task full-task)
                  (setq task-found t))))))
        
        ;; If no section info or task not found, search in all sections
        (unless task-found
          (dolist (sec task-manager-sections)
            (let ((tasks (gethash sec task-manager-tasks)))
              (dolist (full-task tasks)
                (let ((stripped-task (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" full-task))
                      (stripped-task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task-text)))
                  (when (string-match-p (regexp-quote (string-trim stripped-task-text)) (string-trim stripped-task))
                    (setq section sec)
                    (setq original-task full-task)
                    (setq task-found t)))))))
        
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
  (setq task-manager-tasks (make-hash-table :test 'equal))
  (setq task-manager-expanded-sections (make-hash-table :test 'equal))
  (setq task-manager-selected-tasks nil)
  (setq task-manager-task-notes (make-hash-table :test 'equal))
  (setq task-manager-undo-history nil)
  (setq task-manager-show-commands nil)
  (setq task-manager-last-backup-time 0)
  ;; Initialize calendar
  (require 'calendar)
  (require 'org)
  (setq org-calendar-buffer "*Calendar*")
  ;; Start with all sections collapsed
  (dolist (section task-manager-sections)
    (puthash section nil task-manager-expanded-sections))
  (switch-to-buffer (get-buffer-create "*Task Manager*"))
  (task-manager-mode)
  (task-manager-load-tasks)
  (task-manager-check-overdue-migration)
  (task-manager-refresh)
  (message "Task manager initialized."))

(define-derived-mode task-manager-mode special-mode "Task Manager"
  "Major mode for task management."
  (buffer-disable-undo)
  (setq buffer-read-only t)
  ;; Set up key bindings
  (define-key task-manager-mode-map (kbd "C-g") 'keyboard-quit)
  (define-key task-manager-mode-map (kbd "a") 'task-manager-add-task)
  
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
    (define-key o-map (kbd "a") 'task-manager-focus-archive)
    
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
  (define-key task-manager-mode-map (kbd "E") 'task-manager-export)
  (define-key task-manager-mode-map (kbd "f") 'task-manager-filter-tasks)
  (define-key task-manager-mode-map (kbd "g") 'task-manager-refresh)
  (define-key task-manager-mode-map (kbd "i") 'task-manager-move-to-inbox)
  (define-key task-manager-mode-map (kbd "I") 'task-manager-import)
  (define-key task-manager-mode-map (kbd "k") 'task-manager-delete-tasks)
  (define-key task-manager-mode-map (kbd "K") 'task-manager-delete-section-tasks)
  (define-key task-manager-mode-map (kbd "l") 'task-manager-duplicate-task)
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
  (define-key task-manager-mode-map (kbd "z") 'task-manager-collapse-all-sections)
  (define-key task-manager-mode-map (kbd "SPC") 'task-manager-toggle-task-at-point))

;; Add mode hook to make 'd' key override all other functions
(add-hook 'task-manager-mode-hook
          (lambda ()
            ;; Override any existing keybindings for 'd'
            (local-set-key (kbd "d") 'task-manager-set-due-date)
            (local-set-key (kbd "C-d") 'task-manager-set-due-date)
            (local-set-key (kbd "M-d") 'task-manager-set-due-date)
            ;; Make sure the key binding takes precedence over other modes
            (define-key task-manager-mode-map (kbd "d") 'task-manager-set-due-date)
            ;; Disable any other keybindings that might interfere
            (define-key task-manager-mode-map (kbd "C-d") nil)
            (define-key task-manager-mode-map (kbd "M-d") nil)))

;; Remove any duplicate keybindings
(define-key task-manager-mode-map (kbd "d") 'task-manager-set-due-date)

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

(defun task-manager-add-task-month ()
  "Add a task to the Month section."
  (interactive)
  (task-manager-add-task "Month"))

(defun task-manager-add-task-calendar ()
  "Add a task to the Calendar section."
  (interactive)
  (task-manager-add-task "Calendar"))

(defun task-manager-add-task-someday ()
  "Add a task to the Someday section."
  (interactive)
  (task-manager-add-task "Someday"))

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

(defun task-manager-add-multiple-tasks-month ()
  "Add multiple tasks to the Month section."
  (interactive)
  (task-manager-add-multiple-tasks "Month"))

(defun task-manager-add-multiple-tasks-calendar ()
  "Add multiple tasks to the Calendar section."
  (interactive)
  (task-manager-add-multiple-tasks "Calendar"))

(defun task-manager-add-multiple-tasks-someday ()
  "Add multiple tasks to the Someday section."
  (interactive)
  (task-manager-add-multiple-tasks "Someday"))

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
      (task-manager-refresh-keep-cursor)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to Someday." (length tasks-to-move)))))

(defun task-manager-toggle-commands ()
  "Toggle the visibility of the commands section."
  (interactive)
  (setq task-manager-show-commands (not task-manager-show-commands))
  (task-manager-refresh-keep-cursor)
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
    
    ;; Calculate days until December 19th, 2045
    (let* ((target-date (encode-time 0 0 0 19 12 2045))
           (now (current-time))
           (birth-date (encode-time 0 0 0 19 12 1961))
           (seconds-remaining (- (time-to-seconds target-date) (time-to-seconds now)))
           (days-remaining (floor (/ seconds-remaining 86400)))
           (weeks-remaining (floor (/ days-remaining 7)))
           (total-life-seconds (- (time-to-seconds target-date) (time-to-seconds birth-date)))
           (life-percentage (floor (* 100 (/ seconds-remaining total-life-seconds)))))
      (insert (format "%s [%s] -> %d%% | %d Weeks | %d Days left in this body\n\n" 
                     current-date current-time life-percentage weeks-remaining days-remaining)))
    
    ;; Display tasks due today at the top
    (insert (propertize "⏰ Due Today" 'face '(:inherit font-lock-keyword-face :weight bold)))
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
                     (has-notes (gethash task task-manager-task-notes))
                     (display-task (replace-regexp-in-string "^TODAY " "" task)))
                ;; Insert checkbox marker
                (insert (format "  [%s] " (if selected "X" " ")))
                
                ;; Add note indicator if task has notes
                (when has-notes
                  (insert (propertize "📝 " 'face '(:foreground "blue" :weight bold))))
                
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
    
    ;; Display ALL sections with tasks (excluding Notes section)
    (dolist (section task-manager-sections)
      ;; Skip the Notes section
      (unless (string= section "Notes")
        (let ((tasks (gethash section task-manager-tasks))
              (expanded (gethash section task-manager-expanded-sections)))
          ;; Add section header with color based on section
          (insert (propertize (format "%s %s (%d)" 
                                    (cond
                                     ((string= section "Inbox") "📥")
                                     ((string= section "Today") "🌞")
                                     ((string= section "Week") "📊")
                                     ((string= section "Monday") "📌")
                                     ((string= section "Calendar") "📆")
                                     ((string= section "Someday") "⏳")
                                     ((string= section "Archive") "📦")
                                     (t ""))
                                    section (length tasks))
                             'face (cond
                                   ((string= section "Inbox") '(:foreground "#4ECDC4" :weight bold))
                                   ((string= section "Today") '(:foreground "#FF6B6B" :weight bold))
                                   ((string= section "Week") '(:foreground "#45B7D1" :weight bold))
                                   ((string= section "Monday") '(:foreground "#96CEB4" :weight bold))
                                   ((string= section "Calendar") '(:foreground "#FFEEAD" :weight bold))
                                   ((string= section "Someday") '(:foreground "#D4A5A5" :weight bold))
                                   ((string= section "Archive") '(:foreground "#9B9B9B" :weight bold))
                                   (t '(:foreground "#4A4A4A" :weight bold)))))
          (if expanded
              (insert " [-]\n")
            (insert " [+]\n"))
          
          ;; Show tasks if section is expanded
          (when expanded
            ;; Sort tasks if in Calendar section
            (when (string= section "Calendar")
              (setq tasks (task-manager-sort-tasks-by-due-date tasks)))
            
            (dolist (task tasks)
              (let ((selected (member task task-manager-selected-tasks))
                    (has-notes (gethash task task-manager-task-notes))
                    (display-task (replace-regexp-in-string "^TODAY " "" task)))
                ;; Insert checkbox marker
                (insert (format "  [%s] " (if selected "X" " ")))
                
                ;; Add note indicator if task has notes
                (when has-notes
                  (insert (propertize "📝 " 'face '(:foreground "blue" :weight bold))))
                
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
                
                (insert "\n")))))))
    
    ;; Display commands if enabled, otherwise show a hint
    (if task-manager-show-commands
        (progn
          (insert "\nCommands:\n")
          (insert "Task Entry:\n")
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
          (insert "\nTask Management:\n")
          (insert "  RET: Edit task at cursor\n")
          (insert "  SPC: Toggle task selection\n")
          (insert "  k: Delete selected tasks\n")
          (insert "  K: Delete all tasks in section\n")
          (insert "  l: Duplicate task at cursor\n")
          (insert "  b: Bulk edit selected tasks\n")
          (insert "  u: Undo last operation\n")
          (insert "\nTask Properties:\n")
          (insert "  d: Set due date\n")
          (insert "  D: Clear due date\n")
          (insert "  r: Set recurring task\n")
          (insert "  R: Clear recurring status\n")
          (insert "  T: Add/remove tags\n")
          (insert "  X: Set reminders\n")
          (insert "  N: Add/edit notes\n")
          (insert "  n: View notes\n")
          (insert "\nNavigation:\n")
          (insert "  o i: Open Inbox section\n")
          (insert "  o t: Open Today section\n")
          (insert "  o w: Open Week section\n")
          (insert "  o m: Open Monday section\n")
          (insert "  o s: Open Someday section\n")
          (insert "  o c: Open Calendar section\n")
          (insert "  o a: Open Archive section\n")
          (insert "  n: Next task\n")
          (insert "  p: Previous task\n")
          (insert "\nTask Movement:\n")
          (insert "  i: Move to Inbox\n")
          (insert "  t: Move to Today\n")
          (insert "  w: Move to Week\n")
          (insert "  m: Move to Monday\n")
          (insert "  s: Move to Someday\n")
          (insert "  c: Move to Calendar\n")
          (insert "  W: Move all Inbox/Today to Week\n")
          (insert "\nOther Functions:\n")
          (insert "  B: Create manual backup\n")
          (insert "  C: Toggle commands visibility\n")
          (insert "  E: Export tasks\n")
          (insert "  I: Import tasks\n")
          (insert "  O: Import from org-agenda\n")
          (insert "  S: Search tasks\n")
          (insert "  f: Filter tasks\n")
          (insert "  g: Refresh buffer\n")
          (insert "  q: Quit buffer\n")
          (insert "  z: Collapse all sections\n")
          (insert "  e: List tasks with dates\n")
          (insert "  M: Move all tasks from a section\n"))
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
  (let ((task nil))
    (condition-case nil
        (setq task (if (string= section "Calendar")
                       ;; For Calendar section, first get the date
                       (let ((date (task-manager-select-date-with-calendar)))
                         (if date
                             (let ((task-text (read-string "Enter new task: " "")))
                               (if (string-empty-p task-text)
                                   (progn
                                     (message "Task creation cancelled.")
                                     (keyboard-quit))
                                 (format "%s [Due: %s]" task-text date)))
                           (message "Date selection cancelled.")
                           (keyboard-quit)))
                     ;; For other sections, just get the task
                     (let ((task-text (read-string "Enter new task: " "")))
                       (if (string-empty-p task-text)
                           (progn
                             (message "Task creation cancelled.")
                             (keyboard-quit))
                         task-text))))
      (quit (setq task nil)))
    (when task
      ;; Check if task has a due date in any format
      (if (or (string-match "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" task)
              (string-match "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" task)
              (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" task))
          ;; If task has a due date, add it to Calendar section
          (progn
            (push task (gethash "Calendar" task-manager-tasks))
            (message "Task added to Calendar section due to date."))
        ;; Otherwise add it to the requested section
        (progn
          (push task (gethash section task-manager-tasks))
          (message "Task added to %s section." section)))
      (task-manager-save-tasks)
      (task-manager-refresh-keep-cursor)
      ;; Find and move to the new task
      (goto-char (point-min))
      (search-forward task nil t)
      (beginning-of-line))))

(defun task-manager-add-multiple-tasks (&optional section)
  "Add multiple tasks to SECTION. If SECTION is not provided, use the current section."
  (interactive)
  (let* ((section (or section (task-manager-get-current-section)))
         (tasks '())
         (task nil))
    (condition-case nil
        (progn
          (setq task (read-string (format "Add task to %s (empty line to finish): " section) ""))
          (while (not (string-empty-p task))
            (push task tasks)
            (setq task (read-string (format "Add task to %s (empty line to finish): " section) ""))))
      (quit (setq tasks nil)))
    (when (and tasks (> (length tasks) 0))
      (setf (gethash section task-manager-tasks)
            (append (gethash section task-manager-tasks) (reverse tasks)))
      (task-manager-save-tasks)
      (task-manager-refresh-keep-cursor)
      ;; Find and move to the last added task
      (goto-char (point-min))
      (search-forward (car (reverse tasks)) nil t)
      (beginning-of-line)
      (message "Added %d tasks to %s." (length tasks) section))))

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
      (task-manager-refresh-keep-cursor)
      
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
      (task-manager-refresh-keep-cursor)
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
      (task-manager-refresh-keep-cursor)
      
      ;; Restore cursor position
      (goto-char (point-min))
      (forward-line (1- current-line))
      (beginning-of-line)
      
      (message "Moved %d task(s) to %s." (length tasks-to-move) target))))

(defun task-manager-toggle-all-sections ()
  "Toggle expansion of all sections. First press collapses all sections, second press expands them."
  (interactive)
  (let ((all-collapsed t))
    ;; Check if all sections are collapsed
    (dolist (section task-manager-sections)
      (when (gethash section task-manager-expanded-sections)
        (setq all-collapsed nil)))
    
    ;; Toggle all sections to the opposite state
    (dolist (section task-manager-sections)
      (puthash section (not all-collapsed) task-manager-expanded-sections))
    
    (task-manager-refresh-keep-cursor)
    (message "All sections %s" (if all-collapsed "expanded" "collapsed"))))

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
          (task-manager-refresh-keep-cursor)
          
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
      (task-manager-refresh-keep-cursor)
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
  
  ;; Move any tasks with dates to Calendar section before saving
  (task-manager-move-tasks-with-dates-to-calendar)
  
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
      
      ;; Write notes section (hidden from main view)
      (insert "\n* Notes\n")
      (maphash (lambda (task notes)
                 (when (and notes (not (string-empty-p notes)))
                   (insert (format "** %s\n" task))
                   (insert ":NOTES:\n")
                   (insert notes)
                   (unless (string-suffix-p "\n" notes)
                     (insert "\n"))
                   (insert ":END:\n")))
               task-manager-task-notes)
      
      ;; Write to file
      (write-region (point-min) (point-max) task-manager-save-file))
    
    (message "Tasks saved to %s" task-manager-save-file)))

(defun task-manager-load-tasks ()
  "Load tasks from the save file."
  (interactive)
  ;; Initialize empty lists for all sections
  (dolist (section task-manager-sections)
    (puthash section nil task-manager-tasks))
  
  ;; Clear notes
  (setq task-manager-task-notes (make-hash-table :test 'equal))
  
  (when (file-exists-p task-manager-save-file)
    ;; Read and parse the org file
    (with-temp-buffer
      (insert-file-contents task-manager-save-file)
      (org-mode)
      (goto-char (point-min))
      (let ((current-section nil)
            (current-task nil)
            (in-notes-section nil)
            (in-notes-content nil))
        (while (not (eobp))
          (cond
           ;; Section header (level 1 heading)
           ((looking-at "^\\* \\(.+\\)$")
            (let ((section-name (match-string 1)))
              (if (string= section-name "Notes")
                  (setq in-notes-section t)
                (setq in-notes-section nil)
                (setq current-section section-name)
                ;; Make sure section exists (handle custom sections)
                (unless (member current-section task-manager-sections)
                  (add-to-list 'task-manager-sections current-section)
                  (puthash current-section nil task-manager-tasks)))))
           
           ;; Task (level 2 heading)
           ((and (not in-notes-section)
                 current-section
                 (looking-at "^\\*\\* \\(.+\\)$"))
            (let ((task (match-string 1)))
              (when (and task (not (string-empty-p (string-trim task))))
                (push task (gethash current-section task-manager-tasks)))))
           
           ;; Note task (level 2 heading in Notes section)
           ((and in-notes-section
                 (looking-at "^\\*\\* \\(.+\\)$"))
            (setq current-task (match-string 1))
            (setq in-notes-content nil))
           
           ;; Start of notes content
           ((and in-notes-section
                 current-task
                 (looking-at "^:NOTES:$"))
            (setq in-notes-content t)
            (let ((notes-content ""))
              (forward-line)
              (while (and (not (eobp))
                         (not (looking-at "^:END:$")))
                (setq notes-content (concat notes-content (buffer-substring-no-properties (line-beginning-position) (line-end-position)) "\n"))
                (forward-line))
              (when (not (string-empty-p notes-content))
                (puthash current-task (string-trim-right notes-content) task-manager-task-notes))
              (setq in-notes-content nil))))
          (forward-line)))
      
      ;; Move any tasks with dates to Calendar section
      (task-manager-move-tasks-with-dates-to-calendar)
      
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
      (task-manager-refresh-keep-cursor)
      
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
    (task-manager-refresh-keep-cursor)
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
    (task-manager-refresh-keep-cursor)
    (message "Priority for task set to %s." priority)))

(defun task-manager-select-date-with-calendar ()
  "Select a date using org's calendar."
  (let ((date-time nil))
    (condition-case nil
        (progn
          (require 'calendar)
          (require 'org)
          (setq org-calendar-buffer "*Calendar*")
          (setq date-time
                (progn
                  (when (active-minibuffer-window)
                    (delete-minibuffer-contents))
                  (org-read-date t t nil "Select date: "))))
      (quit (progn
             (when (active-minibuffer-window)
               (delete-minibuffer-contents))
             nil)))
    ;; Cleanup form
    (when (get-buffer org-calendar-buffer)
      (kill-buffer org-calendar-buffer))
    (when (active-minibuffer-window)
      (delete-minibuffer-contents))
    
    (when date-time
      (let* ((decoded (decode-time date-time))
             (year (nth 5 decoded))
             (month (nth 4 decoded))
             (day (nth 3 decoded)))
        (format "%04d-%02d-%02d" 
                year month day)))))

(defun task-manager-set-due-date ()
  "Set the due date and time for a task using org's calendar.
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
         (date-time (task-manager-select-date-with-calendar))
         ;; Remove any existing due date tag
         (task-without-date (replace-regexp-in-string "\\[Due: [^]]*\\]" "" task))
         (task-without-date (string-trim task-without-date))
         (new-task (if date-time
                      (format "%s [Due: %s]" task-without-date date-time)
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
    (task-manager-refresh-keep-cursor)
    (if date-time
        (message "Due date and time set to %s." date-time)
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
    (task-manager-refresh-keep-cursor)
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
    (task-manager-refresh-keep-cursor)
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
    (task-manager-refresh-keep-cursor)
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
      (task-manager-refresh-keep-cursor)
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
      (task-manager-refresh-keep-cursor)
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
  
  (task-manager-refresh-keep-cursor)
  
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
                  (task-manager-refresh-keep-cursor)
                  
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
      (task-manager-refresh-keep-cursor)
      
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
      (task-manager-refresh-keep-cursor)
      
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
      (task-manager-refresh-keep-cursor)
      
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
      (task-manager-refresh-keep-cursor)
      
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
      (task-manager-refresh-keep-cursor)
      
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
    (task-manager-refresh-keep-cursor)
    
    ;; Focus on Week section
    (puthash "Week" t task-manager-expanded-sections)
    (goto-char (point-min))
    (search-forward "Week" nil t)
    (forward-line 1)
    
    (message "Moved %d tasks from Inbox and Today to Week." count)))

(defun task-manager-duplicate-task ()
  "Duplicate the task at the current cursor position and move cursor to the new task."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (current-line (line-number-at-pos))
         (section nil)
         (task nil)
         (timestamp (format-time-string "%Y%m%d%H%M%S")))
    
    (if task-at-point
        (progn
          (setq section (car task-at-point))
          (setq task (cdr task-at-point))
          
          ;; Add a unique identifier to the duplicated task
          (let ((duplicated-task (format "%s [ID: %s]" task timestamp)))
            ;; Add the task to the same section
            (push duplicated-task (gethash section task-manager-tasks))
            
            ;; Save and refresh
            (task-manager-save-tasks)
            (task-manager-refresh-keep-cursor)
            
            ;; Find the newly added task (it should be the first task in the section)
            (goto-char (point-min))
            (search-forward section nil t)
            (forward-line 1)  ; Move to the first task line
            (beginning-of-line)
            
            (message "Task duplicated in %s section." section)))
      (message "No task at cursor position."))))

;; Add timer for automatic task migration
(defvar task-manager-auto-migrate-timer nil
  "Timer object for automatic task migration from Today to Week.")

(defun task-manager-auto-migrate-today-to-week ()
  "Automatically move all tasks from Today section to Week section."
  (let ((today-tasks (gethash "Today" task-manager-tasks))
        (week-tasks (gethash "Week" task-manager-tasks))
        (count 0))
    
    ;; Add all Today tasks to Week
    (dolist (task today-tasks)
      (push task week-tasks)
      (setq count (1+ count)))
    
    ;; Update the section hash tables
    (puthash "Week" week-tasks task-manager-tasks)
    (puthash "Today" nil task-manager-tasks)
    
    ;; Remove any of these tasks from selection if they were selected
    (setq task-manager-selected-tasks 
          (seq-filter (lambda (task)
                        (not (member task today-tasks)))
                      task-manager-selected-tasks))
    
    (task-manager-save-tasks)
    
    ;; Only refresh if the task manager buffer is visible
    (let ((buffer (get-buffer "*Task Manager*")))
      (when (and buffer (get-buffer-window buffer))
        (with-current-buffer buffer
          (task-manager-refresh-keep-cursor))))
    
    (message "Auto-migrated %d tasks from Today to Week section." count)))

(defun task-manager-schedule-auto-migrate ()
  "Schedule automatic migration of tasks from Today to Week section at 3 AM."
  (interactive)
  ;; Cancel any existing timer
  (when task-manager-auto-migrate-timer
    (cancel-timer task-manager-auto-migrate-timer))
  
  ;; Calculate time until next 3 AM
  (let* ((now (current-time))
         (now-decoded (decode-time now))
         (hour (nth 2 now-decoded))
         (minute (nth 1 now-decoded))
         (second (nth 0 now-decoded))
         (target-hour 3)
         (target-minute 0)
         (target-second 0)
         (seconds-until-target
          (if (< hour target-hour)
              ;; If current time is before 3 AM, calculate seconds until 3 AM today
              (+ (* (- target-hour hour) 3600)
                 (* (- target-minute minute) 60)
                 (- target-second second))
            ;; If current time is after 3 AM, calculate seconds until 3 AM tomorrow
            (+ (* (- (+ 24 target-hour) hour) 3600)
               (* (- target-minute minute) 60)
               (- target-second second)))))
    
    ;; Schedule the migration
    (setq task-manager-auto-migrate-timer
          (run-at-time seconds-until-target nil 'task-manager-auto-migrate-today-to-week))
    
    ;; Calculate and show the next migration time
    (let ((next-migration-time (time-add now (seconds-to-time seconds-until-target))))
      (message "Next automatic migration scheduled for %s" 
               (format-time-string "%Y-%m-%d %H:%M:%S" next-migration-time)))))

;; Add key binding for importing from org-agenda
(define-key task-manager-mode-map (kbd "O") 'task-manager-import-from-org-agenda)

;; Function to import tasks from org-agenda in a directory
(defun task-manager-import-from-org-agenda ()
  "Import tasks from org-agenda in all .org files in a directory and add them to Inbox.
Excludes tasks.org and backup files (tasks-backup-*). After importing, removes the tasks from the original files."
  (interactive)
  (let ((directory (read-directory-name "Directory with org files: "))
        (imported-count 0))
    (dolist (file (directory-files directory t "\\.org$"))
      (let ((filename (file-name-nondirectory file)))
        (unless (or (string= filename "tasks.org")
                   (string-match-p "^tasks-backup-" filename))
          (with-current-buffer (find-file-noselect file)
            (org-mode)
            (goto-char (point-min))
            (let ((positions-to-delete '()))
              (while (re-search-forward "^\\*+ \\(TODO\\|DONE\\) \\(.*\\)$" nil t)
                (unless (string= (match-string 1) "DONE")
                  (push (cons (match-beginning 0)
                            (save-excursion
                              (forward-line 1)
                              (point)))
                        positions-to-delete)
                  (push (format "%s [Source: %s]" 
                              (match-string 2)
                              filename)
                        (gethash "Inbox" task-manager-tasks))
                  (setq imported-count (1+ imported-count))))
              (when positions-to-delete
                (setq positions-to-delete (sort positions-to-delete (lambda (a b) (> (car a) (car b)))))
                (dolist (pos positions-to-delete)
                  (delete-region (car pos) (cdr pos)))
                (save-buffer)))))))
    (task-manager-save-tasks)
    (task-manager-refresh-keep-cursor)
    (message "Imported %d tasks from org-agenda to Inbox and removed them from source files." imported-count)))

(defun task-manager-check-overdue-migration ()
  "Check if tasks should have been migrated since last run and migrate if needed."
  (let* ((now (current-time))
         (now-decoded (decode-time now))
         (current-hour (nth 2 now-decoded))
         (current-minute (nth 1 now-decoded))
         (last-run-file (expand-file-name "~/.task-manager-last-run"))
         (last-run-time
          (if (file-exists-p last-run-file)
              (with-temp-buffer
                (insert-file-contents last-run-file)
                (string-to-number (buffer-string)))
            ;; If no last run file, assume last run was 24 hours ago
            (- (time-to-seconds now) 86400)))
         (last-run-decoded (decode-time (seconds-to-time last-run-time)))
         (last-run-hour (nth 2 last-run-decoded))
         (last-run-date (nth 3 last-run-decoded))
         (current-date (nth 3 now-decoded)))
    
    ;; Check if we missed a migration
    (when (or
           ;; If last run was before 3 AM and current time is after 3 AM
           (and (< last-run-hour 3) (>= current-hour 3))
           ;; If we missed a whole day
           (> current-date last-run-date))
      (message "Performing overdue task migration from Today to Week...")
      (task-manager-auto-migrate-today-to-week))
    
    ;; Save current time as last run time
    (with-temp-file last-run-file
      (insert (number-to-string (time-to-seconds now))))))

(defun task-manager-collapse-all-sections ()
  "Collapse all sections."
  (interactive)
  (dolist (section task-manager-sections)
    (puthash section nil task-manager-expanded-sections))
  (task-manager-refresh-keep-cursor)
  (message "All sections collapsed"))

(defun task-manager-list-tasks-with-dates ()
  "List all tasks with dates in a new buffer, organized by months and weeks."
  (interactive)
  (let ((tasks-with-dates nil)
        (buffer (get-buffer-create "*Tasks with Dates*"))
        (today (format-time-string "%a, %d")))  ; Add comma between day and date
    
    ;; Collect all tasks with dates
    (maphash
     (lambda (section tasks)
       (dolist (task tasks)
         (let ((due-date (task-manager-get-due-date task)))
           (when due-date
             (push (cons due-date (cons section task)) tasks-with-dates)))))
     task-manager-tasks)
    
    ;; Sort tasks by date
    (setq tasks-with-dates
          (sort tasks-with-dates
                (lambda (a b)
                  (time-less-p (car a) (car b)))))
    
    ;; Display tasks in buffer
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        
        ;; Group tasks by month
        (let ((current-month nil)
              (current-week nil)
              (current-date nil))
          (dolist (task-info tasks-with-dates)
            (let* ((due-date (car task-info))
                   (section (car (cdr task-info)))
                   (task (cdr (cdr task-info)))
                   (month-year (format-time-string "%B %Y" due-date))
                   (week-number (format-time-string "%V" due-date))
                   (formatted-date (format-time-string "%a, %d" due-date)))  ; Add comma between day and date
              
              ;; Print month header if it's a new month
              (unless (string= month-year current-month)
                (setq current-month month-year)
                (insert (format "\n%s\n" (make-string (length month-year) ?=)))
                (insert (format "%s\n" month-year))
                (insert (make-string (length month-year) ?=) "\n\n")
                (setq current-week nil)
                (setq current-date nil))
              
              ;; Print week header if it's a new week
              (unless (string= week-number current-week)
                (when current-week
                  (insert "\n"))  ; Add blank line between weeks
                (setq current-week week-number)
                (insert (format "Week %s\n" week-number))
                (insert (make-string (+ 5 (length week-number)) ?-) "\n")
                (setq current-date nil))
              
              ;; Print date header if it's a new date
              (unless (string= formatted-date current-date)
                (when current-date
                  (insert "\n"))  ; Add blank line between dates
                (setq current-date formatted-date)
                (insert "  [")
                ;; Highlight today's date
                (if (string= formatted-date today)
                    (insert-text-button formatted-date
                                      'face '(:foreground "red" :weight bold)
                                      'help-echo "Today")
                  (insert formatted-date))
                (insert "]\n"))
              
              ;; Print the task
              (insert "    ")  ; Indent tasks under their date
              (insert-text-button
               task
               'action (lambda (_)
                        (task-manager-goto-task section task))
               'follow-link t
               'help-echo "Click to go to this task")
              (insert "\n"))))
        
        (goto-char (point-min))
        (special-mode)))
    
    (switch-to-buffer buffer)))

;; Update the keybinding in task-manager-mode
(define-key task-manager-mode-map (kbd "e") 'task-manager-list-tasks-with-dates)

(defun task-manager-get-due-date (task)
  "Extract the due date from TASK and return it as a time value.
Returns nil if no due date is found."
  (when (string-match "\\[Due: \\([^]]+\\)\\]" task)
    (let ((date-str (match-string 1 task)))
      (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date-str)
        (let ((year (string-to-number (match-string 1 date-str)))
              (month (string-to-number (match-string 2 date-str)))
              (day (string-to-number (match-string 3 date-str))))
          (encode-time 0 0 0 day month year))))))

(defun task-manager-move-all-from-section ()
  "Move all tasks from one section to another section."
  (interactive)
  (let* ((source-section (completing-read "Move all tasks from section: " task-manager-sections))
         (target-section (completing-read (format "Move all tasks from %s to section: " source-section) 
                                        task-manager-sections))
         (tasks-to-move (gethash source-section task-manager-tasks))
         (count (length tasks-to-move)))
    
    (when (and source-section target-section
               (not (string= source-section target-section))
               (> count 0))
      ;; Add all tasks to target section
      (dolist (task tasks-to-move)
        (push task (gethash target-section task-manager-tasks)))
      
      ;; Clear source section
      (puthash source-section nil task-manager-tasks)
      
      ;; Remove any of these tasks from selection if they were selected
      (setq task-manager-selected-tasks 
            (seq-filter (lambda (task)
                          (not (member task tasks-to-move)))
                        task-manager-selected-tasks))
      
      (task-manager-save-tasks)
      (task-manager-refresh-keep-cursor)
      
      ;; Focus on target section
      (puthash target-section t task-manager-expanded-sections)
      (goto-char (point-min))
      (search-forward target-section nil t)
      (forward-line 1)
      
      (message "Moved %d tasks from %s to %s." count source-section target-section))))

;; Add key binding for moving all tasks from a section
(define-key task-manager-mode-map (kbd "M") 'task-manager-move-all-from-section)

;; Function to manage notes for a task
(defun task-manager-manage-notes ()
  "Open a buffer to manage notes for the task at cursor position."
  (interactive)
  (let* ((task-info (task-manager-get-task-at-point))
         (task (cdr task-info))
         (section (car task-info)))
    (if task
        (let* ((buffer-name (format "*Notes for: %s*" task))
               (existing-buffer (get-buffer buffer-name))
               (notes (gethash task task-manager-task-notes)))
          (if existing-buffer
              (switch-to-buffer existing-buffer)
            (let ((buffer (get-buffer-create buffer-name)))
              (switch-to-buffer buffer)
              (org-mode)
              (insert (format "Task: %s\n" task))
              (insert (format "Section: %s\n\n" section))
              (if notes
                  (progn
                    (insert notes)
                    (end-of-buffer)
                    (newline))
                (forward-line 3))
              (setq-local task-manager-current-task task)
              (setq-local task-manager-current-section section)
              (local-set-key (kbd "N") 'task-manager-save-notes)
              (local-set-key (kbd "C-k") 'task-manager-exit-notes))))
      (message "No task at cursor position"))))

(defun task-manager-save-notes ()
  "Save notes for the current task and close the buffer."
  (interactive)
  (let* ((task task-manager-current-task)
         (section task-manager-current-section)
         (notes (string-trim (buffer-substring-no-properties 
                            (save-excursion
                              (goto-char (point-min))
                              (forward-line 3)
                              (point))
                            (point-max)))))
    
    (if (string-empty-p notes)
        ;; If notes are empty, remove them and the note icon
        (progn
          (remhash task task-manager-task-notes)
          (message "Notes removed."))
      ;; Otherwise, save the notes
      (puthash task notes task-manager-task-notes)
      (message "Notes saved."))
    
    ;; Save to file and refresh display
    (task-manager-save-tasks)
    (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
      (kill-buffer)
      (with-current-buffer "*Task Manager*"
        (task-manager-refresh-keep-cursor)
        ;; Find and move to the task
        (goto-char (point-min))
        (search-forward task-text nil t)
        (beginning-of-line)))))

(defun task-manager-exit-notes ()
  "Exit the notes buffer without saving."
  (interactive)
  (let ((task task-manager-current-task)
        (task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task-manager-current-task)))
    (kill-buffer)
    (message "Notes not saved.")
    ;; Return to task manager and find the task
    (with-current-buffer "*Task Manager*"
      (goto-char (point-min))
      (search-forward task-text nil t)
      (beginning-of-line))))

(defun task-manager-view-notes ()
  "View notes for the task at point in read-only mode."
  (interactive)
  (let* ((task-at-point (task-manager-get-task-at-point))
         (task (when task-at-point (cdr task-at-point)))
         (section (when task-at-point (car task-at-point))))
    
    (if (not task)
        (message "No task at cursor position.")
      (let* ((notes (gethash task task-manager-task-notes))
             (notes-buffer-name (format "*View Notes: %s*" (substring task 0 (min 30 (length task)))))
             (notes-buffer (get-buffer-create notes-buffer-name)))
        
        (if (not notes)
            (message "No notes for this task.")
          ;; Set up the view buffer
          (with-current-buffer notes-buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Task Notes (Read Only)\n")
              (insert "====================\n\n")
              (insert (format "Task: %s\n" task))
              (insert (format "Section: %s\n\n" section))
              (insert "----------------------------------------\n\n")
              (insert notes)
              (goto-char (point-min))
              (view-mode)
              (local-set-key (kbd "q") 'kill-buffer)))
          
          ;; Display the buffer
          (switch-to-buffer notes-buffer))))))

;; Update task display to show note icon more prominently
(defun task-manager-format-task (task section &optional in-today-section)
  "Format a task for display, including checkbox and note icon if present."
  (let* ((selected (member task task-manager-selected-tasks))
         (has-notes (gethash task task-manager-task-notes))
         (display-task (replace-regexp-in-string "^TODAY " "" task))
         (formatted ""))
    
    ;; Add checkbox
    (setq formatted (concat formatted (format "  [%s] " (if selected "X" " "))))
    
    ;; Add note indicator if task has notes
    (when has-notes
      (setq formatted (concat formatted 
                             (propertize "📝 " 
                                        'face '(:foreground "blue" :weight bold)
                                        'help-echo "Task has notes (press N to view/edit)"))))
    
    ;; Add task text
    (setq formatted (concat formatted display-task))
    
    ;; Add section info for tasks in Today section
    (when in-today-section
      (setq formatted (concat formatted (format " (in %s)" section))))
    
    formatted))

;; Add key binding for editing task notes
(define-key task-manager-mode-map (kbd "N") 'task-manager-manage-notes)

(defun task-manager-parse-date-from-task (task)
  "Extract date from TASK string in format [Due: YYYY-MM-DD], YYYY-MM-DD or <YYYY-MM-DD Day>."
  (let ((date nil))
    ;; Try to match [Due: YYYY-MM-DD] format
    (if (string-match "\\[Due: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\]" task)
        (setq date (match-string 1 task))
      ;; Try to match <YYYY-MM-DD Day> format
      (if (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [A-Za-z]\\{3\\}>" task)
          (setq date (match-string 1 task))
        ;; Try to match YYYY-MM-DD format
        (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" task)
          (setq date (match-string 1 task)))))
    date))

(defun task-manager-move-tasks-with-dates-to-calendar ()
  "Move all tasks with due dates to Calendar section."
  (dolist (section task-manager-sections)
    (unless (string= section "Calendar")
      (let ((tasks (gethash section task-manager-tasks))
            (tasks-to-move nil))
        (dolist (task tasks)
          (when (or (string-match "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" task)
                    (string-match "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" task)
                    (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" task))
            (push task tasks-to-move)))
        ;; Move tasks to Calendar
        (dolist (task tasks-to-move)
          (setf (gethash section task-manager-tasks)
                (remove task (gethash section task-manager-tasks)))
          (push task (gethash "Calendar" task-manager-tasks)))))))

(defun task-manager-make-urls-clickable (text)
  "Convert URLs in text to clickable links, showing only the link text."
  (let ((url-regexp "https?://[^[:space:]]+"))
    (replace-regexp-in-string
     url-regexp
     (lambda (url)
       (let ((display-text (replace-regexp-in-string "^https?://" "" url)))
         (format "<a href=\"%s\">%s</a>" url display-text)))
     text)))

(defun task-manager-generate-html-report ()
  "Generate an HTML report with tables for Due Today, Today tasks, and Calendar tasks."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (due-today-tasks nil)
         (today-tasks nil)
         (calendar-tasks nil)
         (report-file (expand-file-name "task-report.html" (file-name-directory task-manager-save-file)))
         (markdown-file (expand-file-name "task-report.md" (file-name-directory task-manager-save-file))))
    
    ;; Collect tasks
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks)))
        (dolist (task tasks)
          (cond
           ;; Due Today tasks
           ((or (and (string-match "\\[Due: \\([^]]+\\)\\]" task)
                     (string= (match-string 1 task) today))
                (string-match-p "\\[Recurring: daily\\]" task))
            (push task due-today-tasks))
           ;; Today section tasks
           ((string= section "Today")
            (push task today-tasks))
           ;; Calendar section tasks
           ((string= section "Calendar")
            (push task calendar-tasks))))))
    
    ;; Reverse the lists to maintain original order
    (setq due-today-tasks (reverse due-today-tasks))
    (setq today-tasks (reverse today-tasks))
    (setq calendar-tasks (reverse calendar-tasks))
    
    ;; Take only first 15 Calendar tasks
    (setq calendar-tasks (seq-take calendar-tasks 15))
    
    ;; Generate HTML
    (with-temp-buffer
      (insert "<!DOCTYPE html>\n")
      (insert "<html lang=\"en\">\n<head>\n")
      (insert "<meta charset=\"UTF-8\">\n")
      (insert "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no\">\n")
      (insert "<style>\n")
      ;; Base styles for accessibility
      (insert "body { 
        font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'Helvetica Neue', sans-serif;
        margin: 0;
        padding: 20px;
        font-size: 17.28px;  /* Decreased from 19.2px by 10% */
        line-height: 1.6;
        color: #000;
        background-color: #fff;
        -webkit-text-size-adjust: 100%;
        -webkit-font-smoothing: antialiased;
      }\n")
      ;; Section headers
      (insert "h1 {
        font-size: 18px;
        font-weight: 700;
        margin: 20px 0 10px;
        padding: 8px;
        background-color: #f0f0f0;
        border-radius: 8px;
        color: #000;
      }\n")
      (insert "h2 { 
        font-size: 18px;
        font-weight: 700;
        margin: 20px 0 10px;
        padding: 8px;
        background-color: #f0f0f0;
        border-radius: 8px;
        color: #000;
      }\n")
      ;; Task containers
      (insert ".task-container {
        margin: 4px 0;
        padding: 6px 10px;
        border-radius: 4px;
        background-color: #fff;
        box-shadow: 0 1px 2px rgba(0,0,0,0.1);
        max-width: 95%;
        display: inline-block;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        cursor: pointer;
        -webkit-tap-highlight-color: transparent;
        transition: background-color 0.2s ease;
      }\n")
      ;; Due Today tasks
      (insert ".due-today {
        background-color: #ffebee;
        border-left: 3px solid #d32f2f;
      }\n")
      ;; Today tasks
      (insert ".today {
        background-color: #e8f5e9;
        border-left: 3px solid #2e7d32;
      }\n")
      ;; Calendar tasks
      (insert ".calendar {
        background-color: #e3f2fd;
        border-left: 3px solid #1976d2;
      }\n")
      ;; Task text
      (insert ".task-text {
        font-size: 17.28px;
        margin: 0;
        padding: 2px 0;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        line-height: 1.2;
      }\n")
      ;; Task date
      (insert ".task-date {
        font-size: 14.4px;
        color: #666;
        margin-bottom: 1px;
        white-space: nowrap;
      }\n")
      ;; Links
      (insert "a {
        color: #0066cc;
        text-decoration: underline;
        font-size: 17.28px;  /* Decreased from 19.2px by 10% */
      }\n")
      ;; Focus styles for accessibility
      (insert "*:focus {
        outline: 3px solid #0066cc;
        outline-offset: 2px;
      }\n")
      ;; Completed task styles
      (insert ".completed {
        text-decoration: line-through;
        opacity: 0.6;
      }\n")
      ;; High contrast mode support
      (insert "@media (prefers-contrast: more) {
        body { color: #000; background: #fff; }
        .due-today { background: #fff; border-left-color: #000; }
        .today { background: #fff; border-left-color: #000; }
        .calendar { background: #fff; border-left-color: #000; }
        a { color: #000; }
      }\n")
      ;; Dark mode support
      (insert "@media (prefers-color-scheme: dark) {
        body { color: #fff; background: #000; }
        h2 { background-color: #1a1a1a; color: #fff; }
        .task-container { background-color: #1a1a1a; }
        .due-today { background-color: #330000; }
        .today { background-color: #003300; }
        .calendar { background-color: #000033; }
        .task-date { color: #ccc; }
        a { color: #66b3ff; }
      }\n")
      (insert "</style>\n")
      (insert "<script>\n")
      (insert "document.addEventListener('DOMContentLoaded', function() {
        // Get all task containers
        const taskContainers = document.querySelectorAll('.task-container');
        
        // Add touch event listeners to each task container
        taskContainers.forEach(container => {
          let touchStartTime;
          let touchEndTime;
          
          container.addEventListener('touchstart', function(e) {
            touchStartTime = new Date().getTime();
          }, false);
          
          container.addEventListener('touchend', function(e) {
            touchEndTime = new Date().getTime();
            const touchDuration = touchEndTime - touchStartTime;
            
            // Only toggle if it's a quick tap (less than 300ms)
            if (touchDuration < 300) {
              const taskText = container.querySelector('.task-text');
              taskText.classList.toggle('completed');
              
              // Save the state to localStorage
              const taskId = container.getAttribute('data-task-id');
              const isCompleted = taskText.classList.contains('completed');
              localStorage.setItem('task-' + taskId, isCompleted);
            }
          }, false);
        });
        
        // Restore completed states from localStorage
        taskContainers.forEach(container => {
          const taskId = container.getAttribute('data-task-id');
          const isCompleted = localStorage.getItem('task-' + taskId) === 'true';
          if (isCompleted) {
            const taskText = container.querySelector('.task-text');
            taskText.classList.add('completed');
          }
        });
      });\n")
      (insert "</script>\n")
      (insert "</head>\n<body>\n")
      
      ;; Due Today section
      (insert "<h2>Due Today</h2>\n")
      (dolist (task due-today-tasks)
        (insert "<div class=\"task-container due-today\" data-task-id=\"")
        (insert (md5 task))  ;; Use MD5 hash of task as unique ID
        (insert "\">\n")
        (insert "<p class=\"task-text\">")
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (task-manager-make-urls-clickable task-text)))
        (insert "</p>\n</div>\n"))
      
      ;; Today tasks section
      (insert "<h2>Today</h2>\n")
      (dolist (task today-tasks)
        (insert "<div class=\"task-container today\" data-task-id=\"")
        (insert (md5 task))  ;; Use MD5 hash of task as unique ID
        (insert "\">\n")
        (insert "<p class=\"task-text\">")
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (task-manager-make-urls-clickable task-text)))
        (insert "</p>\n</div>\n"))
      
      ;; Calendar tasks section
      (insert "<h2>Calendar</h2>\n")
      (dolist (task calendar-tasks)
        (let* ((date (task-manager-parse-date-from-task task))
               (task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task))
               (task-text (replace-regexp-in-string "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" "" task-text))
               (task-text (replace-regexp-in-string "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" "" task-text))
               (task-text (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" "" task-text))
               (task-text (string-trim task-text))
               (formatted-date (when date
                                (let ((time (encode-time 0 0 0 
                                                         (string-to-number (substring date 8 10))
                                                         (string-to-number (substring date 5 7))
                                                         (string-to-number (substring date 0 4)))))
                                  (format-time-string "%A, %d/%m %Y" time)))))
          (insert "<div class=\"task-container calendar\" data-task-id=\"")
          (insert (md5 task))  ;; Use MD5 hash of task as unique ID
          (insert "\">\n")
          (when formatted-date
            (insert (format "<p class=\"task-date\">%s</p>\n" formatted-date)))
          (insert "<p class=\"task-text\">")
          (insert (task-manager-make-urls-clickable task-text))
          (insert "</p>\n</div>\n")))
      
      (insert "</body>\n</html>")
      
      ;; Save HTML to file silently
      (write-region (point-min) (point-max) report-file nil t))
    
    ;; Generate Markdown
    (with-temp-buffer
      (insert "# Task Report\n\n")
      
      ;; Due Today section
      (insert "## Due Today\n\n")
      (dolist (task due-today-tasks)
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (format "- %s\n" task-text))))
      (insert "\n")
      
      ;; Today section
      (insert "## Today\n\n")
      (dolist (task today-tasks)
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (format "- %s\n" task-text))))
      (insert "\n")
      
      ;; Calendar section
      (insert "## Calendar\n\n")
      (dolist (task calendar-tasks)
        (let* ((date (task-manager-parse-date-from-task task))
               (task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task))
               (task-text (replace-regexp-in-string "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" "" task-text))
               (task-text (replace-regexp-in-string "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" "" task-text))
               (task-text (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" "" task-text))
               (task-text (string-trim task-text))
               (formatted-date (when date
                                (let ((time (encode-time 0 0 0 
                                                       (string-to-number (substring date 8 10))
                                                       (string-to-number (substring date 5 7))
                                                       (string-to-number (substring date 0 4)))))
                                  (format-time-string "%A, %d/%m %Y" time)))))
          (insert (format "### %s\n\n" (or formatted-date "")))
          (insert (format "- %s\n\n" task-text))))
      
      ;; Save Markdown to file silently
      (write-region (point-min) (point-max) markdown-file nil t))
    
    (message "HTML and Markdown reports generated at %s" report-file)))

;; Add key binding for HTML report generation
(define-key task-manager-mode-map (kbd "H") 'task-manager-generate-html-report)

;; Add variable for the auto-report timer
(defvar task-manager-auto-report-timer nil
  "Timer object for automatic HTML report generation.")

(defun task-manager-schedule-auto-report ()
  "Schedule automatic generation of HTML report at 10:00 AM local time."
  (interactive)
  ;; Cancel any existing timer
  (when task-manager-auto-report-timer
    (cancel-timer task-manager-auto-report-timer))
  
  ;; Calculate time until next 10 AM
  (let* ((now (current-time))
         (now-decoded (decode-time now))
         (hour (nth 2 now-decoded))
         (minute (nth 1 now-decoded))
         (second (nth 0 now-decoded))
         (target-hour 10)
         (target-minute 0)
         (target-second 0)
         (seconds-until-target
          (if (< hour target-hour)
              ;; If current time is before 10 AM, calculate seconds until 10 AM today
              (+ (* (- target-hour hour) 3600)
                 (* (- target-minute minute) 60)
                 (- target-second second))
            ;; If current time is after 10 AM, calculate seconds until 10 AM tomorrow
            (+ (* (- (+ 24 target-hour) hour) 3600)
               (* (- target-minute minute) 60)
               (- target-second second)))))
    
    ;; Schedule the report generation
    (setq task-manager-auto-report-timer
          (run-at-time seconds-until-target nil 'task-manager-generate-auto-report))
    
    ;; Calculate and show the next report time
    (let ((next-report-time (time-add now (seconds-to-time seconds-until-target))))
      (message "Next automatic report generation scheduled for %s" 
               (format-time-string "%Y-%m-%d %H:%M:%S" next-report-time)))))

(defun task-manager-generate-auto-report ()
  "Generate and save HTML report automatically at scheduled time."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (due-today-tasks nil)
         (today-tasks nil)
         (calendar-tasks nil)
         (report-file "/Users/juanmanuelferreradiaz/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/task-report.html")
         (markdown-file "/Users/juanmanuelferreradiaz/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/task-report.md"))
    
    ;; Collect tasks
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks)))
        (dolist (task tasks)
          (cond
           ;; Due Today tasks
           ((or (and (string-match "\\[Due: \\([^]]+\\)\\]" task)
                     (string= (match-string 1 task) today))
                (string-match-p "\\[Recurring: daily\\]" task))
            (push task due-today-tasks))
           ;; Today section tasks
           ((string= section "Today")
            (push task today-tasks))
           ;; Calendar section tasks
           ((string= section "Calendar")
            (push task calendar-tasks))))))
    
    ;; Reverse the lists to maintain original order
    (setq due-today-tasks (reverse due-today-tasks))
    (setq today-tasks (reverse today-tasks))
    (setq calendar-tasks (reverse calendar-tasks))
    
    ;; Take only first 15 Calendar tasks
    (setq calendar-tasks (seq-take calendar-tasks 15))
    
    ;; Generate HTML
    (with-temp-buffer
      (insert "<!DOCTYPE html>\n")
      (insert "<html>\n<head>\n")
      (insert "<meta charset=\"UTF-8\">\n")
      (insert "<style>\n")
      (insert "body { font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'Helvetica Neue', sans-serif; margin: 40px; font-size: 32px; line-height: 1.5; color: #333; }\n")
      (insert "table { border-collapse: collapse; margin-bottom: 30px; width: 100%; }\n")
      (insert "th { background-color: #f8f8f8; padding: 12px; text-align: left; font-size: 28px; font-weight: 600; color: #666; border-bottom: 1px solid #e0e0e0; }\n")
      (insert "td { padding: 12px; border-bottom: 1px solid #e0e0e0; font-size: 32px; }\n")
      (insert "h1 { color: #333; margin-top: 40px; font-size: 41px; font-weight: 600; }\n")
      (insert "h2 { color: #333; margin-top: 30px; font-size: 34px; font-weight: 600; }\n")
      (insert ".due-today { background-color: #ff6b6b; color: white; }\n")
      (insert ".today { background-color: #4ecdc4; color: white; }\n")
      (insert ".calendar { background-color: #fff9e6; }\n")
      (insert "tr:nth-child(even) { background-color: #fafafa; }\n")
      (insert "tr:hover { background-color: #f5f5f5; }\n")
      (insert "a { color: #0066cc; text-decoration: none; font-size: 32px; }\n")
      (insert "a:hover { text-decoration: underline; }\n")
      (insert ".task-date { color: #666; font-size: 28px; }\n")
      (insert ".task-text { font-size: 32px; }\n")
      (insert ".today td { padding: 8px 12px; line-height: 1.2; }\n")  ;; Reduced line height and padding
      (insert ".today tr { margin: 0; border-spacing: 0; }\n")  ;; Remove spacing between rows
      (insert ".today table { border-spacing: 0; border-collapse: collapse; }\n")  ;; Ensure no spacing in table
      (insert "</style>\n")
      (insert "</head>\n<body>\n")
      
      ;; Due Today table
      (insert "<h2>Due Today</h2>\n")
      (insert "<table>\n")
      (dolist (task due-today-tasks)
        (insert "<tr class=\"due-today\"><td class=\"task-text\">")
        (insert (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task))
        (insert "</td></tr>\n"))
      (insert "</table>\n")
      
      ;; Today tasks table
      (insert "<h2>Today</h2>\n")
      (insert "<table class=\"today\">\n")  ;; Add class to table
      (dolist (task today-tasks)
        (insert "<tr class=\"today\"><td class=\"task-text\">")
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (task-manager-make-urls-clickable task-text)))
        (insert "</td></tr>\n"))
      (insert "</table>\n")
      
      ;; Calendar tasks table
      (insert "<h2>Calendar</h2>\n")
      (insert "<table>\n")
      (dolist (task calendar-tasks)
        (let* ((date (task-manager-parse-date-from-task task))
               (task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task))
               (task-text (replace-regexp-in-string "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" "" task-text))
               (task-text (replace-regexp-in-string "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" "" task-text))
               (task-text (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" "" task-text))
               (task-text (string-trim task-text))
               (formatted-date (when date
                                (let ((time (encode-time 0 0 0 
                                                         (string-to-number (substring date 8 10))
                                                         (string-to-number (substring date 5 7))
                                                         (string-to-number (substring date 0 4)))))
                                  (format-time-string "%A, %d/%m %Y" time)))))
          (insert "<tr class=\"calendar\"><td class=\"task-date\">")
          (insert (or formatted-date ""))
          (insert "</td><td class=\"task-text\">")
          (insert (task-manager-make-urls-clickable task-text))
          (insert "</td></tr>\n")))
      (insert "</table>\n")
      
      (insert "</body>\n</html>")
      
      ;; Save HTML to file silently
      (write-region (point-min) (point-max) report-file nil t))
    
    ;; Generate Markdown
    (with-temp-buffer
      (insert "# Task Report\n\n")
      
      ;; Due Today section
      (insert "## Due Today\n\n")
      (dolist (task due-today-tasks)
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (format "- %s\n" task-text))))
      (insert "\n")
      
      ;; Today section
      (insert "## Today\n\n")
      (dolist (task today-tasks)
        (let ((task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task)))
          (insert (format "- %s\n" task-text))))
      (insert "\n")
      
      ;; Calendar section
      (insert "## Calendar\n\n")
      (dolist (task calendar-tasks)
        (let* ((date (task-manager-parse-date-from-task task))
               (task-text (replace-regexp-in-string "\\[\\(Due\\|Priority\\|Recurring\\|Tags\\|Reminder\\): [^]]*\\]" "" task))
               (task-text (replace-regexp-in-string "\\[Due: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\]" "" task-text))
               (task-text (replace-regexp-in-string "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\}>" "" task-text))
               (task-text (replace-regexp-in-string "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" "" task-text))
               (task-text (string-trim task-text))
               (formatted-date (when date
                                (let ((time (encode-time 0 0 0 
                                                       (string-to-number (substring date 8 10))
                                                       (string-to-number (substring date 5 7))
                                                       (string-to-number (substring date 0 4)))))
                                  (format-time-string "%A, %d/%m %Y" time)))))
          (insert (format "### %s\n\n" (or formatted-date "")))
          (insert (format "- %s\n\n" task-text))))
      
      ;; Save Markdown to file silently
      (write-region (point-min) (point-max) markdown-file nil t))
    
    ;; Schedule next report
    (task-manager-schedule-auto-report)))

;; Schedule the first automatic report when the file is loaded
(add-hook 'after-init-hook 'task-manager-schedule-auto-report)

;; Add this function after task-manager-parse-date-from-task

(defun task-manager-sort-tasks-by-due-date (tasks)
  "Sort TASKS by their due dates. Tasks without due dates are placed at the end."
  (sort tasks
        (lambda (a b)
          (let ((date-a (task-manager-parse-date-from-task a))
                (date-b (task-manager-parse-date-from-task b)))
            (cond
             ;; If both tasks have dates, compare them
             ((and date-a date-b)
              (string< date-a date-b))
             ;; If only first task has date, it comes first
             (date-a t)
             ;; If only second task has date, it comes first
             (date-b nil)
             ;; If neither has date, maintain original order
             (t nil))))))

;; Utility: Refresh while keeping cursor fixed in window
(defun task-manager-refresh-keep-cursor ()
  "Refresh the display of tasks, keeping the cursor at the same visual position in the window."
  (let* ((win (selected-window))
         (orig-point (point))
         (orig-window-start (window-start win))
         (lines-from-top (count-screen-lines orig-window-start orig-point)))
    (task-manager-refresh)
    (goto-char orig-point)
    (let ((new-window-start (save-excursion
                              (goto-char (point-min))
                              (forward-line (max 0 (- (line-number-at-pos (point)) lines-from-top)))
                              (point))))
      (set-window-start win new-window-start))))

(provide 'task-manager2)

;;; task-manager2.el ends here

