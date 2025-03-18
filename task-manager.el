;;; task-manager.el --- Simple task manager with sections -*- lexical-binding: t -*-

;; Author: Assistant
;; Keywords: task management, todo
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (org "9.3"))

;;; Commentary:
;; A simple task manager with sections for Inbox, Today, Week, Monday, Calendar, Someday, and Archive
;; Use 'a' to add tasks, 'd' to delete, 'm' to move, and 'e' to expand/collapse sections

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'calendar)
(require 'autorevert)
(require 'org)

(defvar task-manager-sections
  '("Inbox" "Today" "Week" "Monday" "Calendar" "Someday" "Archive")
  "List of available sections.")

(defvar task-manager-tasks (make-hash-table :test 'equal)
  "Hash table storing tasks for each section.")

(defvar-local task-manager-selected-tasks nil
  "List of currently selected tasks.")

(defvar-local task-manager-expanded-sections (make-hash-table :test 'equal)
  "Hash table tracking expanded/collapsed state of sections.")

(defvar-local task-manager-all-expanded t
  "Flag indicating if all sections are expanded.")

(defvar-local task-manager-edit-mode nil
  "Flag indicating if edit mode is active.")

(defvar-local task-manager-move-source nil
  "Temporarily stores the source section during a move-all operation.")

(defvar task-manager-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define all keybindings
    (define-key map (kbd "a") 'task-manager-add-task)
    (define-key map (kbd "A") 'task-manager-add-multiple-tasks)
    (define-key map (kbd "k") 'task-manager-delete-tasks)
    (define-key map (kbd "K") 'task-manager-delete-all-tasks-in-section)
    (define-key map (kbd "D") 'task-manager-delete-all-tasks)
    (define-key map (kbd "m") 'task-manager-move-tasks)
    (define-key map (kbd "M") 'task-manager-move-all-tasks-in-section)
    (define-key map (kbd "e") 'task-manager-toggle-section)
    (define-key map (kbd "z") 'task-manager-toggle-all-sections)
    (define-key map (kbd "f") 'task-manager-toggle-focus)
    (define-key map (kbd "d") 'task-manager-toggle-edit-mode)
    (define-key map (kbd "x") 'task-manager-archive-tasks)
    (define-key map (kbd "n") 'task-manager-next-task)
    (define-key map (kbd "p") 'task-manager-previous-task)
    (define-key map (kbd "SPC") 'task-manager-toggle-task)
    (define-key map (kbd "RET") 'task-manager-edit-task)
    (define-key map (kbd "* s") 'task-manager-toggle-all-tasks-in-section)
    (define-key map (kbd "* a") 'task-manager-toggle-all-tasks)
    (define-key map (kbd "C-c d") 'task-manager-insert-date-from-calendar)
    (define-key map (kbd "C-k") 'task-manager-permanently-delete-tasks)
    (define-key map (kbd "X") 'task-manager-empty-recycle-bin)
    (define-key map (kbd "W") 'task-manager-move-to-week)
    (define-key map (kbd "i") 'task-manager-focus-inbox)
    (define-key map (kbd "t") 'task-manager-focus-today)
    (define-key map (kbd "w") 'task-manager-focus-week)
    (define-key map (kbd "o") 'task-manager-focus-monday)
    (define-key map (kbd "c") 'task-manager-focus-calendar)
    (define-key map (kbd "s") 'task-manager-focus-someday)
    (define-key map (kbd "r") 'task-manager-focus-archive)
    map)
  "Keymap for `task-manager-mode'.")

(define-derived-mode task-manager-mode special-mode "Task Manager"
  "Major mode for task management."
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defvar task-manager-section-mapping
  '(("Hoy" . "Today")
    ("Semana" . "Week")
    ("Lunes" . "Monday")
    ("Calendario" . "Calendar")
    ("Algun Dia" . "Someday")
    ("Archivo" . "Archive"))
  "Mapping of old section names to new ones.")

(defvar task-manager-date-format "%Y-%m-%d"
  "Format for dates in tasks.")

(defvar task-manager-task-cache nil
  "Cache of recently added or modified tasks.")

(defvar task-manager-last-save-time nil
  "Timestamp of the last save operation.")

(defun task-manager-migrate-sections ()
  "Migrate tasks from old section names to new ones."
  (let ((migrated-tasks (make-hash-table :test 'equal)))
    ;; First, initialize all new sections with empty lists
    (dolist (section task-manager-sections)
      (puthash section nil migrated-tasks))
    
    ;; Migrate tasks from old sections to new ones
    (maphash
     (lambda (section tasks)
       (when tasks  ; Only process sections with tasks
         (let ((new-section
                (or (cdr (assoc section task-manager-section-mapping))
                    (and (member section task-manager-sections) section))))
           (when new-section  ; If we have a valid target section
             (puthash new-section
                      (append tasks (gethash new-section migrated-tasks))
                      migrated-tasks)))))
     task-manager-tasks)
    
    ;; Replace the old task hash with the migrated one
    (setq task-manager-tasks migrated-tasks)
    (task-manager-save-data)))

(defcustom task-manager-save-file 
  (expand-file-name "tasks.org" "/Users/juanmanuelferreradiaz/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/")
  "File where task manager data is saved."
  :type 'file
  :group 'task-manager)

;;;###autoload
(defun task-manager-init ()
  "Initialize the task manager."
  (interactive)
  (let ((buffer (get-buffer-create "*Task Manager*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'task-manager-mode)
        (task-manager-mode))
      (setq buffer-read-only t)
      ;; Initialize all sections with empty lists
      (clrhash task-manager-tasks)
      (dolist (section task-manager-sections)
        (puthash section nil task-manager-tasks))
      ;; Load existing tasks
      (task-manager-load-data)
      ;; Initialize expanded state for all sections
      (dolist (section task-manager-sections)
        (puthash section t task-manager-expanded-sections))
      ;; Set up auto-save
      (add-hook 'kill-buffer-hook #'task-manager-ensure-save nil t)
      ;; Add hook to save tasks when Emacs is closing
      (add-hook 'kill-emacs-hook #'task-manager-save-data)
      (task-manager-refresh))
    (switch-to-buffer buffer)))

(defun task-manager-ensure-save ()
  "Ensure tasks are saved before killing the buffer."
  (when (eq major-mode 'task-manager-mode)
    (task-manager-save-data)))

(defun task-manager-save-data ()
  "Save task manager data to org file."
  (interactive)
  (make-directory (file-name-directory task-manager-save-file) t)
  (with-temp-buffer
    ;; Enable org-mode for proper handling of org syntax
    (org-mode)
    (insert "#+TITLE: GTD Tasks\n")
    (insert "#+STARTUP: overview\n\n")
    ;; Save each section and its tasks
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks)))
        (when tasks  ; Only write sections that have tasks
          (insert (format "* %s\n" section))
          (dolist (task tasks)
            (insert (format "** TODO %s\n" task)))
          (insert "\n"))))
    ;; Save buffer to file
    (write-region (point-min) (point-max) task-manager-save-file))
  ;; Update last save time
  (setq task-manager-last-save-time (current-time))
  (message "Tasks saved to %s" task-manager-save-file))

(defun task-manager-load-data ()
  "Load task manager data from org file."
  (interactive)
  ;; Initialize empty lists for all sections
  (clrhash task-manager-tasks)
  (dolist (section task-manager-sections)
    (puthash section nil task-manager-tasks))
  
  (when (file-exists-p task-manager-save-file)
    ;; Read and parse org file
    (with-temp-buffer
      ;; Enable org-mode for proper handling of org syntax
      (org-mode)
      (insert-file-contents task-manager-save-file)
      (goto-char (point-min))
      (let (current-section)
        (while (not (eobp))
          (cond
           ;; Section header
           ((looking-at "^\\* \\(.+\\)$")
            (setq current-section (match-string 1)))
           ;; Task line (TODO state)
           ((and current-section
                 (looking-at "^\\*\\* TODO \\(.+\\)$"))
            (let ((task (match-string 1)))
              (when (and task (not (string-empty-p (string-trim task))))
                (push task (gethash current-section task-manager-tasks))))))
          (forward-line))))
    (message "Tasks loaded from %s" task-manager-save-file)))

(defun task-manager-clean-empty-tasks ()
  "Remove all empty or whitespace-only tasks from all sections."
  (dolist (section task-manager-sections)
    (let ((tasks (gethash section task-manager-tasks)))
      (setf (gethash section task-manager-tasks)
            (cl-remove-if (lambda (task)
                           (or (null task)
                               (string-empty-p (string-trim task))))
                         tasks)))))

(defun task-manager-get-todays-tasks ()
  "Get all tasks that contain today's date."
  (let* ((today (format-time-string "%Y-%m-%d"))
         (todays-tasks nil))
    (dolist (section task-manager-sections)
      (let ((tasks (gethash section task-manager-tasks)))
        (dolist (task tasks)
          (when (string-match-p today task)
            (push (cons section task) todays-tasks)))))
    (nreverse todays-tasks)))

(defun task-manager-parse-date-from-task (task)
  "Extract date from TASK string in format YYYY-MM-DD or <YYYY-MM-DD Day>."
  (let ((date nil))
    ;; Try to match <YYYY-MM-DD Day> format
    (if (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) [A-Za-z]\\{3\\}>" task)
        (setq date (match-string 1 task))
      ;; Try to match YYYY-MM-DD format
      (when (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" task)
        (setq date (match-string 1 task))))
    date))

(defun task-manager-date-to-days (date-str)
  "Convert DATE-STR in format YYYY-MM-DD to days since epoch."
  (when date-str
    (let ((year (string-to-number (substring date-str 0 4)))
          (month (string-to-number (substring date-str 5 7)))
          (day (string-to-number (substring date-str 8 10))))
      (calendar-absolute-from-gregorian (list month day year)))))

(defun task-manager-sort-tasks-by-date (tasks)
  "Sort TASKS by date, with dates closer to today appearing first."
  (let* ((today (calendar-absolute-from-gregorian 
                (calendar-current-date)))
         (tasks-with-dates
          (mapcar (lambda (task)
                    (let* ((date-str (task-manager-parse-date-from-task task))
                           (days (task-manager-date-to-days date-str))
                           (distance (if days (abs (- days today)) 999999)))
                      (list task date-str distance)))
                  tasks)))
    ;; Sort by distance from today
    (setq tasks-with-dates 
          (sort tasks-with-dates 
                (lambda (a b) 
                  (< (nth 2 a) (nth 2 b)))))
    ;; Extract just the tasks
    (mapcar #'car tasks-with-dates)))

(defun task-manager-safe-recenter ()
  "Recenter the window displaying the task manager buffer."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (when windows
      (with-selected-window (car windows)
        (recenter)))))

(defun task-manager-refresh ()
  "Refresh the task manager display."
  (task-manager-clean-empty-tasks)
  (let ((inhibit-read-only t)
        (current-line (line-number-at-pos))
        (current-column (current-column)))
    (erase-buffer)
    (insert "Task Manager")
    (when task-manager-edit-mode
      (insert " [EDIT MODE]"))
    (when task-manager-move-source
      (insert (format " [Moving from: %s]" task-manager-move-source)))
    (insert "\n============\n\n")
    
    ;; Sort Calendar tasks by date
    (let ((calendar-tasks (gethash "Calendar" task-manager-tasks)))
      (when calendar-tasks
        (setf (gethash "Calendar" task-manager-tasks)
              (task-manager-sort-tasks-by-date calendar-tasks))))
    
    ;; Display today's tasks at the top if any exist
    (let ((todays-tasks (task-manager-get-todays-tasks)))
      (when todays-tasks
        (insert "Calendar Tasks:\n")
        (dolist (task-pair todays-tasks)
          (let* ((section (car task-pair))
                 (task (cdr task-pair))
                 (selected (member task task-manager-selected-tasks)))
            (insert (format "  [%s] " (if selected "*" " ")))
            ;; Make task text clickable for editing with proper link handling
            (let ((start 0)
                  (button-start (point)))
              (while (string-match "\\(https?://[^\s\n]+\\)" task start)
                (let ((link-start (match-beginning 1))
                      (link-end (match-end 1))
                      (link (match-string 1 task)))
                  ;; Make text before link clickable for editing
                  (let ((text-before (substring task start link-start)))
                    (unless (string-empty-p text-before)
                      (insert-text-button text-before
                                        'face (if selected 'bold 'default)
                                        'follow-link t
                                        'task task
                                        'section section
                                        'action #'task-manager-edit-task-inline
                                        'help-echo "Click to edit task")))
                  ;; Insert clickable link
                  (insert-text-button link
                                    'face 'link
                                    'follow-link t
                                    'action (lambda (_) 
                                            (browse-url link))
                                    'help-echo "Click to open link")
                  (setq start link-end)))
              ;; Make remaining text clickable for editing
              (let ((remaining-text (substring task start)))
                (unless (string-empty-p remaining-text)
                  (insert-text-button remaining-text
                                    'face (if selected 'bold 'default)
                                    'follow-link t
                                    'task task
                                    'section section
                                    'action #'task-manager-edit-task-inline
                                    'help-echo "Click to edit task"))))
            (insert (format " (in %s)\n" section))))
        (insert "\n============\n\n")))
    
    ;; Display all sections
    (dolist (section task-manager-sections)
      (task-manager-insert-section section))
    
    ;; Display commands
    (insert "\nCommands:\n")
    (insert "  a: Add a single task (C-c d during input to insert date)\n")
    (insert "  A: Add multiple tasks (one per line)\n")
    (insert "  RET/Click: Edit task under cursor\n")
    (insert "  SPC: Select/deselect task and move to next\n")
    (insert "  * s: Select all tasks in current section\n")
    (insert "  * a: Select all tasks across all sections\n")
    (insert "  m: Move selected task(s) to another section\n")
    (insert "  M: Move all tasks from one section to another\n")
    (insert "  W: Move all tasks from Inbox and Today to Week\n")
    (insert "  k: Delete selected task(s)\n")
    (insert "  K: Move all tasks in section to Archive\n")
    (insert "  x: Move selected task(s) to Archive\n")
    (insert "  D: Delete ALL tasks from ALL sections (with confirmation)\n")
    (insert "  n/p: Move to next/previous task\n")
    (insert "  e: Expand/collapse section\n")
    (insert "  z: Expand/collapse all sections\n")
    
    ;; Restore cursor position
    (goto-char (point-min))
    (forward-line (1- current-line))
    (move-to-column current-column)
    ;; Center the view on the cursor using the safe function
    (task-manager-safe-recenter)))

(defun task-manager-insert-section (section)
  "Insert SECTION and its tasks."
  (let ((expanded (gethash section task-manager-expanded-sections t))
        (tasks (gethash section task-manager-tasks)))
    (insert (format "%s %s\n"
                    (if expanded "[-]" "[+]")
                    section))
    (when expanded
      (dolist (task tasks)
        (let ((selected (member task task-manager-selected-tasks)))
          (insert (format "  [%s] " (if selected "*" " ")))
          ;; Split task into parts and insert with links
          (let ((start 0)
                (button-start (point)))
            (while (string-match "\\(https?://[^\s\n]+\\)" task start)
              (let ((link-start (match-beginning 1))
                    (link-end (match-end 1))
                    (link (match-string 1 task)))
                ;; Make text before link clickable for editing
                (let ((text-before (substring task start link-start)))
                  (unless (string-empty-p text-before)
                    (insert-text-button text-before
                                      'face (if selected 'bold 'default)
                                      'follow-link t
                                      'task task
                                      'section section
                                      'action #'task-manager-edit-task-inline
                                      'help-echo "Click to edit task")))
                ;; Insert clickable link
                (insert-text-button link
                                  'face 'link
                                  'follow-link t
                                  'action (lambda (_) 
                                          (browse-url link))
                                  'help-echo "Click to open link")
                (setq start link-end)))
            ;; Make remaining text clickable for editing
            (let ((remaining-text (substring task start)))
              (unless (string-empty-p remaining-text)
                (insert-text-button remaining-text
                                  'face (if selected 'bold 'default)
                                  'follow-link t
                                  'task task
                                  'section section
                                  'action #'task-manager-edit-task-inline
                                  'help-echo "Click to edit task"))))
          (insert "\n"))))))

(defun task-manager-insert-task-with-links (task)
  "Insert TASK text with clickable links."
  (let ((start 0))
    (while (string-match "\\(https?://[^\s\n]+\\)" task start)
      (let ((link-start (match-beginning 1))
            (link-end (match-end 1))
            (link (match-string 1 task)))
        ;; Insert text before link
        (insert (substring task start link-start))
        ;; Insert clickable link
        (insert-text-button link
                           'face 'link
                           'follow-link t
                           'action (lambda (_) 
                                    (browse-url link))
                           'help-echo "Click to open link")
        (setq start link-end)))
    ;; Insert remaining text after last link
    (insert (substring task start))))

(defun task-manager-create-input-window (prompt initial-text)
  "Create a centered window for single task input with PROMPT and INITIAL-TEXT."
  (let* ((buf (get-buffer-create "*Task Input*"))
         (window-min-height 3)
         (height 5)
         (width 80)
         (frame-height (frame-height))
         (frame-width (frame-width))
         (window (display-buffer-in-side-window
                 buf
                 `((side . top)
                   (slot . 0)
                   (window-height . ,height)
                   (window-width . ,width)
                   (preserve-size . (t . t))
                   (body-function . ,#'(lambda (window)
                                       (with-selected-window window
                                         (erase-buffer)
                                         (insert prompt "\n\n")
                                         ;; Insert the text and remember cursor position
                                         (let ((input-pos (point)))
                                           (insert initial-text)
                                           (insert "\n\n[RET: save, C-g: cancel]")
                                           ;; Move cursor to end of input text
                                           (goto-char input-pos)
                                           (when initial-text
                                             (forward-char (length initial-text)))))))))))
    window))

(defun task-manager-insert-date-from-calendar ()
  "Insert a date using the calendar."
  (interactive)
  (let ((date nil))
    (calendar)
    (unwind-protect
        (progn
          (calendar-forward-day 0)
          (message "Use arrow keys to move, RET to select date, q to cancel")
          (let ((calendar-move-hook nil)
                (calendar-exit-hook nil))
            (while (not date)
              (let ((key (read-key-sequence nil)))
                (cond
                 ((equal key "\r")  ; RET
                  (let* ((calendar-date (calendar-cursor-to-date))
                         (time (encode-time 0 0 0
                                          (nth 1 calendar-date)  ; day
                                          (nth 0 calendar-date)  ; month
                                          (nth 2 calendar-date)  ; year
                                          )))
                    (setq date (format-time-string task-manager-date-format time))))
                 ((equal key "q")
                  (setq date 'quit))
                 ((equal key [left])
                  (calendar-backward-day 1))
                 ((equal key [right])
                  (calendar-forward-day 1))
                 ((equal key [up])
                  (calendar-backward-week 1))
                 ((equal key [down])
                  (calendar-forward-week 1))
                 ((equal key [prior])  ; PageUp
                  (calendar-backward-month 1))
                 ((equal key [next])   ; PageDown
                  (calendar-forward-month 1))))))
          (unless (eq date 'quit)
            date))
      (calendar-exit))))

(defun task-manager-read-task-with-date (prompt &optional initial-text)
  "Read task input with optional date selection using calendar.
PROMPT is shown to user, INITIAL-TEXT is optional starting text."
  (let* ((window (task-manager-create-input-window 
                 (concat prompt "\nPress C-c d to insert date") 
                 (or initial-text "")))
         (buffer (window-buffer window))
         (done nil)
         result)
    (with-current-buffer buffer
      (let ((map (make-sparse-keymap)))
        ;; Enable basic editing commands
        (use-local-map (make-composed-keymap map (current-global-map)))
        
        (define-key map (kbd "RET")
                    (lambda ()
                      (interactive)
                      (let ((start-pos (save-excursion
                                       (goto-char (point-min))
                                       (forward-line 2)
                                       (point)))
                            (end-pos (save-excursion
                                     (goto-char (point-max))
                                     (search-backward "[RET")
                                     (beginning-of-line)
                                     (point))))
                        (setq result (string-trim
                                    (buffer-substring-no-properties start-pos end-pos))))
                      (setq done t)
                      (exit-recursive-edit)))
        
        (define-key map (kbd "C-c d")
                    (lambda ()
                      (interactive)
                      (let ((date (task-manager-insert-date-from-calendar)))
                        (when date
                          (insert date " ")))))
        
        (define-key map (kbd "C-g")
                    (lambda ()
                      (interactive)
                      (setq done 'quit)
                      (exit-recursive-edit)))))
    
    (unwind-protect
        (progn
          (select-window window)
          (message "Type your task. Press C-c d to insert date, RET when done")
          (while (not done)
            (condition-case nil
                (recursive-edit)
              (quit (setq done 'quit))))
          (unless (eq done 'quit)
            result))
      (when (window-live-p window)
        (delete-window window))
      (kill-buffer buffer))))

(defun task-manager-preserve-point (func &rest args)
  "Execute FUNC with ARGS while preserving point position."
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (apply func args)
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)))

(defun task-manager-cache-task (section task)
  "Add TASK in SECTION to the task cache."
  (push (cons section task) task-manager-task-cache))

(defun task-manager-apply-cached-changes ()
  "Apply cached changes to the current tasks."
  (dolist (cached-task task-manager-task-cache)
    (let ((section (car cached-task))
          (task (cdr cached-task)))
      (unless (member task (gethash section task-manager-tasks))
        (push task (gethash section task-manager-tasks)))))
  (setq task-manager-task-cache nil))

(defun task-manager-add-task (section)
  "Add a task to SECTION."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (task-manager-preserve-point
   (lambda (section)
     (let ((task (string-trim (task-manager-read-task-with-date "Enter task:"))))
       (unless (string-empty-p task)
         ;; Add to cache and current tasks
         (task-manager-cache-task section task)
         (push task (gethash section task-manager-tasks))
         (task-manager-save-data)
         (task-manager-refresh))))
   section))

(defun task-manager-delete-tasks ()
  "Delete Calendar tasks and move other tasks to Archive section."
  (interactive)
  (when task-manager-selected-tasks
    (let ((deleted-count 0)
          (archived-count 0))
      (dolist (task task-manager-selected-tasks)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              ;; Calendar tasks and tasks with today's date are deleted, others are archived
              (if (or (string= section "Calendar")
                      (string-match-p (format-time-string task-manager-date-format) task))
                  (setq deleted-count (1+ deleted-count))
                (let ((archived-task (if (string= section "Archive")
                                       task
                                     (concat task " (from " section ")"))))
                  (push archived-task (gethash "Archive" task-manager-tasks))
                  (setq archived-count (1+ archived-count))))))))
      (setq task-manager-selected-tasks nil)
      (task-manager-save-data)
      (task-manager-refresh)
      (message "%d task%s deleted, %d task%s archived"
               deleted-count (if (= deleted-count 1) "" "s")
               archived-count (if (= archived-count 1) "" "s")))))

(defun task-manager-permanently-delete-tasks ()
  "Permanently delete selected tasks from Recycle Bin."
  (interactive)
  (if (not task-manager-selected-tasks)
      (message "No tasks selected. Select tasks to delete permanently.")
    (let ((tasks-in-recycle-bin 
           (cl-intersection task-manager-selected-tasks 
                           (gethash "Recycle Bin" task-manager-tasks)
                           :test #'string=)))
      (if (not tasks-in-recycle-bin)
          (message "Selected tasks are not in Recycle Bin. Move them there first.")
        (let ((task-count (length tasks-in-recycle-bin)))
          (when (yes-or-no-p 
                 (format "⚠️ PERMANENT DELETION: Delete %d task%s forever? This cannot be undone! "
                         task-count (if (= task-count 1) "" "s")))
            (setf (gethash "Recycle Bin" task-manager-tasks)
                  (cl-remove-if (lambda (task)
                                 (member task tasks-in-recycle-bin))
                               (gethash "Recycle Bin" task-manager-tasks)))
            (setq task-manager-selected-tasks
                  (cl-remove-if (lambda (task)
                                 (member task tasks-in-recycle-bin))
                               task-manager-selected-tasks))
            (task-manager-save-data)
            (task-manager-refresh)
            (message "%d task%s permanently deleted"
                     task-count (if (= task-count 1) "" "s"))))))))

(defun task-manager-move-tasks ()
  "Move selected tasks to another section."
  (interactive)
  (when task-manager-selected-tasks
    (let* ((current-section (save-excursion
                             (while (and (not (looking-at "\\[[-+]\\] \\(.+\\)$"))
                                       (> (point) (point-min)))
                               (forward-line -1))
                             (when (looking-at "\\[[-+]\\] \\(.+\\)$")
                               (match-string 1))))
           (target (completing-read "Move to section: " 
                                  (remove current-section task-manager-sections))))
      (dolist (task task-manager-selected-tasks)
        (dolist (section task-manager-sections)
          (let ((tasks (gethash section task-manager-tasks)))
            (when (member task tasks)
              (setf (gethash section task-manager-tasks)
                    (remove task tasks))
              (push task (gethash target task-manager-tasks))))))
      (setq task-manager-selected-tasks nil)
      (task-manager-refresh))))

(defun task-manager-move-all-tasks-in-section ()
  "Move all tasks from one section to another using dialogs."
  (interactive)
  (let* ((source-section (completing-read "Move all tasks from section: " 
                                        task-manager-sections))
         (target-section (completing-read 
                         (format "Move all tasks from %s to: " source-section)
                         (remove source-section task-manager-sections)))
         (source-tasks (gethash source-section task-manager-tasks)))
    (when (and source-tasks (not (null source-tasks)))
      ;; Move all tasks to target section
      (setf (gethash target-section task-manager-tasks)
            (append source-tasks (gethash target-section task-manager-tasks)))
      ;; Clear source section
      (setf (gethash source-section task-manager-tasks) nil)
      (task-manager-refresh))))

(defun task-manager-toggle-section ()
  "Toggle expansion of selected section."
  (interactive)
  (let ((section (completing-read "Toggle section: " task-manager-sections)))
    (let ((current-state (gethash section task-manager-expanded-sections t)))
      (puthash section (not current-state) task-manager-expanded-sections)
      (task-manager-refresh))))

(defun task-manager-next-task ()
  "Move point to the next task line and center the view."
  (interactive)
  (let ((orig-pos (point))
        (found nil))
    (beginning-of-line)
    (forward-line)
    (while (and (not (eobp))
                (not found))
      (when (looking-at "^  \\[[ *]\\]")
        (setq found t))
      (unless found
        (forward-line)))
    (if found
        (progn
          (move-to-column 2)
          (task-manager-safe-recenter)
          (message "Moved to next task"))
      (goto-char orig-pos)
      (message "No next task found"))))

(defun task-manager-previous-task ()
  "Move point to the previous task line and center the view."
  (interactive)
  (let ((orig-pos (point))
        (found nil))
    (beginning-of-line)
    (forward-line -1)
    (while (and (not (bobp))
                (not found))
      (when (looking-at "^  \\[[ *]\\]")
        (setq found t))
      (unless found
        (forward-line -1)))
    (if found
        (progn
          (move-to-column 2)
          (task-manager-safe-recenter)
          (message "Moved to previous task"))
      (goto-char orig-pos)
      (message "No previous task found"))))

(defun task-manager-toggle-task ()
  "Toggle selection of task at point and move to next task."
  (interactive)
  (let ((was-on-task nil)
        (task-text nil)
        (current-point (point))
        (next-task-pos nil))
    ;; Check if we're on a task line and get the task text
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^  \\[[ *]\\] \\(.+?\\)\\($\\| (in \\|\n\\)")
        (setq was-on-task t
              task-text (match-string-no-properties 1))))
    
    (when (and was-on-task task-text)
      ;; Find the position of the next task before refreshing
      (save-excursion
        (forward-line)
        (while (and (not (eobp))
                    (not next-task-pos))
          (if (looking-at "^  \\[[ *]\\]")
              (setq next-task-pos (point))
            (forward-line))))
      
      ;; Toggle the task selection
      (let ((task (replace-regexp-in-string " (in [^)]+)$" "" task-text)))
        (if (member task task-manager-selected-tasks)
            (setq task-manager-selected-tasks
                  (remove task task-manager-selected-tasks))
          (push task task-manager-selected-tasks)))
      
      ;; Refresh the display
      (task-manager-refresh)
      
      ;; Move to the next task
      (if next-task-pos
          (progn
            (goto-char next-task-pos)
            (move-to-column 2)
            (task-manager-safe-recenter))
        ;; If no next task in current section, try to find first task in next section
        (goto-char current-point)
        (let ((found nil))
          (while (and (not found)
                     (not (eobp)))
            (forward-line)
            (when (looking-at "^  \\[[ *]\\]")
              (setq found t)
              (move-to-column 2)
              (task-manager-safe-recenter)))
          (unless found
            (message "No more tasks")))))))

(defun task-manager-toggle-all-sections ()
  "Toggle expansion state of all sections."
  (interactive)
  (setq task-manager-all-expanded (not task-manager-all-expanded))
  (dolist (section task-manager-sections)
    (puthash section task-manager-all-expanded task-manager-expanded-sections))
  (task-manager-refresh))

(defun task-manager-toggle-edit-mode ()
  "Toggle edit mode for tasks."
  (interactive)
  (setq task-manager-edit-mode (not task-manager-edit-mode))
  (task-manager-refresh))

(defun task-manager-edit-task ()
  "Edit the task at point."
  (interactive)
  (when task-manager-edit-mode
    (save-excursion
      (beginning-of-line)
      (when (looking-at "  \\[[ *]\\] \\(.+?\\)\\( \\[RET to edit\\]\\)?$")
        (let* ((old-task (match-string 1))
               (section (save-excursion
                         (while (not (looking-at "\\[[-+]\\] \\(.+\\)$"))
                           (forward-line -1))
                         (match-string 1)))
               (new-task (string-trim (task-manager-read-task-with-date "Edit task:" old-task))))
          (let ((tasks (gethash section task-manager-tasks)))
            (if (string-empty-p new-task)
                (setf (gethash section task-manager-tasks)
                      (remove old-task tasks))
              (setf (gethash section task-manager-tasks)
                    (cons new-task (remove old-task tasks))))))
        (task-manager-save-data)
        (task-manager-refresh)))))

(defun task-manager-archive-tasks ()
  "Move selected tasks to the Archive section."
  (interactive)
  (when task-manager-selected-tasks
    (dolist (task task-manager-selected-tasks)
      (dolist (section task-manager-sections)
        (let ((tasks (gethash section task-manager-tasks)))
          (when (member task tasks)
            (setf (gethash section task-manager-tasks)
                  (remove task tasks))
            (push task (gethash "Archive" task-manager-tasks))))))
    (setq task-manager-selected-tasks nil)
    (message "Tasks archived")
    (task-manager-refresh)))

(defun task-manager-toggle-all-tasks-in-section (section)
  "Toggle selection of all tasks in SECTION."
  (interactive
   (list (completing-read "Toggle all tasks in section: " task-manager-sections)))
  (let ((tasks (gethash section task-manager-tasks)))
    (if (cl-every (lambda (task) (member task task-manager-selected-tasks)) tasks)
        ;; If all tasks are selected, unselect them
        (setq task-manager-selected-tasks
              (cl-remove-if (lambda (task) (member task tasks))
                           task-manager-selected-tasks))
      ;; Otherwise, select all unselected tasks
      (dolist (task tasks)
        (unless (member task task-manager-selected-tasks)
          (push task task-manager-selected-tasks)))))
  (task-manager-refresh))

(defun task-manager-toggle-all-tasks ()
  "Toggle selection of all tasks in all sections."
  (interactive)
  (let ((all-tasks (cl-loop for section being the hash-keys of task-manager-tasks
                           append (gethash section task-manager-tasks))))
    (if (cl-every (lambda (task) (member task task-manager-selected-tasks)) all-tasks)
        ;; If all tasks are selected, unselect all
        (setq task-manager-selected-tasks nil)
      ;; Otherwise, select all tasks
      (setq task-manager-selected-tasks all-tasks)))
  (task-manager-refresh))

(defun task-manager-delete-all-tasks ()
  "Delete ALL tasks from ALL sections with extra confirmation."
  (interactive)
  (let ((total-tasks 0)
        (sections-with-tasks nil))
    ;; Count total tasks and collect sections with tasks
    (maphash (lambda (section tasks)
               (when (> (length tasks) 0)
                 (push section sections-with-tasks)
                 (setq total-tasks (+ total-tasks (length tasks)))))
             task-manager-tasks)
    
    (when (> total-tasks 0)  ; Only proceed if there are tasks to delete
      ;; First warning
      (when (yes-or-no-p 
             (format "Really delete ALL %d tasks from %d sections? This cannot be undone! " 
                     total-tasks (length sections-with-tasks)))
        ;; Second warning with typing confirmation
        (let ((confirm-text (format "delete-%d-tasks" total-tasks)))
          (when (string= (read-string 
                         (format "Type '%s' to confirm deletion: " confirm-text))
                        confirm-text)
            ;; Final warning with countdown
            (let ((countdown 10))
              (while (> countdown 0)
                (message "Deleting all tasks in %d seconds... Press C-g to cancel" 
                         countdown)
                (sit-for 1)
                (setq countdown (1- countdown)))
              ;; Perform deletion
              (dolist (section task-manager-sections)
                (puthash section nil task-manager-tasks))
              (setq task-manager-selected-tasks nil)
              (message "All tasks have been deleted from %d sections" (length sections-with-tasks))
              (task-manager-refresh))))))))

(defun task-manager-create-multi-input-window (prompt)
  "Create a centered window for multiple task input with PROMPT."
  (let* ((buf (get-buffer-create "*Task Input*"))
         (window-min-height 3)
         (height 15)
         (width 80)
         (frame-height (frame-height))
         (frame-width (frame-width))
         (window (display-buffer-in-side-window
                 buf
                 `((side . top)
                   (slot . 0)
                   (window-height . ,height)
                   (window-width . ,width)
                   (preserve-size . (t . t))
                   (body-function . ,#'(lambda (window)
                                       (with-selected-window window
                                         (erase-buffer)
                                         (insert prompt "\n\n")
                                         (insert "\n\n[C-c C-c: save, C-g: cancel, C-c d: insert date]")
                                         ;; Move cursor to input position
                                         (goto-char (point-min))
                                         (forward-line 2))))))))
    window))

(defun task-manager-read-multiple-tasks (prompt)
  "Read multiple tasks, one per line, in a centered window with PROMPT."
  (let* ((window (task-manager-create-multi-input-window prompt))
         (buffer (window-buffer window))
         (done nil)
         result)
    (with-current-buffer buffer
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c")
                    (lambda ()
                      (interactive)
                      (let ((start-pos (save-excursion
                                       (goto-char (point-min))
                                       (forward-line 2)
                                       (point)))
                        (setq result
                              (split-string
                               (string-trim (buffer-substring-no-properties start-pos (point-max)))
                               "\n" t "[ \t\n\r]+")))
                      (setq done t)
                      (exit-recursive-edit)))
        
        (define-key map (kbd "C-c d")
                    (lambda ()
                      (interactive)
                      (let ((date (task-manager-insert-date-from-calendar)))
                        (when date
                          (insert date " ")))))
        
        (define-key map (kbd "RET") 'newline)  ; Allow normal newlines
        (define-key map (kbd "C-g")
                    (lambda ()
                      (interactive)
                      (setq done 'quit)
                      (exit-recursive-edit)))
        (use-local-map map)))
    
    (unwind-protect
        (progn
          (select-window window)
          (message "Enter tasks (one per line). Press C-c d to insert date, C-c C-c when done")
          (while (not done)
            (condition-case nil
                (recursive-edit)
              (quit (setq done 'quit))))
          (unless (eq done 'quit)
            result))
      (when (window-live-p window)
        (delete-window window))
      (kill-buffer buffer))))

(defun task-manager-add-multiple-tasks (section)
  "Add multiple tasks to SECTION, with support for date insertion."
  (interactive
   (list (completing-read "Section: " task-manager-sections)))
  (let ((tasks (task-manager-read-multiple-tasks 
                "Enter tasks (one per line)\nUse C-c d to insert date at cursor position")))
    (when tasks  ; Only proceed if we got tasks (not cancelled)
      (dolist (task tasks)
        (unless (string-empty-p task)
          (push task (gethash section task-manager-tasks))))
      (task-manager-save-data)
      (task-manager-refresh)
      (message "%d tasks added to %s" (length tasks) section))))

(defun task-manager-empty-recycle-bin ()
  "Delete all tasks from the Recycle Bin."
  (interactive)
  (let ((recycle-bin-tasks (gethash "Recycle Bin" task-manager-tasks)))
    (when (and recycle-bin-tasks
               (yes-or-no-p (format "Permanently delete all %d tasks in Recycle Bin? This cannot be undone! "
                                   (length recycle-bin-tasks))))
      (setf (gethash "Recycle Bin" task-manager-tasks) nil)
      (task-manager-save-data)
      (task-manager-refresh)
      (message "Recycle Bin emptied"))))

(defun task-manager-delete-all-tasks-in-section ()
  "Move all tasks in a section to Archive."
  (interactive)
  (let* ((section (completing-read "Move all tasks from section: " task-manager-sections))
         (tasks (gethash section task-manager-tasks))
         (archived-count 0))
    (when (and tasks
               (yes-or-no-p (format "Move all %d tasks from %s to Archive? " 
                                   (length tasks) section)))
      ;; Move tasks to Archive with source section info
      (unless (string= section "Archive")
        (dolist (task tasks)
          (let ((archived-task (concat task " (from " section ")")))
            (push archived-task (gethash "Archive" task-manager-tasks))
            (setq archived-count (1+ archived-count)))))
      (setf (gethash section task-manager-tasks) nil)
      (task-manager-save-data)
      (task-manager-refresh)
      (message "All tasks from %s moved to Archive" section))))

(defun task-manager-edit-task-inline (button)
  "Edit task inline when button is clicked."
  (let* ((old-task (button-get button 'task))
         (section (button-get button 'section)))
    
    ;; Use the proven task-manager-read-task-with-date function
    (let ((new-task (task-manager-read-task-with-date "Edit task:" old-task)))
      (when (and new-task (not (string-empty-p new-task)))
        ;; Remove old task
        (let ((tasks (gethash section task-manager-tasks)))
          (setf (gethash section task-manager-tasks)
                (remove old-task tasks)))
        
        ;; Add new task
        (push new-task (gethash section task-manager-tasks))
        
        ;; Update selected tasks if needed
        (when (member old-task task-manager-selected-tasks)
          (setq task-manager-selected-tasks
                (cons new-task (remove old-task task-manager-selected-tasks))))
        
        ;; Save and refresh
        (task-manager-save-data)
        (task-manager-refresh)
        ;; Center the view safely
        (task-manager-safe-recenter)
        (message "Task updated: %s" new-task)))))

(defun task-manager-move-to-week ()
  "Move all tasks from Inbox and Today sections to Week section."
  (interactive)
  (let ((inbox-tasks (gethash "Inbox" task-manager-tasks))
        (today-tasks (gethash "Today" task-manager-tasks))
        (moved-count 0))
    ;; Move tasks from Inbox
    (when inbox-tasks
      (setq moved-count (+ moved-count (length inbox-tasks)))
      (setf (gethash "Week" task-manager-tasks)
            (append inbox-tasks (gethash "Week" task-manager-tasks)))
      (setf (gethash "Inbox" task-manager-tasks) nil))
    
    ;; Move tasks from Today
    (when today-tasks
      (setq moved-count (+ moved-count (length today-tasks)))
      (setf (gethash "Week" task-manager-tasks)
            (append today-tasks (gethash "Week" task-manager-tasks)))
      (setf (gethash "Today" task-manager-tasks) nil))
    
    ;; Save changes and refresh
    (task-manager-save-data)
    (task-manager-refresh)
    (message "%d task%s moved to Week"
             moved-count (if (= moved-count 1) "" "s"))))

(defun task-manager-collapse-except (target-section)
  "Collapse all sections except TARGET-SECTION."
  (dolist (section task-manager-sections)
    (puthash section (string= section target-section) task-manager-expanded-sections)))

(defun task-manager-focus-inbox ()
  "Focus on the Inbox section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Inbox
        (task-manager-collapse-except "Inbox")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Inbox section
        (goto-char (point-min))
        (if (search-forward "[-] Inbox" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Inbox"))
          (message "Inbox section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-today ()
  "Focus on the Today section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Today
        (task-manager-collapse-except "Today")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Today section
        (goto-char (point-min))
        (if (search-forward "[-] Today" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Today"))
          (message "Today section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-week ()
  "Focus on the Week section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Week
        (task-manager-collapse-except "Week")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Week section
        (goto-char (point-min))
        (if (search-forward "[-] Week" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Week"))
          (message "Week section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-monday ()
  "Focus on the Monday section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Monday
        (task-manager-collapse-except "Monday")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Monday section
        (goto-char (point-min))
        (if (search-forward "[-] Monday" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Monday"))
          (message "Monday section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-calendar ()
  "Focus on the Calendar section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Calendar
        (task-manager-collapse-except "Calendar")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Calendar section
        (goto-char (point-min))
        (if (search-forward "[-] Calendar" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Calendar"))
          (message "Calendar section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-someday ()
  "Focus on the Someday section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Someday
        (task-manager-collapse-except "Someday")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Someday section
        (goto-char (point-min))
        (if (search-forward "[-] Someday" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Someday"))
          (message "Someday section not found")))
    (message "Could not locate sections")))

(defun task-manager-focus-archive ()
  "Focus on the Archive section and collapse others."
  (interactive)
  (goto-char (point-min))
  (if (search-forward "[" nil t)
      (progn
        ;; Collapse all sections except Archive
        (task-manager-collapse-except "Archive")
        ;; Refresh to ensure expansion state is applied
        (task-manager-refresh)
        ;; Find and move to the Archive section
        (goto-char (point-min))
        (if (search-forward "[-] Archive" nil t)
            (progn
              (forward-line 1)
              (task-manager-safe-recenter)
              (message "Focused on Archive"))
          (message "Archive section not found")))
    (message "Could not locate sections")))

(provide 'task-manager)

;;; task-manager.el ends here 
