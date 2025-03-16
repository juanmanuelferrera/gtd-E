
https://github.com/user-attachments/assets/62a2f126-f04b-4247-9136-489ea60d5638

# Gtd-E

# Configuration

1. Adjust the path to your local `task-manager.el`
   
   #+end_srcelisp
   (add-to-list 'load-path "/Users/juanmanuelferreradiaz/.emacs.d/site-lisp/task-manager/")
   (require 'task-manager)
   #+begin_src
   
2. Declare the path to the folder where tasks.org lives for saving tasks
   
   #+end_srcelisp
   (defcustom task-manager-save-file
     (expand-file-name "tasks.org" "/Users/juanmanuelferreradiaz/Library/Mobile Documents/iCloudcomappsonthemove~beorg/Documents/my-gtd/")
     "File where task manager data is saved."
     :type 'file
     :group 'task-manager)
   #+begin_src 

# GTD+E Task Manager Overview

The GTD+E Task Manager is a simple elisp-based system for integrating the Getting Things Done (GTD) methodology with enhanced task organization. It provides sections for different tasks, allowing efficient tracking and prioritizing.

## Task Sections

- Inbox: Temporary space for new tasks -> (i) to focus on section.
- Today: Tasks to complete today -> (t) to focus on section.
- Week: Tasks planned for the week -> (w) to focus on section.
- Monday: Planning tasks during the weekly review -> (o) to focus on section.
- Calendar: Deadlined tasks, sorted by date-> (c) to focus on section.
- Someday: Future tasks or ideas -> (s) to focus on section.
- Archive: Completed or outdated tasks -> (r) to focus on section.

# Daily Routine

At the start of each day:
1. Open the Today section and select tasks to focus on.
2. Review the Week section to align with weekly goals.
3. Adjust tasks between sections as necessary.

# Weekly Review

Conduct a review every Monday:
- Assess progress on tasks in Week and Today sections.
- Evaluate Monday section tasks for movement to Week or Today.
- Adjust accordingly.

# Monthly Review

On the first of each month:
- Inspect Someday section tasks.
- Sort relevant tasks into Today, Week, or Monday sections, or leave in Someday.
- Archive tasks that can be closed.

# Task Operations

- Add Tasks: Use keybinding `a` to add to Inbox or directly to sections.
- Edit Tasks: Click on a task to edit inline.
- Delete Tasks: Use `k` to delete selected tasks.
- Move Tasks: Use `m` to move to another section.
- Focus: Collapse other sections to focus on one.

# Calendar Functionality

The calendar feature enhances time-sensitive task management.

## Key Features

- Task Date Insertion: Use `C-c d` to bring up a calendar for quick date selection.
- Automatic Calendar Section: Tasks with dates appear in "Calendar Tasks" at the top.
- Sorting by Date: Calendar section tasks are sorted by due dates.
- Daily Review: Check Calendar section for tasks due today.

## Using Calendar Functionality

- Navigate with arrow keys, select a date with `RET`, or cancel with `q`.
- Regularly check the Calendar section for updates.

# Conclusion

The calendar functionality streamlines task management, ensuring visibility of due tasks for effective prioritization and deadline tracking.

# Task Management Key Bindings

- `a`: Add a single task.
- `A`: Add multiple tasks.
- `k`: Delete selected tasks.
- `m`: Move selected tasks.
- `W`: Move all tasks from Inbox and Today to Week.
- `D`: Delete all tasks with confirmation.
- `z`: Expand/collapse all sections.
- `e`: Toggle section expansion.
- `n` / `p`: Move to next/previous task.
- `RET`: Edit the task under the cursor.

# Final Remark

This software is based on GTD+R paper system by Kenji Ohta:
https://gtd-r.blogspot.com/

https://github.com/user-attachments/assets/6ab7d15c-89c4-46ce-a318-6e5169e6b687

