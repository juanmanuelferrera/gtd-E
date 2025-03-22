# GTD + Emacs · Task Manager

## Overview
`task-manager2.el` is an enhanced task management package for Emacs, designed to help you efficiently organize and manage your tasks. It includes a variety of features like sections for Inbox, Today, Week, and more, along with functionalities for searching, setting priorities, reminders, tags, and exporting/importing tasks.

## Features
- Sections: Organized into multiple sections including Inbox, Today, Week, Monday, Calendar, Someday, and Archive.
- Task Management: Add, edit, delete, and archive tasks. Support for single and bulk operations.
- Recurring Tasks: Easily set and manage recurring tasks (daily, weekly, monthly).
- Prioritization: Assign priorities (high, medium, low) to tasks.
- Due Dates: Set and clear due dates for tasks.
- Tags: Add and manage tags for effective categorization.
- Search: Search for tasks across all sections.
- Export/Import: Export tasks to various formats (org, JSON, CSV) and import from these formats.
- Undo History: Supports undo functionality for recent changes.
- Backup: Automatic and manual backup options to prevent data loss.

## Task Sections
- Inbox: Temporary space for new tasks -> `(i)` to focus on section.
- Today: Tasks to complete today -> `(t)` to focus on section.
- Week: Tasks planned for the week -> `(w)` to focus on section.
- Monday: Planning tasks during the weekly review -> `(o)` to focus on section.
- Calendar: Deadlined tasks, sorted by date -> `(c)` to focus on section.
- Someday: Future tasks or ideas -> `(s)` to focus on section.
- Archive: Completed or outdated tasks -> `(F)` to focus on section.

## Daily Routine
At the start of each day:
- Open the Today section and select tasks to focus on.
- Review the Week section to align with weekly goals.
- Adjust tasks between sections as necessary.

## Weekly Review
Conduct a review every Monday:
- Assess progress on tasks in Week and Today sections.
- Evaluate Monday section tasks for movement to Week or Today.
- Adjust accordingly.

## Monthly Review
On the first of each month:
- Inspect Someday section tasks.
- Sort relevant tasks into Today, Week, or Monday sections, or leave in Someday.
- Archive tasks that can be closed.

## Key Commands
- a: Add a single task
- A: Add multiple tasks
- k: Delete selected tasks (move to Archive)
- K: Delete all tasks in a section
- m: Move selected tasks to another section
- RET: Edit task at current position
- S: Search tasks
- r: Set task as recurring 
- d: Set due date
- T: Add tags
- x: Export tasks
- I: Import tasks
- u: Undo last operation
- B: Manual backup

## Installation
To install `task-manager2.el`, follow these steps:

1. Download the file `task-manager2.el` to your Emacs load path.
2. Add the following to your Emacs configuration (e.g., `init.el`):

   ```elisp
   (require 'task-manager2)
   ```

3. Restart Emacs or evaluate the above line in your configuration.

## Usage
To start the task manager, run:

```elisp
M-x task-manager2-init
```

## Backup and Data Persistence
The task manager supports automatic backups every 2 hours. You can also create backups manually with the `B` command while in the task manager buffer.

## License
This project is licensed under the MIT License.

## Contribution
Contributions are welcome! Please open issues or pull requests if you'd like to improve this package.

## Acknowledgments
Thanks to all contributors and the Emacs community for their continuous support and contributions to open-source tools.
