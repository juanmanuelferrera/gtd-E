https://github.com/user-attachments/assets/a0de2d64-24c0-4ccc-bdc8-683664a985bb

# GTD + Emacs · Task Manager

A powerful, self-contained task management application for Emacs that organizes tasks into six distinct sections: Inbox, Today, Monday, Month, Calendar, and Someday. This task manager is based on the GTD+R (Getting Things Done + Rodhia) methodology by Kenji Ohta, https://gtd-r.blogspot.com/ which combines David Allen's GTD principles with a structured review system.

## GTD+R Methodology

The task manager implements the GTD+R methodology, which consists of three core principles:

## Daily Routine
At the start of each day:
- Open the Today and Week section and select tasks to focus on Today.
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

The task manager automatically helps with this workflow through:
- Automatic migration of Today tasks to Week at 3 AM
- Regular backups to prevent data loss
- Easy movement between sections
- Quick task entry and editing
- Flexible organization with tags and due dates

## Features

- **Six Dedicated Sections**: Organize tasks into Inbox, Today, Monday, Month, Calendar, and Someday
- **Task Entry**: Add tasks to any section with a simple key command
- **Task Movement**: Move tasks between sections (individual or batch)
- **Date/Time Assignment**: Set due dates for Calendar section tasks
- **Bulk Actions**: Select all tasks in Inbox/Week for movement
- **Multi-select**: Delete, complete, or move multiple tasks at once
- **Task Status Tracking**: Mark tasks as complete/incomplete
- **Recurring Tasks**: Set tasks to repeat daily, weekly, or monthly
- **Tags**: Add and manage tags for better organization
- **Reminders**: Set reminders for important tasks
- **Search**: Find tasks across all sections
- **Filtering**: Filter tasks by properties like priority, due date, or tags
- **Import/Export**: Import tasks from org-agenda or export to various formats
- **Automatic Backups**: Scheduled backups to prevent data loss
- **Undo System**: Undo up to 15 operations

## Installation

1. Clone this repository or download the `task-manager2.el` file
2. Add the following to your `.emacs` or `init.el`:

```elisp
(add-to-list 'load-path "/path/to/task-manager")
(require 'task-manager2)
```

3. Restart Emacs or evaluate the above code

## Usage

Start the task manager with:

```
M-x task-manager2-init
```

### Key Commands

#### Task Management
- `a i`: Add task to Inbox
- `a t`: Add task to Today
- `a w`: Add task to Week
- `a m`: Add task to Monday
- `a s`: Add task to Someday
- `a c`: Add task to Calendar
- `A i`: Add multiple tasks to Inbox
- `A t`: Add multiple tasks to Today
- `A w`: Add multiple tasks to Week
- `A m`: Add multiple tasks to Monday
- `A s`: Add multiple tasks to Someday
- `A c`: Add multiple tasks to Calendar
- `RET`: Edit task at current position
- `l`: Duplicate task at cursor
- `k`: Delete selected tasks (move to Archive)
- `K`: Delete all tasks in a section

#### Navigation
- `o i`: Open Inbox section
- `o t`: Open Today section
- `o w`: Open Week section
- `o m`: Open Monday section
- `o s`: Open Someday section
- `o c`: Open Calendar section
- `o a`: Open Archive section
- `n`: Move to next task
- `p`: Move to previous task

#### Task Properties
- `d`: Set due date
- `D`: Clear due date
- `r`: Set recurring task
- `R`: Clear recurring status
- `T`: Add/delete tags
- `X`: Set reminders

#### Task Movement
- `m`: Move selected tasks to Monday
- `w`: Move task to Week
- `W`: Move all Inbox and Today tasks to Week
- `s`: Move task to Someday
- `i`: Move task to Inbox
- `t`: Move task to Today
- `c`: Move task to Calendar

#### Other Functions
- `z`: Collapse all sections
- `C`: Toggle commands visibility
- `f`: Filter tasks by properties
- `S`: Search tasks
- `v`: View all recurring tasks
- `b`: Bulk edit tasks
- `E`: Export tasks
- `I`: Import tasks
- `O`: Import tasks from org-agenda
- `B`: Create manual backup
- `u`: Undo
- `SPC`: Toggle task selection

## Automatic Features

- **Daily Migration**: Tasks in the Today section are automatically moved to the Week section at 3 AM
- **Backup System**: Automatic backups every 2 hours, with a maximum of 3 backup files retained

## Customization

You can customize the task manager by modifying the following variables:

- `task-manager-save-file`: Location where tasks are saved
- `task-manager-backup-directory`: Directory for backup files
- `task-manager-backup-interval`: Time between automatic backups
- `task-manager-max-backups`: Maximum number of backup files to keep


## Backup and Data Persistence
The task manager supports automatic backups every 2 hours. You can also create backups manually with the `B` command while in the task manager buffer.

## License
This project is licensed under the MIT License.

## Contribution
Contributions are welcome! Please open issues or pull requests if you'd like to improve this package.

## Acknowledgments
Thanks to all contributors and the Emacs community for their continuous support and contributions to open-source tools.
