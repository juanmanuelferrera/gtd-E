https://github.com/user-attachments/assets/a0de2d64-24c0-4ccc-bdc8-683664a985bb

# Task Manager for Emacs

A powerful and intuitive task management system for Emacs, designed to help you organize your tasks across different time horizons and contexts. This implementation is based on the GTD+E method, combining David Allen's Getting Things Done (GTD) methodology with Kenji Ohta's enhanced task organization system.

## The GTD+E Method

The GTD+E method is an enhanced version of GTD that focuses on time-based task organization and prioritization. Here's how it works:

### Core Principles
1. **Capture Everything**: All tasks go into the Inbox first
2. **Clarify & Process**: Tasks are evaluated based on:
   - When they need to be done
   - Their importance and impact
3. **Organize**: Tasks are sorted into appropriate sections:
   - **Today**: Immediate priorities
   - **Week**: Important upcoming tasks
   - **Monday**: Next week's focus
   - **Calendar**: Time-specific commitments
   - **Someday**: Future possibilities
   - **Archive**: Completed tasks

## Features

### Core Functionality
- **Six Dedicated Sections**
  - Inbox (default view) - Capture everything
  - Today - Immediate priorities
  - Week - Important upcoming tasks
  - Monday - Next week's focus
  - Calendar - Time-specific commitments
  - Someday - Future possibilities
  - Archive - Completed tasks

### Task Management
- Quick task entry with automatic cursor focus
- Multi-select functionality for batch operations
- Task movement between sections
- Date/time assignment for Calendar section tasks
- Bulk actions for task management
- Task status tracking (complete/incomplete)

### Advanced Features
- **Search & Filter**
  - Full-text search across all tasks
  - Filter tasks by properties
  - Smart task organization

- **Task Properties**
  - Due dates
  - Priorities
  - Tags
  - Recurring tasks
  - Reminders
  - Notes

- **Data Management**
  - Automatic backups
  - Export/Import functionality
  - Org-mode integration
  - iCloud sync support

## Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/task-manager.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/task-manager")
(require 'task-manager2)
```

## Usage

Start the task manager with:
```
M-x task-manager2-init
```

### Key Commands

#### Basic Operations
- `a`: Add a single task
- `A`: Add multiple tasks
- `k`: Delete selected tasks (move to Archive)
- `K`: Delete all tasks in a section
- `RET`: Edit task at current position
- `SPC`: Toggle task selection

#### Navigation
- `i`: Focus on Inbox section
- `t`: Focus on Today section
- `o`: Focus on Monday section
- `c`: Focus on Calendar section
- `s`: Focus on Someday section
- `F`: Focus on Archive section
- `n`: Move to next task
- `p`: Move to previous task

#### Task Management
- `m`: Move selected tasks to another section
- `w`: Move task to Week section
- `W`: Move all tasks from Inbox and Today to Week
- `d`: Set due date
- `D`: Clear due date
- `T`: Add tags
- `r`: Set task as recurring
- `R`: Clear recurring status
- `X`: Set reminders
- `N`: Manage notes for a task

#### Advanced Features
- `S`: Search tasks
- `f`: Filter tasks by properties
- `b`: Bulk edit tasks
- `E`: Export tasks
- `I`: Import tasks
- `O`: Import tasks from org-agenda
- `u`: Undo (up to 15 operations)
- `z`: Toggle expansion of all sections
- `C`: Toggle commands visibility

## Configuration

The task manager can be customized through various variables:

```elisp
;; Change save file location
(setq task-manager-save-file "~/path/to/your/tasks.org")

;; Modify backup settings
(setq task-manager-backup-interval (* 4 60 60))  ; 4 hours
(setq task-manager-max-backups 5)
```

### File Locations

The task manager uses two main files:

1. **tasks.org**: This is your main task database file
   - Default location: `~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/tasks.org`
   - You can change this by setting `task-manager-save-file` in your Emacs configuration
   - This file is automatically synced with iCloud if you're using the default location
   - The file is in Org mode format, so you can edit it directly if needed

2. **task-report.html**: This is the generated HTML report
   - Default location: Same directory as your tasks.org file
   - The report is automatically generated when you view tasks
   - You can open it in any web browser
   - The report includes all your tasks with their current status and properties

To set up custom locations, add this to your Emacs configuration:

```elisp
;; Set custom locations for task files
(setq task-manager-save-file "~/path/to/your/tasks.org")
(setq task-manager-report-file "~/path/to/your/task-report.html")

;; Optional: Set custom backup directory
(setq task-manager-backup-directory "~/path/to/your/backups/")
```

Note: If you change the default iCloud location, make sure to:
1. Create the directory if it doesn't exist
2. Ensure you have write permissions
3. Consider setting up your own backup system if not using iCloud

## Integration

- **Org Mode**: Seamless integration with Org mode for enhanced task management
- **iCloud**: Automatic sync with iCloud for cross-device access
- **Calendar**: Integration with Emacs calendar for date-based task management

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Built with Emacs Lisp
- Inspired by GTD methodology and Kenji Ohta's GTD+E method
- Enhanced with modern task management principles 
Contributions are welcome! Please open issues or pull requests if you'd like to improve this package.

## Acknowledgments
Thanks to all contributors and the Emacs community for their continuous support and contributions to open-source tools.
