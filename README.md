# Gtd-E

* Configuration for task-manager.el

;; Add the path to your local task-manager.el

(add-to-list 'load-path "/Users/juanmanuelferreradiaz/.emacs.d/site-lisp/task-manager/")
(require 'task-manager)

;; Define the file path for saving tasks

(defcustom task-manager-save-file 
  (expand-file-name "tasks.org" "/Users/juanmanuelferreradiaz/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/my-gtd/")
  "File where task manager data is saved."
  :type 'file
  :group 'task-manager)
#+end_src

1. Modify the =load-path= to point to your local installation of =task-manager.el=.
2. Update the =task-manager-save-file= path to where your =tasks.org= file is located.


* GTD+E Task Manager Manual

** Overview
The GTD+E Task Manager is a simple and effective task management system designed in elisp for integrating the Getting Things Done (GTD) methodology with enhanced task organization using Emacs (GTD+E). It provides sections for different types of tasks, allowing for efficient tracking, reviewing, and prioritizing of tasks.

*** Sections
1. *Inbox*: Temporary space for capturing new tasks with no designated placement.
2. *Today*: Tasks you aim to complete today.
3. *Week*: Tasks you plan to address within this week.
4. *Monday*: Assists in planning tasks for the upcoming week during the weekly review.
5. *Calendar*: Tasks with specific deadlines, sorted by date.
6. *Someday*: Ideas or tasks to consider for future scheduling.
7. *Archive*: Storage for completed or out-of-date tasks, allowing for later reference.

** Daily Routine
At the start of each day:
1. Open the *Today* section and identify which tasks you plan to focus on.
2. Review the *Week* section to ensure you are aligned with your weekly goals.
3. Adjust as necessary, moving tasks from the Week section to Today.

** Weekly Review (Every Monday)
Conduct a review to determine:
1. Progress on tasks listed in the *Week* and *Today* sections.
2. Evaluate tasks in the *Monday* section to decide what needs to be moved into *Week* or *Today*.
3. For any tasks that are ready to be moved to *Today* or remain in *Someday*, make those adjustments.

** Monthly Review (1st of each Month)
On the first of each month:
1. Inspect tasks in the *Someday* section.
2. Determine which tasks are still relevant and sort them into Today, Week, or Monday sections, or leave them in Someday.
3. Archive tasks that can be closed for good.

** Task Operations
- *Add Tasks*: Utilize the keybinding =a= to add tasks to the Inbox, or select a section to add tasks directly.
- *Edit Tasks*: Click on any task in the display to edit it inline, which allows for immediate changes.
- *Delete Tasks*: Use the =d= key to delete any selected tasks, with the option to permanently delete using =C-k=.
- *Move Tasks*: Use =m= to move selected tasks to another section for better organization.
- *Focus*: Use section-specific commands to collapse all other sections and focus on one section at a time.

** Calendar Functionality in GTD+E Task Manager*

The calendar functionality in the GTD+E Task Manager enhances time-sensitive task management by allowing users to link tasks with specific deadlines easily. 

** Key Features:

1. *Task Date Insertion*: 
   - Use the =C-c d= command during task entry to bring up a calendar interface, enabling quick date selection for tasks.

2. *Automatic Calendar Section*: 
   - Tasks with assigned dates automatically appear in a newly created section called "Calendar Tasks" at the top of the main sections, ensuring immediate visibility.

3. *Sorting by Date*: 
   - Tasks in the Calendar section are sorted by due dates, allowing you to prioritize deadlines effectively.

4. *Daily Review*: 
   - Quickly check the Calendar section to identify tasks due today, streamlining daily planning.

** Using Calendar Functionality:

- Navigate the calendar using arrow keys, select a date with =RET=, or cancel with =q=.
- Regularly check the Calendar section to stay updated on impending deadlines and integrate tasks into your daily workflow.

** Conclusion

The calendar functionality in the GTD+E Task Manager streamlines task management by automatically highlighting date-driven tasks in the "Calendar Tasks" section at the very top of the screen, enabling efficient prioritization and effective deadline tracking.

* Task Management Key Bindings
- =a=: Add a single task.
- =A=: Add multiple tasks (one per line).
- =k=: Delete selected tasks.
- =m=: Move selected tasks.
- =W=: Move all tasks from Inbox and Today to Week.
- =D=: Delete all tasks with confirmation.
- =z=: Expand/collapse all sections.
- =e=: Toggle expansion of one section.
- =n= / =p=: Move to the next/previous task.
- =RET=: Edit the task under the cursor.

* Conclusion
Using the GTD+E Task Manager allows you to maintain a structured approach to managing tasks, facilitating daily operational success, weekly reviews for strategic alignment, and monthly reflection to ensure long-term productivity. Follow this manual for efficient use and empowerment through organizational clarity.
