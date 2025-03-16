# Gtd-E

;; Configuration for task-manager.el

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

*** Instructions

1. Modify the =load-path= to point to your local installation of =task-manager.el=.
2. Update the =task-manager-save-file= path to where your =tasks.org= file is located.
