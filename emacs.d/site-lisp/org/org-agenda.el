;;; org-agenda.el --- The table editor for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for creating and using the Agenda for Org-mode.

;;; Code:

(require 'org)
(eval-when-compile
  (require 'calendar))

(declare-function add-to-diary-list "diary-lib"
                  (date string specifier &optional marker globcolor literal))
(declare-function calendar-absolute-from-iso    "cal-iso"    (date))
(declare-function calendar-astro-date-string    "cal-julian" (&optional date))
(declare-function calendar-bahai-date-string    "cal-bahai"  (&optional date))
(declare-function calendar-check-holidays       "holidays"   (date))
(declare-function calendar-chinese-date-string  "cal-china"  (&optional date))
(declare-function calendar-coptic-date-string   "cal-coptic" (&optional date))
(declare-function calendar-ethiopic-date-string "cal-coptic" (&optional date))
(declare-function calendar-french-date-string   "cal-french" (&optional date))
(declare-function calendar-goto-date            "cal-move"   (date))
(declare-function calendar-hebrew-date-string   "cal-hebrew" (&optional date))
(declare-function calendar-islamic-date-string  "cal-islam"  (&optional date))
(declare-function calendar-iso-date-string      "cal-iso"    (&optional date))
(declare-function calendar-iso-from-absolute    "cal-iso"    (&optional date))
(declare-function calendar-julian-date-string   "cal-julian" (&optional date))
(declare-function calendar-mayan-date-string    "cal-mayan"  (&optional date))
(declare-function calendar-persian-date-string  "cal-persia" (&optional date))
(declare-function org-columns-quit              "org-colview" ())
(defvar calendar-mode-map)

;; Defined somewhere in this file, but used before definition.
(defvar org-agenda-buffer-name)
(defvar org-agenda-overriding-header)
(defvar entry)
(defvar date)
(defvar org-agenda-undo-list)
(defvar org-agenda-pending-undo-list)
(defvar original-date) ; dynamically scoped, calendar.el does scope this

(defcustom org-agenda-confirm-kill 1
  "When set, remote killing from the agenda buffer needs confirmation.
When t, a confirmation is always needed.  When a number N, confirmation is
only needed when the text to be killed contains more than N non-white lines."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Always" t)
	  (number :tag "When more than N lines")))

(defcustom org-agenda-compact-blocks nil
  "Non-nil means, make the block agenda more compact.
This is done by leaving out unnecessary lines."
  :group 'org-agenda
  :type 'boolean)

(defgroup org-agenda-export nil
 "Options concerning exporting agenda views in Org-mode."
 :tag "Org Agenda Export"
 :group 'org-agenda)

(defcustom org-agenda-with-colors t
  "Non-nil means, use colors in agenda views."
  :group 'org-agenda-export
  :type 'boolean)

(defcustom org-agenda-exporter-settings nil
  "Alist of variable/value pairs that should be active during agenda export.
This is a good place to set uptions for ps-print and for htmlize."
  :group 'org-agenda-export
  :type '(repeat
	  (list
	   (variable)
	   (sexp :tag "Value"))))

(defcustom org-agenda-export-html-style ""
  "The style specification for exported HTML Agenda files.
If this variable contains a string, it will replace the default <style>
section as produced by `htmlize'.
Since there are different ways of setting style information, this variable
needs to contain the full HTML structure to provide a style, including the
surrounding HTML tags.  The style specifications should include definitions
the fonts used by the agenda, here is an example:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       .org-agenda-structure {
          font-size: 110%;
          color: #003399;
          font-weight: 600;
       }
       .org-todo {
          color: #cc6666;
          font-weight: bold;
       }
       .org-done {
          color: #339933;
       }
       .title { text-align: center; }
       .todo, .deadline { color: red; }
       .done { color: green; }
    </style>

or, if you want to keep the style in a file,

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to also add other text to the header.  However,
<style>...</style> is required, if not present the variable will be ignored."
  :group 'org-agenda-export
  :group 'org-export-html
  :type 'string)

(defgroup org-agenda-custom-commands nil
 "Options concerning agenda views in Org-mode."
 :tag "Org Agenda Custom Commands"
 :group 'org-agenda)

(defconst org-sorting-choice
  '(choice
    (const time-up) (const time-down)
    (const category-keep) (const category-up) (const category-down)
    (const tag-down) (const tag-up)
    (const priority-up) (const priority-down)
    (const effort-up) (const effort-down))
  "Sorting choices.")

(defconst org-agenda-custom-commands-local-options
  `(repeat :tag "Local settings for this command. Remember to quote values"
	   (choice :tag "Setting"
	    (list :tag "Any variable"
		  (variable :tag "Variable")
		  (sexp :tag "Value"))
	    (list :tag "Files to be searched"
		  (const org-agenda-files)
		  (list
		   (const :format "" quote)
		   (repeat
			   (file))))
	    (list :tag "Sorting strategy"
		  (const org-agenda-sorting-strategy)
		  (list
		   (const :format "" quote)
		   (repeat
		    ,org-sorting-choice)))
	    (list :tag "Prefix format"
		  (const org-agenda-prefix-format :value "  %-12:c%?-12t% s")
		  (string))
	    (list :tag "Number of days in agenda"
		  (const org-agenda-ndays)
		  (integer :value 1))
	    (list :tag "Fixed starting date"
		  (const org-agenda-start-day)
		  (string :value "2007-11-01"))
	    (list :tag "Start on day of week"
		  (const org-agenda-start-on-weekday)
		  (choice :value 1
			  (const :tag "Today" nil)
			  (number :tag "Weekday No.")))
	    (list :tag "Include data from diary"
		  (const org-agenda-include-diary)
		  (boolean))
	    (list :tag "Deadline Warning days"
		  (const org-deadline-warning-days)
		  (integer :value 1))
	    (list :tag "Standard skipping condition"
		  :value (org-agenda-skip-function '(org-agenda-skip-entry-if))
		  (const org-agenda-skip-function)
		  (list
		   (const :format "" quote)
		   (list
		    (choice
		     :tag "Skiping range"
		     (const :tag "Skip entry" org-agenda-skip-entry-if)
		     (const :tag "Skip subtree" org-agenda-skip-subtree-if))
		    (repeat :inline t :tag "Conditions for skipping"
			    (choice
			     :tag "Condition type"
			     (list :tag "Regexp matches" :inline t (const :format "" 'regexp) (regexp))
			     (list :tag "Regexp does not match" :inline t (const :format "" 'notregexp) (regexp))
			     (const :tag "scheduled" 'scheduled)
			     (const :tag "not scheduled" 'notscheduled)
			     (const :tag "deadline" 'deadline)
			     (const :tag "no deadline" 'notdeadline))))))
	    (list :tag "Non-standard skipping condition"
		  :value (org-agenda-skip-function)
		  (list
		   (const org-agenda-skip-function)
		   (sexp :tag "Function or form (quoted!)")))))
  "Selection of examples for agenda command settings.
This will be spliced into the custom type of
`org-agenda-custom-commands'.")


(defcustom org-agenda-custom-commands nil
  "Custom commands for the agenda.
These commands will be offered on the splash screen displayed by the
agenda dispatcher \\[org-agenda].  Each entry is a list like this:

   (key desc type match settings files)

key      The key (one or more characters as a string) to be associated
         with the command.
desc     A description of the command, when omitted or nil, a default
         description is built using MATCH.
type     The command type, any of the following symbols:
          agenda      The daily/weekly agenda.
          todo        Entries with a specific TODO keyword, in all agenda files.
          search      Entries containing search words entry or headline.
          tags        Tags/Property/TODO match in all agenda files.
          tags-todo   Tags/P/T match in all agenda files, TODO entries only.
          todo-tree   Sparse tree of specific TODO keyword in *current* file.
          tags-tree   Sparse tree with all tags matches in *current* file.
          occur-tree  Occur sparse tree for *current* file.
          ...         A user-defined function.
match    What to search for:
          - a single keyword for TODO keyword searches
          - a tags match expression for tags searches
          - a word search expression for text searches.
          - a regular expression for occur searches
          For all other commands, this should be the empty string.
settings  A list of option settings, similar to that in a let form, so like
          this: ((opt1 val1) (opt2 val2) ...).   The values will be
          evaluated at the moment of execution, so quote them when needed.
files     A list of files file to write the produced agenda buffer to
          with the command `org-store-agenda-views'.
          If a file name ends in \".html\", an HTML version of the buffer
          is written out.  If it ends in \".ps\", a postscript version is
          produced.  Otherwide, only the plain text is written to the file.

You can also define a set of commands, to create a composite agenda buffer.
In this case, an entry looks like this:

  (key desc (cmd1 cmd2 ...) general-settings-for-whole-set files)

where

desc   A description string to be displayed in the dispatcher menu.
cmd    An agenda command, similar to the above.  However, tree commands
       are no allowed, but instead you can get agenda and global todo list.
       So valid commands for a set are:
       (agenda \"\" settings)
       (alltodo \"\" settings)
       (stuck \"\" settings)
       (todo \"match\" settings files)
       (search \"match\" settings files)
       (tags \"match\" settings files)
       (tags-todo \"match\" settings files)

Each command can carry a list of options, and another set of options can be
given for the whole set of commands.  Individual command options take
precedence over the general options.

When using several characters as key to a command, the first characters
are prefix commands.  For the dispatcher to display useful information, you
should provide a description for the prefix, like

 (setq org-agenda-custom-commands
   '((\"h\" . \"HOME + Name tag searches\") ; describe prefix \"h\"
     (\"hl\" tags \"+HOME+Lisa\")
     (\"hp\" tags \"+HOME+Peter\")
     (\"hk\" tags \"+HOME+Kim\")))"
  :group 'org-agenda-custom-commands
  :type `(repeat
	  (choice :value ("x" "Describe command here" tags "" nil)
	   (list :tag "Single command"
		 (string :tag "Access Key(s) ")
		 (option (string :tag "Description"))
		 (choice
		  (const :tag "Agenda" agenda)
		  (const :tag "TODO list" alltodo)
		  (const :tag "Search words" search)
		  (const :tag "Stuck projects" stuck)
		  (const :tag "Tags search (all agenda files)" tags)
		  (const :tag "Tags search of TODO entries (all agenda files)" tags-todo)
		  (const :tag "TODO keyword search (all agenda files)" todo)
		  (const :tag "Tags sparse tree (current buffer)" tags-tree)
		  (const :tag "TODO keyword tree (current buffer)" todo-tree)
		  (const :tag "Occur tree (current buffer)" occur-tree)
		  (sexp :tag "Other, user-defined function"))
		 (string :tag "Match (only for some commands)")
		 ,org-agenda-custom-commands-local-options
		 (option (repeat :tag "Export" (file :tag "Export to"))))
	   (list :tag "Command series, all agenda files"
		 (string :tag "Access Key(s)")
		 (string :tag "Description  ")
		 (repeat :tag "Component"
		  (choice
		   (list :tag "Agenda"
			 (const :format "" agenda)
			 (const :tag "" :format "" "")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "TODO list (all keywords)"
			 (const :format "" alltodo)
			 (const :tag "" :format "" "")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "Search words"
			 (const :format "" search)
			 (string :tag "Match")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "Stuck projects"
			 (const :format "" stuck)
			 (const :tag "" :format "" "")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "Tags search"
			 (const :format "" tags)
			 (string :tag "Match")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "Tags search, TODO entries only"
			 (const :format "" tags-todo)
			 (string :tag "Match")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "TODO keyword search"
			 (const :format "" todo)
			 (string :tag "Match")
			 ,org-agenda-custom-commands-local-options)
		   (list :tag "Other, user-defined function"
			 (symbol :tag "function")
			 (string :tag "Match")
			 ,org-agenda-custom-commands-local-options)))

		 (repeat :tag "Settings for entire command set"
			 (list (variable :tag "Any variable")
			       (sexp :tag "Value")))
		 (option (repeat :tag "Export" (file :tag "Export to"))))
	   (cons :tag "Prefix key documentation"
		 (string :tag "Access Key(s)")
		 (string :tag "Description  ")))))

(defcustom org-agenda-query-register ?o
  "The register holding the current query string.
The prupose of this is that if you construct a query string interactively,
you can then use it to define a custom command."
  :group 'org-agenda-custom-commands
  :type 'character)

(defcustom org-stuck-projects
  '("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") nil "")
  "How to identify stuck projects.
This is a list of four items:
1. A tags/todo matcher string that is used to identify a project.
   The entire tree below a headline matched by this is considered one project.
2. A list of TODO keywords identifying non-stuck projects.
   If the project subtree contains any headline with one of these todo
   keywords, the project is considered to be not stuck.  If you specify
   \"*\" as a keyword, any TODO keyword will mark the project unstuck.
3. A list of tags identifying non-stuck projects.
   If the project subtree contains any headline with one of these tags,
   the project is considered to be not stuck.  If you specify \"*\" as
   a tag, any tag will mark the project unstuck.
4. An arbitrary regular expression matching non-stuck projects.

After defining this variable, you may use \\[org-agenda-list-stuck-projects]
or `C-c a #' to produce the list."
  :group 'org-agenda-custom-commands
  :type '(list
	  (string :tag "Tags/TODO match to identify a project")
	  (repeat :tag "Projects are *not* stuck if they have an entry with TODO keyword any of" (string))
	  (repeat :tag "Projects are *not* stuck if they have an entry with TAG being any of" (string))
	  (regexp :tag "Projects are *not* stuck if this regexp matches\ninside the subtree")))


(defgroup org-agenda-skip nil
 "Options concerning skipping parts of agenda files."
 :tag "Org Agenda Skip"
 :group 'org-agenda)

(defcustom org-agenda-todo-list-sublevels t
  "Non-nil means, check also the sublevels of a TODO entry for TODO entries.
When nil, the sublevels of a TODO entry are not checked, resulting in
potentially much shorter TODO lists."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-todo-ignore-with-date nil
  "Non-nil means, don't show entries with a date in the global todo list.
You can use this if you prefer to mark mere appointments with a TODO keyword,
but don't want them to show up in the TODO list.
When this is set, it also covers deadlines and scheduled items, the settings
of `org-agenda-todo-ignore-scheduled' and `org-agenda-todo-ignore-deadlines'
will be ignored."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-todo-ignore-scheduled nil
  "Non-nil means, don't show scheduled entries in the global todo list.
The idea behind this is that by scheduling it, you have already taken care
of this item.
See also `org-agenda-todo-ignore-with-date'."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-todo-ignore-deadlines nil
  "Non-nil means, don't show near deadline entries in the global todo list.
Near means closer than `org-deadline-warning-days' days.
The idea behind this is that such items will appear in the agenda anyway.
See also `org-agenda-todo-ignore-with-date'."
  :group 'org-agenda-skip
  :group 'org-todo
  :type 'boolean)

(defcustom org-agenda-skip-scheduled-if-done nil
  "Non-nil means don't show scheduled items in agenda when they are done.
This is relevant for the daily/weekly agenda, not for the TODO list.  And
it applies only to the actual date of the scheduling.  Warnings about
an item with a past scheduling dates are always turned off when the item
is DONE."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-deadline-if-done nil
  "Non-nil means don't show deadines when the corresponding item is done.
When nil, the deadline is still shown and should give you a happy feeling.
This is relevant for the daily/weekly agenda.  And it applied only to the
actualy date of the deadline.  Warnings about approching and past-due
deadlines are always turned off when the item is DONE."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-agenda-skip-timestamp-if-done nil
  "Non-nil means don't select item by timestamp or -range if it is DONE."
  :group 'org-agenda-skip
  :type 'boolean)

(defcustom org-timeline-show-empty-dates 3
  "Non-nil means, `org-timeline' also shows dates without an entry.
When nil, only the days which actually have entries are shown.
When t, all days between the first and the last date are shown.
When an integer, show also empty dates, but if there is a gap of more than
N days, just insert a special line indicating the size of the gap."
  :group 'org-agenda-skip
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "All" t)
	  (number :tag "at most")))


(defgroup org-agenda-startup nil
  "Options concerning initial settings in the Agenda in Org Mode."
  :tag "Org Agenda Startup"
  :group 'org-agenda)

(defcustom org-finalize-agenda-hook nil
  "Hook run just before displaying an agenda buffer."
  :group 'org-agenda-startup
  :type 'hook)

(defcustom org-agenda-mouse-1-follows-link nil
  "Non-nil means, mouse-1 on a link will follow the link in the agenda.
A longer mouse click will still set point.  Does not work on XEmacs.
Needs to be set before org.el is loaded."
  :group 'org-agenda-startup
  :type 'boolean)

(defcustom org-agenda-start-with-follow-mode nil
  "The initial value of follow-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :type 'boolean)

(defvar org-agenda-include-inactive-timestamps nil
  "Non-nil means, include inactive time stamps in agenda and timeline.")

(defgroup org-agenda-windows nil
  "Options concerning the windows used by the Agenda in Org Mode."
  :tag "Org Agenda Windows"
  :group 'org-agenda)

(defcustom org-agenda-window-setup 'reorganize-frame
  "How the agenda buffer should be displayed.
Possible values for this option are:

current-window    Show agenda in the current window, keeping all other windows.
other-frame       Use `switch-to-buffer-other-frame' to display agenda.
other-window      Use `switch-to-buffer-other-window' to display agenda.
reorganize-frame  Show only two windows on the current frame, the current
                  window and the agenda.
See also the variable `org-agenda-restore-windows-after-quit'."
  :group 'org-agenda-windows
  :type '(choice
	  (const current-window)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defcustom org-agenda-window-frame-fractions '(0.5 . 0.75)
  "The min and max height of the agenda window as a fraction of frame height.
The value of the variable is a cons cell with two numbers between 0 and 1.
It only matters if `org-agenda-window-setup' is `reorganize-frame'."
  :group 'org-agenda-windows
  :type '(cons (number :tag "Minimum") (number :tag "Maximum")))

(defcustom org-agenda-restore-windows-after-quit nil
  "Non-nil means, restore window configuration open exiting agenda.
Before the window configuration is changed for displaying the agenda,
the current status is recorded.  When the agenda is exited with
`q' or `x' and this option is set, the old state is restored.  If
`org-agenda-window-setup' is `other-frame', the value of this
option will be ignored.."
  :group 'org-agenda-windows
  :type 'boolean)

(defgroup org-agenda-daily/weekly nil
  "Options concerning the daily/weekly agenda."
  :tag "Org Agenda Daily/Weekly"
  :group 'org-agenda)

(defcustom org-agenda-ndays 7
  "Number of days to include in overview display.
Should be 1 or 7."
  :group 'org-agenda-daily/weekly
  :type 'number)

(defcustom org-agenda-start-on-weekday 1
  "Non-nil means, start the overview always on the specified weekday.
0 denotes Sunday, 1 denotes Monday etc.
When nil, always start on the current day."
  :group 'org-agenda-daily/weekly
  :type '(choice (const :tag "Today" nil)
		 (number :tag "Weekday No.")))

(defcustom org-agenda-show-all-dates t
  "Non-nil means, `org-agenda' shows every day in the selected range.
When nil, only the days which actually have entries are shown."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-format-date 'org-agenda-format-date-aligned
  "Format string for displaying dates in the agenda.
Used by the daily/weekly agenda and by the timeline.  This should be
a format string understood by `format-time-string', or a function returning
the formatted date as a string.  The function must take a single argument,
a calendar-style date list like (month day year)."
  :group 'org-agenda-daily/weekly
  :type '(choice
	  (string :tag "Format string")
	  (function :tag "Function")))

(defun org-agenda-format-date-aligned (date)
  "Format a date string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
    (format "%-10s %2d %s %4d%s"
	    dayname day monthname year weekstring)))

(defcustom org-agenda-weekend-days '(6 0)
  "Which days are weekend?
These days get the special face `org-agenda-date-weekend' in the agenda
and timeline buffers."
  :group 'org-agenda-daily/weekly
  :type '(set :greedy t
	      (const :tag "Monday" 1)
	      (const :tag "Tuesday" 2)
	      (const :tag "Wednesday" 3)
	      (const :tag "Thursday" 4)
	      (const :tag "Friday" 5)
	      (const :tag "Saturday" 6)
	      (const :tag "Sunday" 0)))

(defcustom org-agenda-include-diary nil
  "If non-nil, include in the agenda entries from the Emacs Calendar's diary."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-include-all-todo nil
  "Set  means weekly/daily agenda will always contain all TODO entries.
The TODO entries will be listed at the top of the agenda, before
the entries for specific days."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-repeating-timestamp-show-all t
  "Non-nil means, show all occurences of a repeating stamp in the agenda.
When nil, only one occurence is shown, either today or the
nearest into the future."
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-scheduled-past-days 10000
  "No. of days to continue listing scheduled items that are not marked DONE.
When an item is scheduled on a date, it shows up in the agenda on this
day and will be listed until it is marked done for the number of days
given here."
  :group 'org-agenda-daily/weekly
  :type 'number)

(defcustom org-agenda-start-with-clockreport-mode nil
  "The initial value of clockreport-mode in a newly created agenda window."
  :group 'org-agenda-startup
  :group 'org-agenda-daily/weekly
  :type 'boolean)

(defcustom org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2)
  "Property list with parameters for the clocktable in clockreport mode.
This is the display mode that shows a clock table in the daily/weekly
agenda, the properties for this dynamic block can be set here.
The usual clocktable parameters are allowed here, but you cannot set
the properties :name, :tstart, :tend, :block, and :scope - these will
be overwritten to make sure the content accurately reflects the
current display in the agenda."
  :group 'org-agenda-daily/weekly
  :type 'plist)


(defgroup org-agenda-time-grid nil
  "Options concerning the time grid in the Org-mode Agenda."
  :tag "Org Agenda Time Grid"
  :group 'org-agenda)

(defcustom org-agenda-use-time-grid t
  "Non-nil means, show a time grid in the agenda schedule.
A time grid is a set of lines for specific times (like every two hours between
8:00 and 20:00).  The items scheduled for a day at specific times are
sorted in between these lines.
For details about when the grid will be shown, and what it will look like, see
the variable `org-agenda-time-grid'."
  :group 'org-agenda-time-grid
  :type 'boolean)

(defcustom org-agenda-time-grid
  '((daily today require-timed)
    "----------------"
    (800 1000 1200 1400 1600 1800 2000))

  "The settings for time grid for agenda display.
This is a list of three items.  The first item is again a list.  It contains
symbols specifying conditions when the grid should be displayed:

 daily         if the agenda shows a single day
 weekly        if the agenda shows an entire week
 today         show grid on current date, independent of daily/weekly display
 require-timed show grid only if at least one item has a time specification

The second item is a string which will be places behing the grid time.

The third item is a list of integers, indicating the times that should have
a grid line."
  :group 'org-agenda-time-grid
  :type
  '(list
    (set :greedy t :tag "Grid Display Options"
	 (const :tag "Show grid in single day agenda display" daily)
	 (const :tag "Show grid in weekly agenda display" weekly)
	 (const :tag "Always show grid for today" today)
	 (const :tag "Show grid only if any timed entries are present"
		require-timed)
	 (const :tag "Skip grid times already present in an entry"
		remove-match))
    (string :tag "Grid String")
    (repeat :tag "Grid Times" (integer :tag "Time"))))

(defgroup org-agenda-sorting nil
  "Options concerning sorting in the Org-mode Agenda."
  :tag "Org Agenda Sorting"
  :group 'org-agenda)

(defcustom org-agenda-sorting-strategy
  '((agenda time-up category-keep priority-down)
    (todo category-keep priority-down)
    (tags category-keep priority-down)
    (search category-keep))
  "Sorting structure for the agenda items of a single day.
This is a list of symbols which will be used in sequence to determine
if an entry should be listed before another entry.  The following
symbols are recognized:

time-up         Put entries with time-of-day indications first, early first
time-down       Put entries with time-of-day indications first, late first
category-keep   Keep the default order of categories, corresponding to the
		sequence in `org-agenda-files'.
category-up     Sort alphabetically by category, A-Z.
category-down   Sort alphabetically by category, Z-A.
tag-up          Sort alphabetically by last tag, A-Z.
tag-down        Sort alphabetically by last tag, Z-A.
priority-up     Sort numerically by priority, high priority last.
priority-down   Sort numerically by priority, high priority first.
effort-up       Sort numerically by estimated effort, high effort last.
effort-down     Sort numerically by estimated effort, high effort first.

The different possibilities will be tried in sequence, and testing stops
if one comparison returns a \"not-equal\".  For example, the default
    '(time-up category-keep priority-down)
means: Pull out all entries having a specified time of day and sort them,
in order to make a time schedule for the current day the first thing in the
agenda listing for the day.  Of the entries without a time indication, keep
the grouped in categories, don't sort the categories, but keep them in
the sequence given in `org-agenda-files'.  Within each category sort by
priority.

Leaving out `category-keep' would mean that items will be sorted across
categories by priority.

Instead of a single list, this can also be a set of list for specific
contents, with a context symbol in the car of the list, any of
`agenda', `todo', `tags' for the corresponding agenda views."
  :group 'org-agenda-sorting
  :type `(choice
	  (repeat :tag "General" ,org-sorting-choice)
	  (list :tag "Individually"
		(cons (const :tag "Strategy for Weekly/Daily agenda" agenda)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for TODO lists" todo)
		      (repeat ,org-sorting-choice))
		(cons (const :tag "Strategy for Tags matches" tags)
		      (repeat ,org-sorting-choice)))))

(defcustom org-sort-agenda-notime-is-late t
  "Non-nil means, items without time are considered late.
This is only relevant for sorting.  When t, items which have no explicit
time like 15:30 will be considered as 99:01, i.e. later than any items which
do have a time.  When nil, the default time is before 0:00.  You can use this
option to decide if the schedule for today should come before or after timeless
agenda entries."
  :group 'org-agenda-sorting
  :type 'boolean)

(defcustom org-sort-agenda-noeffort-is-high t
  "Non-nil means, items without effort estimate are sorted as high effort.
When nil, such items are sorted as 0 minutes effort."
  :group 'org-agenda-sorting
  :type 'boolean)

(defgroup org-agenda-line-format nil
  "Options concerning the entry prefix in the Org-mode agenda display."
  :tag "Org Agenda Line Format"
  :group 'org-agenda)

(defcustom org-agenda-prefix-format
  '((agenda  . "  %-12:c%?-12t% s")
    (timeline  . "  % s")
    (todo  . "  %-12:c")
    (tags  . "  %-12:c")
    (search . "  %-12:c"))
  "Format specifications for the prefix of items in the agenda views.
An alist with four entries, for the different agenda types.  The keys to the
sublists are `agenda', `timeline', `todo', and `tags'.  The values
are format strings.
This format works similar to a printf format, with the following meaning:

  %c   the category of the item, \"Diary\" for entries from the diary, or
       as given by the CATEGORY keyword or derived from the file name.
  %T   the *last* tag of the item.  Last because inherited tags come
       first in the list.
  %t   the time-of-day specification if one applies to the entry, in the
       format HH:MM
  %s   Scheduling/Deadline information, a short string

All specifiers work basically like the standard `%s' of printf, but may
contain two additional characters:  A question mark just after the `%' and
a whitespace/punctuation character just before the final letter.

If the first character after `%' is a question mark, the entire field
will only be included if the corresponding value applies to the
current entry.  This is useful for fields which should have fixed
width when present, but zero width when absent.  For example,
\"%?-12t\" will result in a 12 character time field if a time of the
day is specified, but will completely disappear in entries which do
not contain a time.

If there is punctuation or whitespace character just before the final
format letter, this character will be appended to the field value if
the value is not empty.  For example, the format \"%-12:c\" leads to
\"Diary: \" if the category is \"Diary\".  If the category were be
empty, no additional colon would be interted.

The default value of this option is \"  %-12:c%?-12t% s\", meaning:
- Indent the line with two space characters
- Give the category in a 12 chars wide field, padded with whitespace on
  the right (because of `-').  Append a colon if there is a category
  (because of `:').
- If there is a time-of-day, put it into a 12 chars wide field.  If no
  time, don't put in an empty field, just skip it (because of '?').
- Finally, put the scheduling information and append a whitespace.

As another example, if you don't want the time-of-day of entries in
the prefix, you could use:

  (setq org-agenda-prefix-format \"  %-11:c% s\")

See also the variables `org-agenda-remove-times-when-in-prefix' and
`org-agenda-remove-tags'."
  :type '(choice
	  (string :tag "General format")
	  (list :greedy t :tag "View dependent"
		(cons  (const agenda) (string :tag "Format"))
		(cons  (const timeline) (string :tag "Format"))
		(cons  (const todo) (string :tag "Format"))
		(cons  (const tags) (string :tag "Format"))
		(cons  (const search) (string :tag "Format"))))
  :group 'org-agenda-line-format)

(defvar org-prefix-format-compiled nil
  "The compiled version of the most recently used prefix format.
See the variable `org-agenda-prefix-format'.")

(defcustom org-agenda-todo-keyword-format "%-1s"
  "Format for the TODO keyword in agenda lines.
Set this to something like \"%-12s\" if you want all TODO keywords
to occupy a fixed space in the agenda display."
  :group 'org-agenda-line-format
  :type 'string)

(defcustom org-agenda-scheduled-leaders '("Scheduled: " "Sched.%2dx: ")
  "Text preceeding scheduled items in the agenda view.
This is a list with two strings.  The first applies when the item is
scheduled on the current day.  The second applies when it has been scheduled
previously, it may contain a %d to capture how many days ago the item was
scheduled."
  :group 'org-agenda-line-format
  :type '(list
	  (string :tag "Scheduled today     ")
	  (string :tag "Scheduled previously")))

(defcustom org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: ")
  "Text preceeding deadline items in the agenda view.
This is a list with two strings.  The first applies when the item has its
deadline on the current day.  The second applies when it is in the past or
in the future, it may contain %d to capture how many days away the deadline
is (was)."
  :group 'org-agenda-line-format
  :type '(list
	  (string :tag "Deadline today   ")
	  (choice :tag "Deadline relative"
		  (string :tag "Format string")
		  (function))))

(defcustom org-agenda-remove-times-when-in-prefix t
  "Non-nil means, remove duplicate time specifications in agenda items.
When the format `org-agenda-prefix-format' contains a `%t' specifier, a
time-of-day specification in a headline or diary entry is extracted and
placed into the prefix.  If this option is non-nil, the original specification
\(a timestamp or -range, or just a plain time(range) specification like
11:30-4pm) will be removed for agenda display.  This makes the agenda less
cluttered.
The option can be t or nil.  It may also be the symbol `beg', indicating
that the time should only be removed what it is located at the beginning of
the headline/diary entry."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When at beginning of entry" beg)))


(defcustom org-agenda-default-appointment-duration nil
  "Default duration for appointments that only have a starting time.
When nil, no duration is specified in such cases.
When non-nil, this must be the number of minutes, e.g. 60 for one hour."
  :group 'org-agenda-line-format
  :type '(choice
	  (integer :tag "Minutes")
	  (const :tag "No default duration")))


(defcustom org-agenda-remove-tags nil
  "Non-nil means, remove the tags from the headline copy in the agenda.
When this is the symbol `prefix', only remove tags when
`org-agenda-prefix-format' contains a `%T' specifier."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Never" nil)
	  (const :tag "When prefix format contains %T" prefix)))

(if (fboundp 'defvaralias)
    (defvaralias 'org-agenda-remove-tags-when-in-prefix
      'org-agenda-remove-tags))

(defcustom org-agenda-tags-column -80
  "Shift tags in agenda items to this column.
If this number is positive, it specifies the column.  If it is negative,
it means that the tags should be flushright to that column.  For example,
-80 works well for a normal 80 character screen."
  :group 'org-agenda-line-format
  :type 'integer)

(if (fboundp 'defvaralias)
    (defvaralias 'org-agenda-align-tags-to-column 'org-agenda-tags-column))

(defcustom org-agenda-fontify-priorities t
  "Non-nil means, highlight low and high priorities in agenda.
When t, the highest priority entries are bold, lowest priority italic.
This may also be an association list of priority faces.  The face may be
a names face, or a list like `(:background \"Red\")'."
  :group 'org-agenda-line-format
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Defaults" t)
	  (repeat :tag "Specify"
		  (list (character :tag "Priority" :value ?A)
			(sexp :tag "face")))))


(defgroup org-agenda-column-view nil
  "Options concerning column view in the agenda."
  :tag "Org Agenda Column View"
  :group 'org-agenda)

(defcustom org-agenda-columns-show-summaries t
  "Non-nil means, show summaries for columns displayed in the agenda view."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-compute-summary-properties t
  "Non-nil means, recompute all summary properties before column view.
When column view in the agenda is listing properties that have a summary
operator, it can go to all relevant buffers and recompute the summaries
there.  This can mean overhead for the agenda column view, but is necessary
to have thing up to date.
As a special case, a CLOCKSUM property also makes sure that the clock
computations are current."
  :group 'org-agenda-column-view
  :type 'boolean)

(defcustom org-agenda-columns-add-appointments-to-effort-sum nil
  "Non-nil means, the duration of an appointment will add to day effort.
The property to which appointment durations will be added is the one given
in the option `org-effort-property'.  If an appointment does not have
an end time, `org-agenda-default-appointment-duration' will be used.  If that
is not set, an appointment without end time will not contribute to the time
estimate."
  :group 'org-agenda-column-view
  :type 'boolean)

(eval-when-compile
  (require 'cl))
(require 'org)

(defun org-add-agenda-custom-command (entry)
  "Replace or add a command in `org-agenda-custom-commands'.
This is mostly for hacking and trying a new command - once the command
works you probably want to add it to `org-agenda-custom-commands' for good."
  (let ((ass (assoc (car entry) org-agenda-custom-commands)))
    (if ass
	(setcdr ass (cdr entry))
      (push entry org-agenda-custom-commands))))

;;; Define the Org-agenda-mode

(defvar org-agenda-mode-map (make-sparse-keymap)
  "Keymap for `org-agenda-mode'.")

(defvar org-agenda-menu) ; defined later in this file.
(defvar org-agenda-follow-mode nil)
(defvar org-agenda-clockreport-mode nil)
(defvar org-agenda-show-log nil)
(defvar org-agenda-redo-command nil)
(defvar org-agenda-query-string nil)
(defvar org-agenda-mode-hook nil)
(defvar org-agenda-type nil)
(defvar org-agenda-force-single-file nil)

(defun org-agenda-mode ()
  "Mode for time-sorted view on action items in Org-mode files.

The following commands are available:

\\{org-agenda-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq org-agenda-undo-list nil
	org-agenda-pending-undo-list nil)
  (setq major-mode 'org-agenda-mode)
  ;; Keep global-font-lock-mode from turning on font-lock-mode
  (org-set-local 'font-lock-global-modes (list 'not major-mode))
  (setq mode-name "Org-Agenda")
  (use-local-map org-agenda-mode-map)
  (easy-menu-add org-agenda-menu)
  (if org-startup-truncated (setq truncate-lines t))
  (org-add-hook 'post-command-hook 'org-agenda-post-command-hook nil 'local)
  (org-add-hook 'pre-command-hook 'org-unhighlight nil 'local)
  ;; Make sure properties are removed when copying text
  (when (boundp 'buffer-substring-filters)
    (org-set-local 'buffer-substring-filters
		   (cons (lambda (x)
                           (set-text-properties 0 (length x) nil x) x)
			 buffer-substring-filters)))
  (unless org-agenda-keep-modes
    (setq org-agenda-follow-mode org-agenda-start-with-follow-mode
	  org-agenda-clockreport-mode org-agenda-start-with-clockreport-mode
	  org-agenda-show-log nil))
  (easy-menu-change
   '("Agenda") "Agenda Files"
   (append
    (list
     (vector
      (if (get 'org-agenda-files 'org-restrict)
	  "Restricted to single file"
	"Edit File List")
      '(org-edit-agenda-file-list)
      (not (get 'org-agenda-files 'org-restrict)))
     "--")
    (mapcar 'org-file-menu-entry (org-agenda-files))))
  (org-agenda-set-mode-name)
  (apply
   (if (fboundp 'run-mode-hooks) 'run-mode-hooks 'run-hooks)
   (list 'org-agenda-mode-hook)))

(substitute-key-definition 'undo 'org-agenda-undo
			   org-agenda-mode-map global-map)
(org-defkey org-agenda-mode-map "\C-i"     'org-agenda-goto)
(org-defkey org-agenda-mode-map [(tab)]    'org-agenda-goto)
(org-defkey org-agenda-mode-map "\C-m"     'org-agenda-switch-to)
(org-defkey org-agenda-mode-map "\C-k"     'org-agenda-kill)
(org-defkey org-agenda-mode-map "\C-c$"    'org-agenda-archive)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-s" 'org-agenda-archive)
(org-defkey org-agenda-mode-map "$"        'org-agenda-archive)
(org-defkey org-agenda-mode-map "A"        'org-agenda-archive-to-archive-sibling)
(org-defkey org-agenda-mode-map "\C-c\C-o" 'org-agenda-open-link)
(org-defkey org-agenda-mode-map " "        'org-agenda-show)
(org-defkey org-agenda-mode-map "\C-c\C-t" 'org-agenda-todo)
(org-defkey org-agenda-mode-map [(control shift right)] 'org-agenda-todo-nextset)
(org-defkey org-agenda-mode-map [(control shift left)]  'org-agenda-todo-previousset)
(org-defkey org-agenda-mode-map "\C-c\C-xb" 'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "b"        'org-agenda-tree-to-indirect-buffer)
(org-defkey org-agenda-mode-map "o"        'delete-other-windows)
(org-defkey org-agenda-mode-map "L"        'org-agenda-recenter)
(org-defkey org-agenda-mode-map "t"        'org-agenda-todo)
(org-defkey org-agenda-mode-map "a"        'org-agenda-toggle-archive-tag)
(org-defkey org-agenda-mode-map ":"        'org-agenda-set-tags)
(org-defkey org-agenda-mode-map "."        'org-agenda-goto-today)
(org-defkey org-agenda-mode-map "j"        'org-agenda-goto-date)
(org-defkey org-agenda-mode-map "d"        'org-agenda-day-view)
(org-defkey org-agenda-mode-map "w"        'org-agenda-week-view)
(org-defkey org-agenda-mode-map "m"        'org-agenda-month-view)
(org-defkey org-agenda-mode-map "y"        'org-agenda-year-view)
(org-defkey org-agenda-mode-map "\C-c\C-z" 'org-agenda-add-note)
(org-defkey org-agenda-mode-map "z"        'org-agenda-add-note)
(org-defkey org-agenda-mode-map [(shift right)] 'org-agenda-date-later)
(org-defkey org-agenda-mode-map [(shift left)] 'org-agenda-date-earlier)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (right)] 'org-agenda-date-later)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (left)] 'org-agenda-date-earlier)

(org-defkey org-agenda-mode-map ">" 'org-agenda-date-prompt)
(org-defkey org-agenda-mode-map "\C-c\C-s" 'org-agenda-schedule)
(org-defkey org-agenda-mode-map "\C-c\C-d" 'org-agenda-deadline)
(let ((l '(1 2 3 4 5 6 7 8 9 0)))
  (while l (org-defkey org-agenda-mode-map
	     (int-to-string (pop l)) 'digit-argument)))

(org-defkey org-agenda-mode-map "f" 'org-agenda-follow-mode)
(org-defkey org-agenda-mode-map "R" 'org-agenda-clockreport-mode)
(org-defkey org-agenda-mode-map "l" 'org-agenda-log-mode)
(org-defkey org-agenda-mode-map "D" 'org-agenda-toggle-diary)
(org-defkey org-agenda-mode-map "G" 'org-agenda-toggle-time-grid)
(org-defkey org-agenda-mode-map "r" 'org-agenda-redo)
(org-defkey org-agenda-mode-map "g" 'org-agenda-redo)
(org-defkey org-agenda-mode-map "e" 'org-agenda-execute)
(org-defkey org-agenda-mode-map "q" 'org-agenda-quit)
(org-defkey org-agenda-mode-map "x" 'org-agenda-exit)
(org-defkey org-agenda-mode-map "\C-x\C-w" 'org-write-agenda)
(org-defkey org-agenda-mode-map "s" 'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "\C-x\C-s" 'org-save-all-org-buffers)
(org-defkey org-agenda-mode-map "P" 'org-agenda-show-priority)
(org-defkey org-agenda-mode-map "T" 'org-agenda-show-tags)
(org-defkey org-agenda-mode-map "n" 'next-line)
(org-defkey org-agenda-mode-map "p" 'previous-line)
(org-defkey org-agenda-mode-map "\C-c\C-n" 'org-agenda-next-date-line)
(org-defkey org-agenda-mode-map "\C-c\C-p" 'org-agenda-previous-date-line)
(org-defkey org-agenda-mode-map "," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "\C-c," 'org-agenda-priority)
(org-defkey org-agenda-mode-map "i" 'org-agenda-diary-entry)
(org-defkey org-agenda-mode-map "c" 'org-agenda-goto-calendar)
(org-defkey org-agenda-mode-map "C" 'org-agenda-convert-date)
(org-defkey org-agenda-mode-map "M" 'org-agenda-phases-of-moon)
(org-defkey org-agenda-mode-map "S" 'org-agenda-sunrise-sunset)
(org-defkey org-agenda-mode-map "h" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "H" 'org-agenda-holidays)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-i" 'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "I" 'org-agenda-clock-in)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-o" 'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "O" 'org-agenda-clock-out)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-x" 'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "X" 'org-agenda-clock-cancel)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-j" 'org-clock-goto)
(org-defkey org-agenda-mode-map "J" 'org-clock-goto)
(org-defkey org-agenda-mode-map "+" 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map "-" 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(shift up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [(shift down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (up)] 'org-agenda-priority-up)
(org-defkey org-agenda-mode-map [?\C-c ?\C-x (down)] 'org-agenda-priority-down)
(org-defkey org-agenda-mode-map [(right)] 'org-agenda-later)
(org-defkey org-agenda-mode-map [(left)] 'org-agenda-earlier)
(org-defkey org-agenda-mode-map "\C-c\C-x\C-c" 'org-agenda-columns)

(org-defkey org-agenda-mode-map "[" 'org-agenda-manipulate-query-add)
(org-defkey org-agenda-mode-map "]" 'org-agenda-manipulate-query-subtract)
(org-defkey org-agenda-mode-map "{" 'org-agenda-manipulate-query-add-re)
(org-defkey org-agenda-mode-map "}" 'org-agenda-manipulate-query-subtract-re)

(defvar org-agenda-keymap (copy-keymap org-agenda-mode-map)
  "Local keymap for agenda entries from Org-mode.")

(org-defkey org-agenda-keymap
  (if (featurep 'xemacs) [(button2)] [(mouse-2)]) 'org-agenda-goto-mouse)
(org-defkey org-agenda-keymap
  (if (featurep 'xemacs) [(button3)] [(mouse-3)]) 'org-agenda-show-mouse)
(when org-agenda-mouse-1-follows-link
  (org-defkey org-agenda-keymap [follow-link] 'mouse-face))
(easy-menu-define org-agenda-menu org-agenda-mode-map "Agenda menu"
  '("Agenda"
    ("Agenda Files")
    "--"
    ["Show" org-agenda-show t]
    ["Go To (other window)" org-agenda-goto t]
    ["Go To (this window)" org-agenda-switch-to t]
    ["Follow Mode" org-agenda-follow-mode
     :style toggle :selected org-agenda-follow-mode :active t]
    ["Tree to indirect frame" org-agenda-tree-to-indirect-buffer t]
    "--"
    ["Cycle TODO" org-agenda-todo t]
    ("Archive"
     ["Toggle ARCHIVE tag" org-agenda-toggle-archive-tag t]
     ["Move to archive sibling" org-agenda-archive-to-archive-sibling t]
     ["Archive subtree" org-agenda-archive t])
    ["Delete subtree" org-agenda-kill t]
    ["Add note" org-agenda-add-note t]
    "--"
    ["Goto Today" org-agenda-goto-today (org-agenda-check-type nil 'agenda 'timeline)]
    ["Next Dates" org-agenda-later (org-agenda-check-type nil 'agenda)]
    ["Previous Dates" org-agenda-earlier (org-agenda-check-type nil 'agenda)]
    ["Jump to date" org-agenda-goto-date (org-agenda-check-type nil 'agenda)]
    "--"
    ("Tags and Properties"
     ["Show all Tags" org-agenda-show-tags t]
     ["Set Tags current line" org-agenda-set-tags (not (org-region-active-p))]
     ["Change tag in region" org-agenda-set-tags (org-region-active-p)]
     "--"
     ["Column View" org-columns t])
    ("Date/Schedule"
     ["Schedule" org-agenda-schedule t]
     ["Set Deadline" org-agenda-deadline t]
     "--"
     ["Change Date +1 day" org-agenda-date-later (org-agenda-check-type nil 'agenda 'timeline)]
     ["Change Date -1 day" org-agenda-date-earlier (org-agenda-check-type nil 'agenda 'timeline)]
     ["Change Date to ..." org-agenda-date-prompt (org-agenda-check-type nil 'agenda 'timeline)])
    ("Clock"
     ["Clock in" org-agenda-clock-in t]
     ["Clock out" org-agenda-clock-out t]
     ["Clock cancel" org-agenda-clock-cancel t]
     ["Goto running clock" org-clock-goto t])
    ("Priority"
     ["Set Priority" org-agenda-priority t]
     ["Increase Priority" org-agenda-priority-up t]
     ["Decrease Priority" org-agenda-priority-down t]
     ["Show Priority" org-agenda-show-priority t])
    ("Calendar/Diary"
     ["New Diary Entry" org-agenda-diary-entry (org-agenda-check-type nil 'agenda 'timeline)]
     ["Goto Calendar" org-agenda-goto-calendar (org-agenda-check-type nil 'agenda 'timeline)]
     ["Phases of the Moon" org-agenda-phases-of-moon (org-agenda-check-type nil 'agenda 'timeline)]
     ["Sunrise/Sunset" org-agenda-sunrise-sunset (org-agenda-check-type nil 'agenda 'timeline)]
     ["Holidays" org-agenda-holidays (org-agenda-check-type nil 'agenda 'timeline)]
     ["Convert" org-agenda-convert-date (org-agenda-check-type nil 'agenda 'timeline)]
     "--"
     ["Create iCalendar file" org-export-icalendar-combine-agenda-files t])
    "--"
    ("View"
     ["Day View" org-agenda-day-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (equal org-agenda-ndays 1)]
     ["Week View" org-agenda-week-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (equal org-agenda-ndays 7)]
     ["Month View" org-agenda-month-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (member org-agenda-ndays '(28 29 30 31))]
     ["Year View" org-agenda-year-view :active (org-agenda-check-type nil 'agenda)
      :style radio :selected (member org-agenda-ndays '(365 366))]
     "--"
     ["Show Logbook entries" org-agenda-log-mode
      :style toggle :selected org-agenda-show-log :active (org-agenda-check-type nil 'agenda 'timeline)]
     ["Show clock report" org-agenda-clockreport-mode
     :style toggle :selected org-agenda-clockreport-mode :active (org-agenda-check-type nil 'agenda)]
     ["Include Diary" org-agenda-toggle-diary
      :style toggle :selected org-agenda-include-diary :active (org-agenda-check-type nil 'agenda)]
     ["Use Time Grid" org-agenda-toggle-time-grid
      :style toggle :selected org-agenda-use-time-grid :active (org-agenda-check-type nil 'agenda)])
    ["Write view to file" org-write-agenda t]
    ["Rebuild buffer" org-agenda-redo t]
    ["Save all Org-mode Buffers" org-save-all-org-buffers t]
    "--"
    ["Undo Remote Editing" org-agenda-undo org-agenda-undo-list]
    "--"
    ["Quit" org-agenda-quit t]
    ["Exit and Release Buffers" org-agenda-exit t]
    ))

;;; Agenda undo

(defvar org-agenda-allow-remote-undo t
  "Non-nil means, allow remote undo from the agenda buffer.")
(defvar org-agenda-undo-list nil
  "List of undoable operations in the agenda since last refresh.")
(defvar org-agenda-undo-has-started-in nil
  "Buffers that have already seen `undo-start' in the current undo sequence.")
(defvar org-agenda-pending-undo-list nil
  "In a series of undo commands, this is the list of remaning undo items.")


(defun org-agenda-undo ()
  "Undo a remote editing step in the agenda.
This undoes changes both in the agenda buffer and in the remote buffer
that have been changed along."
  (interactive)
  (or org-agenda-allow-remote-undo
      (error "Check the variable `org-agenda-allow-remote-undo' to activate remote undo."))
  (if (not (eq this-command last-command))
      (setq org-agenda-undo-has-started-in nil
	    org-agenda-pending-undo-list org-agenda-undo-list))
  (if (not org-agenda-pending-undo-list)
      (error "No further undo information"))
  (let* ((entry (pop org-agenda-pending-undo-list))
	 buf line cmd rembuf)
    (setq cmd (pop entry) line (pop entry))
    (setq rembuf (nth 2 entry))
    (org-with-remote-undo rembuf
      (while (bufferp (setq buf (pop entry)))
	(if (pop entry)
	    (with-current-buffer buf
	      (let ((last-undo-buffer buf)
                    (inhibit-read-only t))
		(unless (memq buf org-agenda-undo-has-started-in)
		  (push buf org-agenda-undo-has-started-in)
		  (make-local-variable 'pending-undo-list)
		  (undo-start))
		(while (and pending-undo-list
			    (listp pending-undo-list)
			    (not (car pending-undo-list)))
		  (pop pending-undo-list))
		(undo-more 1))))))
    (goto-line line)
    (message "`%s' undone (buffer %s)" cmd (buffer-name rembuf))))

(defun org-verify-change-for-undo (l1 l2)
  "Verify that a real change occurred between the undo lists L1 and L2."
  (while (and l1 (listp l1) (null (car l1))) (pop l1))
  (while (and l2 (listp l2) (null (car l2))) (pop l2))
  (not (eq l1 l2)))

;;; Agenda dispatch

(defvar org-agenda-restrict nil)
(defvar org-agenda-restrict-begin (make-marker))
(defvar org-agenda-restrict-end (make-marker))
(defvar org-agenda-last-dispatch-buffer nil)
(defvar org-agenda-overriding-restriction nil)

;;;###autoload
(defun org-agenda (arg &optional keys restriction)
  "Dispatch agenda commands to collect entries to the agenda buffer.
Prompts for a command to execute.  Any prefix arg will be passed
on to the selected command.  The default selections are:

a     Call `org-agenda-list' to display the agenda for current day or week.
t     Call `org-todo-list' to display the global todo list.
T     Call `org-todo-list' to display the global todo list, select only
      entries with a specific TODO keyword (the user gets a prompt).
m     Call `org-tags-view' to display headlines with tags matching
      a condition  (the user is prompted for the condition).
M     Like `m', but select only TODO entries, no ordinary headlines.
L     Create a timeline for the current buffer.
e     Export views to associated files.

More commands can be added by configuring the variable
`org-agenda-custom-commands'.  In particular, specific tags and TODO keyword
searches can be pre-defined in this way.

If the current buffer is in Org-mode and visiting a file, you can also
first press `<' once to indicate that the agenda should be temporarily
\(until the next use of \\[org-agenda]) restricted to the current file.
Pressing `<' twice means to restrict to the current subtree or region
\(if active)."
  (interactive "P")
  (catch 'exit
    (let* ((prefix-descriptions nil)
	   (org-agenda-custom-commands-orig org-agenda-custom-commands)
	   (org-agenda-custom-commands
	    ;; normalize different versions
	    (delq nil
		  (mapcar
		   (lambda (x)
		     (cond ((stringp (cdr x))
			    (push x prefix-descriptions)
			    nil)
			   ((stringp (nth 1 x)) x)
			   ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
			   (t (cons (car x) (cons "" (cdr x))))))
		   org-agenda-custom-commands)))
	   (buf (current-buffer))
	   (bfn (buffer-file-name (buffer-base-buffer)))
	   entry key type match lprops ans)
      ;; Turn off restriction unless there is an overriding one
      (unless org-agenda-overriding-restriction
	(put 'org-agenda-files 'org-restrict nil)
	(setq org-agenda-restrict nil)
	(move-marker org-agenda-restrict-begin nil)
	(move-marker org-agenda-restrict-end nil))
      ;; Delete old local properties
      (put 'org-agenda-redo-command 'org-lprops nil)
      ;; Remember where this call originated
      (setq org-agenda-last-dispatch-buffer (current-buffer))
      (unless keys
	(setq ans (org-agenda-get-restriction-and-command prefix-descriptions)
	      keys (car ans)
	      restriction (cdr ans)))
      ;; Estabish the restriction, if any
      (when (and (not org-agenda-overriding-restriction) restriction)
	(put 'org-agenda-files 'org-restrict (list bfn))
	(cond
	 ((eq restriction 'region)
	  (setq org-agenda-restrict t)
	  (move-marker org-agenda-restrict-begin (region-beginning))
	  (move-marker org-agenda-restrict-end (region-end)))
	 ((eq restriction 'subtree)
	  (save-excursion
	    (setq org-agenda-restrict t)
	    (org-back-to-heading t)
	    (move-marker org-agenda-restrict-begin (point))
	    (move-marker org-agenda-restrict-end
			 (progn (org-end-of-subtree t)))))))

      (require 'calendar)  ; FIXME: can we avoid this for some commands?
      ;; For example the todo list should not need it (but does...)
      (cond
       ((setq entry (assoc keys org-agenda-custom-commands))
	(if (or (symbolp (nth 2 entry)) (functionp (nth 2 entry)))
	    (progn
	      (setq type (nth 2 entry) match (nth 3 entry) lprops (nth 4 entry))
	      (put 'org-agenda-redo-command 'org-lprops lprops)
	      (cond
	       ((eq type 'agenda)
		(org-let lprops '(org-agenda-list current-prefix-arg)))
	       ((eq type 'alltodo)
		(org-let lprops '(org-todo-list current-prefix-arg)))
	       ((eq type 'search)
		(org-let lprops '(org-search-view current-prefix-arg match nil)))
	       ((eq type 'stuck)
		(org-let lprops '(org-agenda-list-stuck-projects
				  current-prefix-arg)))
	       ((eq type 'tags)
		(org-let lprops '(org-tags-view current-prefix-arg match)))
	       ((eq type 'tags-todo)
		(org-let lprops '(org-tags-view '(4) match)))
	       ((eq type 'todo)
		(org-let lprops '(org-todo-list match)))
	       ((eq type 'tags-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-tags-sparse-tree current-prefix-arg match)))
	       ((eq type 'todo-tree)
		(org-check-for-org-mode)
		(org-let lprops
		  '(org-occur (concat "^" outline-regexp "[ \t]*"
				      (regexp-quote match) "\\>"))))
	       ((eq type 'occur-tree)
		(org-check-for-org-mode)
		(org-let lprops '(org-occur match)))
	       ((functionp type)
		(org-let lprops '(funcall type match)))
	       ((fboundp type)
		(org-let lprops '(funcall type match)))
	       (t (error "Invalid custom agenda command type %s" type))))
	  (org-run-agenda-series (nth 1 entry) (cddr entry))))
       ((equal keys "C")
	(setq org-agenda-custom-commands org-agenda-custom-commands-orig)
	(customize-variable 'org-agenda-custom-commands))
       ((equal keys "a") (call-interactively 'org-agenda-list))
       ((equal keys "s") (call-interactively 'org-search-view))
       ((equal keys "t") (call-interactively 'org-todo-list))
       ((equal keys "T") (org-call-with-arg 'org-todo-list (or arg '(4))))
       ((equal keys "m") (call-interactively 'org-tags-view))
       ((equal keys "M") (org-call-with-arg 'org-tags-view (or arg '(4))))
       ((equal keys "e") (call-interactively 'org-store-agenda-views))
       ((equal keys "L")
	(unless (org-mode-p)
	  (error "This is not an Org-mode file"))
	(unless restriction
	  (put 'org-agenda-files 'org-restrict (list bfn))
	  (org-call-with-arg 'org-timeline arg)))
       ((equal keys "#") (call-interactively 'org-agenda-list-stuck-projects))
       ((equal keys "/") (call-interactively 'org-occur-in-agenda-files))
       ((equal keys "!") (customize-variable 'org-stuck-projects))
       (t (error "Invalid agenda key"))))))

(defun org-agenda-normalize-custom-commands (cmds)
  (delq nil
	(mapcar
	 (lambda (x)
	   (cond ((stringp (cdr x)) nil)
		 ((stringp (nth 1 x)) x)
		 ((not (nth 1 x)) (cons (car x) (cons "" (cddr x))))
		 (t (cons (car x) (cons "" (cdr x))))))
	 cmds)))

(defun org-agenda-get-restriction-and-command (prefix-descriptions)
  "The user interface for selecting an agenda command."
  (catch 'exit
    (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	   (restrict-ok (and bfn (org-mode-p)))
	   (region-p (org-region-active-p))
	   (custom org-agenda-custom-commands)
	   (selstring "")
	   restriction second-time
	   c entry key type match prefixes rmheader header-end custom1 desc)
      (save-window-excursion
	(delete-other-windows)
	(org-switch-to-buffer-other-window " *Agenda Commands*")
	(erase-buffer)
	(insert (eval-when-compile
		  (let ((header
"
Press key for an agenda command:        <   Buffer,subtree/region restriction
--------------------------------        >   Remove restriction
a   Agenda for current week or day      e   Export agenda views
t   List of all TODO entries            T   Entries with special TODO kwd
m   Match a TAGS query                  M   Like m, but only TODO entries
L   Timeline for current buffer         #   List stuck projects (!=configure)
s   Search for keywords                 C   Configure custom agenda commands
/   Multi-occur
")
			(start 0))
		    (while (string-match
			    "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			    header start)
		      (setq start (match-end 0))
		      (add-text-properties (match-beginning 2) (match-end 2)
					   '(face bold) header))
		    header)))
	(setq header-end (move-marker (make-marker) (point)))
	(while t
	  (setq custom1 custom)
	  (when (eq rmheader t)
	    (goto-line 1)
	    (re-search-forward ":" nil t)
	    (delete-region (match-end 0) (point-at-eol))
	    (forward-char 1)
	    (looking-at "-+")
	    (delete-region (match-end 0) (point-at-eol))
	    (move-marker header-end (match-end 0)))
	  (goto-char header-end)
	  (delete-region (point) (point-max))
	  (while (setq entry (pop custom1))
	    (setq key (car entry) desc (nth 1 entry)
		  type (nth 2 entry) match (nth 3 entry))
	    (if (> (length key) 1)
		(add-to-list 'prefixes (string-to-char key))
	      (insert
	       (format
		"\n%-4s%-14s: %s"
		(org-add-props (copy-sequence key)
		    '(face bold))
		(cond
		 ((string-match "\\S-" desc) desc)
		 ((eq type 'agenda) "Agenda for current week or day")
		 ((eq type 'alltodo) "List of all TODO entries")
		 ((eq type 'search) "Word search")
		 ((eq type 'stuck) "List of stuck projects")
		 ((eq type 'todo) "TODO keyword")
		 ((eq type 'tags) "Tags query")
		 ((eq type 'tags-todo) "Tags (TODO)")
		 ((eq type 'tags-tree) "Tags tree")
		 ((eq type 'todo-tree) "TODO kwd tree")
		 ((eq type 'occur-tree) "Occur tree")
		 ((functionp type) (if (symbolp type)
				       (symbol-name type)
				     "Lambda expression"))
		 (t "???"))
		(cond
		 ((stringp match)
		  (org-add-props match nil 'face 'org-warning))
		 (match
		  (format "set of %d commands" (length match)))
		 (t ""))))))
	  (when prefixes
	    (mapc (lambda (x)
		    (insert
		     (format "\n%s   %s"
			     (org-add-props (char-to-string x)
					    nil 'face 'bold)
			     (or (cdr (assoc (concat selstring (char-to-string x))
					     prefix-descriptions))
				 "Prefix key"))))
		  prefixes))
	  (goto-char (point-min))
	  (when (fboundp 'fit-window-to-buffer)
	    (if second-time
		(if (not (pos-visible-in-window-p (point-max)))
		    (fit-window-to-buffer))
	      (setq second-time t)
	      (fit-window-to-buffer)))
	  (message "Press key for agenda command%s:"
		   (if (or restrict-ok org-agenda-overriding-restriction)
		       (if org-agenda-overriding-restriction
			   " (restriction lock active)"
			 (if restriction
			     (format " (restricted to %s)" restriction)
			   " (unrestricted)"))
		     ""))
	  (setq c (read-char-exclusive))
	  (message "")
	  (cond
	   ((assoc (char-to-string c) custom)
	    (setq selstring (concat selstring (char-to-string c)))
	    (throw 'exit (cons selstring restriction)))
	   ((memq c prefixes)
	    (setq selstring (concat selstring (char-to-string c))
		  prefixes nil
		  rmheader (or rmheader t)
		  custom (delq nil (mapcar
				    (lambda (x)
				      (if (or (= (length (car x)) 1)
					      (/= (string-to-char (car x)) c))
					  nil
					(cons (substring (car x) 1) (cdr x))))
				    custom))))
	   ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	    (message "Restriction is only possible in Org-mode buffers")
	    (ding) (sit-for 1))
	   ((eq c ?1)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction 'buffer))
	   ((eq c ?0)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction (if region-p 'region 'subtree)))
	   ((eq c ?<)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction
		  (cond
		   ((eq restriction 'buffer)
		    (if region-p 'region 'subtree))
		   ((memq restriction '(subtree region))
		    nil)
		   (t 'buffer))))
	   ((eq c ?>)
	    (org-agenda-remove-restriction-lock 'noupdate)
	    (setq restriction nil))
	   ((and (equal selstring "") (memq c '(?s ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/)))
	    (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
           ((and (> (length selstring) 0) (eq c ?\d))
            (delete-window)
            (org-agenda-get-restriction-and-command prefix-descriptions))

	   ((equal c ?q) (error "Abort"))
	   (t (error "Invalid key %c" c))))))))

(defun org-run-agenda-series (name series)
  (org-prepare-agenda name)
  (let* ((org-agenda-multi t)
	 (redo (list 'org-run-agenda-series name (list 'quote series)))
	 (cmds (car series))
	 (gprops (nth 1 series))
	 match ;; The byte compiler incorrectly complains about this.  Keep it!
	 cmd type lprops)
    (while (setq cmd (pop cmds))
      (setq type (car cmd) match (nth 1 cmd) lprops (nth 2 cmd))
      (cond
       ((eq type 'agenda)
	(org-let2 gprops lprops
	  '(call-interactively 'org-agenda-list)))
       ((eq type 'alltodo)
	(org-let2 gprops lprops
	  '(call-interactively 'org-todo-list)))
       ((eq type 'search)
	(org-let2 gprops lprops
		  '(org-search-view current-prefix-arg match nil)))
       ((eq type 'stuck)
	(org-let2 gprops lprops
	  '(call-interactively 'org-agenda-list-stuck-projects)))
       ((eq type 'tags)
	(org-let2 gprops lprops
		  '(org-tags-view current-prefix-arg match)))
       ((eq type 'tags-todo)
	(org-let2 gprops lprops
		  '(org-tags-view '(4) match)))
       ((eq type 'todo)
	(org-let2 gprops lprops
		  '(org-todo-list match)))
       ((fboundp type)
	(org-let2 gprops lprops
	  '(funcall type match)))
       (t (error "Invalid type in command series"))))
    (widen)
    (setq org-agenda-redo-command redo)
    (goto-char (point-min)))
  (org-finalize-agenda))

;;;###autoload
(defmacro org-batch-agenda (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Paramters are alternating variable names and values that will be bound
before running the agenda command."
  (let (pars)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (if (> (length cmd-key) 2)
	(eval (list 'let (nreverse pars)
		    (list 'org-tags-view nil cmd-key)))
      (eval (list 'let (nreverse pars) (list 'org-agenda nil cmd-key))))
    (set-buffer org-agenda-buffer-name)
    (princ (org-encode-for-stdout (buffer-string)))))

(defun org-encode-for-stdout (string)
  (if (fboundp 'encode-coding-string)
      (encode-coding-string string buffer-file-coding-system)
    string))

(defvar org-agenda-info nil)

;;;###autoload
(defmacro org-batch-agenda-csv (cmd-key &rest parameters)
  "Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Paramters are alternating variable names and values that will be bound
before running the agenda command.

The output gives a line for each selected agenda item.  Each
item is a list of comma-separated values, like this:

category,head,type,todo,tags,date,time,extra,priority-l,priority-n

category     The category of the item
head         The headline, without TODO kwd, TAGS and PRIORITY
type         The type of the agenda entry, can be
                todo               selected in TODO match
                tagsmatch          selected in tags match
                diary              imported from diary
                deadline           a deadline on given date
                scheduled          scheduled on given date
                timestamp          entry has timestamp on given date
                closed             entry was closed on given date
                upcoming-deadline  warning about deadline
                past-scheduled     forwarded scheduled item
                block              entry has date block including g. date
todo         The todo keyword, if any
tags         All tags including inherited ones, separated by colons
date         The relevant date, like 2007-2-14
time         The time, like 15:00-16:50
extra        Sting with extra planning info
priority-l   The priority letter if any was given
priority-n   The computed numerical priority
agenda-day   The day in the agenda where this is listed"

  (let (pars)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (push (list 'org-agenda-remove-tags t) pars)
    (if (> (length cmd-key) 2)
	(eval (list 'let (nreverse pars)
		    (list 'org-tags-view nil cmd-key)))
      (eval (list 'let (nreverse pars) (list 'org-agenda nil cmd-key))))
    (set-buffer org-agenda-buffer-name)
    (let* ((lines (org-split-string (buffer-string) "\n"))
	   line)
      (while (setq line (pop lines))
	(catch 'next
	  (if (not (get-text-property 0 'org-category line)) (throw 'next nil))
	  (setq org-agenda-info
		(org-fix-agenda-info (text-properties-at 0 line)))
	  (princ
	   (org-encode-for-stdout
	    (mapconcat 'org-agenda-export-csv-mapper
		       '(org-category txt type todo tags date time-of-day extra
				      priority-letter priority agenda-day)
		      ",")))
	  (princ "\n"))))))

(defun org-fix-agenda-info (props)
  "Make sure all properties on an agenda item have a canonical form,
so the export commands can easily use it."
  (let (tmp re)
    (when (setq tmp (plist-get props 'tags))
      (setq props (plist-put props 'tags (mapconcat 'identity tmp ":"))))
    (when (setq tmp (plist-get props 'date))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	'((format "%4d, %9s %2s, %4s" dayname monthname day year))

	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'date tmp)))
    (when (setq tmp (plist-get props 'day))
      (if (integerp tmp) (setq tmp (calendar-gregorian-from-absolute tmp)))
      (let ((calendar-date-display-form '(year "-" month "-" day)))
	(setq tmp (calendar-date-string tmp)))
      (setq props (plist-put props 'day tmp))
      (setq props (plist-put props 'agenda-day tmp)))
    (when (setq tmp (plist-get props 'txt))
      (when (string-match "\\[#\\([A-Z0-9]\\)\\] ?" tmp)
	(plist-put props 'priority-letter (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (when (and (setq re (plist-get props 'org-todo-regexp))
		 (setq re (concat "\\`\\.*" re " ?"))
		 (string-match re tmp))
	(plist-put props 'todo (match-string 1 tmp))
	(setq tmp (replace-match "" t t tmp)))
      (plist-put props 'txt tmp)))
  props)

(defun org-agenda-export-csv-mapper (prop)
  (let ((res (plist-get org-agenda-info prop)))
    (setq res
	  (cond
	   ((not res) "")
	   ((stringp res) res)
	   (t (prin1-to-string res))))
    (while (string-match "," res)
      (setq res (replace-match ";" t t res)))
    (org-trim res)))


;;;###autoload
(defun org-store-agenda-views (&rest parameters)
  (interactive)
  (eval (list 'org-batch-store-agenda-views)))

;; FIXME, why is this a macro?????
;;;###autoload
(defmacro org-batch-store-agenda-views (&rest parameters)
  "Run all custom agenda commands that have a file argument."
  (let ((cmds (org-agenda-normalize-custom-commands org-agenda-custom-commands))
	(pop-up-frames nil)
	(dir default-directory)
	pars cmd thiscmdkey files opts)
    (while parameters
      (push (list (pop parameters) (if parameters (pop parameters))) pars))
    (setq pars (reverse pars))
    (save-window-excursion
      (while cmds
	(setq cmd (pop cmds)
	      thiscmdkey (car cmd)
	      opts (nth 4 cmd)
	      files (nth 5 cmd))
	(if (stringp files) (setq files (list files)))
	(when files
	  (eval (list 'let (append org-agenda-exporter-settings opts pars)
		      (list 'org-agenda nil thiscmdkey)))
	  (set-buffer org-agenda-buffer-name)
	  (while files
	    (eval (list 'let (append org-agenda-exporter-settings opts pars)
			(list 'org-write-agenda
			      (expand-file-name (pop files) dir) t))))
	  (and (get-buffer org-agenda-buffer-name)
	       (kill-buffer org-agenda-buffer-name)))))))

(defun org-write-agenda (file &optional nosettings)
  "Write the current buffer (an agenda view) as a file.
Depending on the extension of the file name, plain text (.txt),
HTML (.html or .htm) or Postscript (.ps) is produced.
If the extension is .ics, run icalendar export over all files used
to construct the agenda and limit the export to entries listed in the
agenda now.
If NOSETTINGS is given, do not scope the settings of
`org-agenda-exporter-settings' into the export commands.  This is used when
the settings have already been scoped and we do not wish to overrule other,
higher priority settings."
  (interactive "FWrite agenda to file: ")
  (if (not (file-writable-p file))
      (error "Cannot write agenda to file %s" file))
  (cond
   ((string-match "\\.html?\\'" file) (require 'htmlize))
   ((string-match "\\.ps\\'" file) (require 'ps-print)))
  (org-let (if nosettings nil org-agenda-exporter-settings)
    '(save-excursion
       (save-window-excursion
	 (cond
	  ((string-match "\\.html?\\'" file)
	   (set-buffer (htmlize-buffer (current-buffer)))

	   (when (and org-agenda-export-html-style
		      (string-match "<style>" org-agenda-export-html-style))
	     ;; replace <style> section with org-agenda-export-html-style
	     (goto-char (point-min))
	     (kill-region (- (search-forward "<style") 6)
			  (search-forward "</style>"))
	     (insert org-agenda-export-html-style))
	   (write-file file)
	   (kill-buffer (current-buffer))
	   (message "HTML written to %s" file))
	  ((string-match "\\.ps\\'" file)
	   (ps-print-buffer-with-faces file)
	   (message "Postscript written to %s" file))
	  ((string-match "\\.ics\\'" file)
	   (let ((org-agenda-marker-table
		  (org-create-marker-find-array
		   (org-agenda-collect-markers)))
		 (org-icalendar-verify-function 'org-check-agenda-marker-table)
		 (org-combined-agenda-icalendar-file file))
	     (apply 'org-export-icalendar 'combine (org-agenda-files))))
	  (t
	   (let ((bs (buffer-string)))
	     (find-file file)
	     (insert bs)
	     (save-buffer 0)
	     (kill-buffer (current-buffer))
	     (message "Plain text written to %s" file))))))
    (set-buffer org-agenda-buffer-name)))

(defun org-agenda-collect-markers ()
  "Collect the markers pointing to entries in the agenda buffer."
  (let (m markers)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (setq m (or (get-text-property (point) 'org-hd-marker)
			  (get-text-property (point) 'org-marker)))
	  (push m markers))
	(beginning-of-line 2)))
    (nreverse markers)))

(defun org-create-marker-find-array (marker-list)
  "Create a alist of files names with all marker positions in that file."
  (let (f tbl m a p)
    (while (setq m (pop marker-list))
      (setq p (marker-position m)
	    f (buffer-file-name (or (buffer-base-buffer
				     (marker-buffer m))
				    (marker-buffer m))))
      (if (setq a (assoc f tbl))
	  (push (marker-position m) (cdr a))
	(push (list f p) tbl)))
    (mapcar (lambda (x) (setcdr x (sort (copy-sequence (cdr x)) '<)) x)
	    tbl)))

(defvar org-agenda-marker-table nil) ; dyamically scoped parameter
(defun org-check-agenda-marker-table ()
  "Check of the current entry is on the marker list."
  (let ((file (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
	a)
    (and (setq a (assoc file org-agenda-marker-table))
	 (save-match-data
	   (save-excursion
	     (org-back-to-heading t)
	     (member (point) (cdr a)))))))

(defun org-check-for-org-mode ()
  "Make sure current buffer is in org-mode.  Error if not."
  (or (org-mode-p)
      (error "Cannot execute org-mode agenda command on buffer in %s."
	     major-mode)))

(defun org-fit-agenda-window ()
  "Fit the window to the buffer size."
  (and (memq org-agenda-window-setup '(reorganize-frame))
       (fboundp 'fit-window-to-buffer)
       (fit-window-to-buffer
	nil
	(floor (* (frame-height) (cdr org-agenda-window-frame-fractions)))
	(floor (* (frame-height) (car org-agenda-window-frame-fractions))))))

;;; Agenda prepare and finalize

(defvar org-agenda-multi nil)  ; dynammically scoped
(defvar org-agenda-buffer-name "*Org Agenda*")
(defvar org-pre-agenda-window-conf nil)
(defvar org-agenda-columns-active nil)
(defvar org-agenda-name nil)
(defun org-prepare-agenda (&optional name)
  (setq org-todo-keywords-for-agenda nil)
  (setq org-done-keywords-for-agenda nil)
  (if org-agenda-multi
      (progn
	(setq buffer-read-only nil)
	(goto-char (point-max))
	(unless (or (bobp) org-agenda-compact-blocks)
	  (insert "\n" (make-string (window-width) ?=) "\n"))
	(narrow-to-region (point) (point-max)))
    (org-agenda-reset-markers)
    (setq org-agenda-contributing-files nil)
    (setq org-agenda-columns-active nil)
    (org-prepare-agenda-buffers (org-agenda-files))
    (setq org-todo-keywords-for-agenda
	  (org-uniquify org-todo-keywords-for-agenda))
    (setq org-done-keywords-for-agenda
	  (org-uniquify org-done-keywords-for-agenda))
    (let* ((abuf (get-buffer-create org-agenda-buffer-name))
	   (awin (get-buffer-window abuf)))
      (cond
       ((equal (current-buffer) abuf) nil)
       (awin (select-window awin))
       ((not (setq org-pre-agenda-window-conf (current-window-configuration))))
       ((equal org-agenda-window-setup 'current-window)
	(switch-to-buffer abuf))
       ((equal org-agenda-window-setup 'other-window)
	(org-switch-to-buffer-other-window abuf))
       ((equal org-agenda-window-setup 'other-frame)
	(switch-to-buffer-other-frame abuf))
       ((equal org-agenda-window-setup 'reorganize-frame)
	(delete-other-windows)
	(org-switch-to-buffer-other-window abuf))))
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t)) (erase-buffer))
    (org-agenda-mode)
    (and name (not org-agenda-name)
	 (org-set-local 'org-agenda-name name)))
  (setq buffer-read-only nil))

(defun org-finalize-agenda ()
  "Finishing touch for the agenda buffer, called just before displaying it."
  (unless org-agenda-multi
    (save-excursion
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(while (org-activate-bracket-links (point-max))
	  (add-text-properties (match-beginning 0) (match-end 0)
			       '(face org-link)))
	(org-agenda-align-tags)
	(unless org-agenda-with-colors
	  (remove-text-properties (point-min) (point-max) '(face nil))))
      (if (and (boundp 'org-overriding-columns-format)
	       org-overriding-columns-format)
	  (org-set-local 'org-overriding-columns-format
			 org-overriding-columns-format))
      (if (and (boundp 'org-agenda-view-columns-initially)
	       org-agenda-view-columns-initially)
	  (org-agenda-columns))
      (when org-agenda-fontify-priorities
	(org-fontify-priorities))
      (run-hooks 'org-finalize-agenda-hook)
      (setq org-agenda-type (get-text-property (point) 'org-agenda-type))
      )))

(defun org-fontify-priorities ()
  "Make highest priority lines bold, and lowest italic."
  (interactive)
  (mapc (lambda (o) (if (eq (org-overlay-get o 'org-type) 'org-priority)
			(org-delete-overlay o)))
	(org-overlays-in (point-min) (point-max)))
  (save-excursion
    (let ((inhibit-read-only t)
	  b e p ov h l)
      (goto-char (point-min))
      (while (re-search-forward "\\[#\\(.\\)\\]" nil t)
	(setq h (or (get-char-property (point) 'org-highest-priority)
		    org-highest-priority)
	      l (or (get-char-property (point) 'org-lowest-priority)
		    org-lowest-priority)
	      p (string-to-char (match-string 1))
	      b (match-beginning 0) e (point-at-eol)
	      ov (org-make-overlay b e))
	(org-overlay-put
	 ov 'face
	 (cond ((listp org-agenda-fontify-priorities)
		(cdr (assoc p org-agenda-fontify-priorities)))
	       ((equal p l) 'italic)
	       ((equal p h) 'bold)))
	(org-overlay-put ov 'org-type 'org-priority)))))


(defvar org-agenda-skip-function nil
  "Function to be called at each match during agenda construction.
If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued.
This may also be a Lisp form, it will be evaluated.
Never set this variable using `setq' or so, because then it will apply
to all future agenda commands.  Instead, bind it with `let' to scope
it dynamically into the agenda-constructing command.  A good way to set
it is through options in org-agenda-custom-commands.")

(defun org-agenda-skip ()
  "Throw to `:skip' in places that should be skipped.
Also moves point to the end of the skipped region, so that search can
continue from there."
  (let ((p (point-at-bol)) to fp)
    (and org-agenda-skip-archived-trees
	 (get-text-property p :org-archived)
	 (org-end-of-subtree t)
	 (throw :skip t))
    (and (get-text-property p :org-comment)
	 (org-end-of-subtree t)
	 (throw :skip t))
    (if (equal (char-after p) ?#) (throw :skip t))
    (when (and (or (setq fp (functionp org-agenda-skip-function))
		   (consp org-agenda-skip-function))
	       (setq to (save-excursion
			  (save-match-data
			    (if fp
				(funcall org-agenda-skip-function)
			      (eval org-agenda-skip-function))))))
      (goto-char to)
      (throw :skip t))))

(defvar org-agenda-markers nil
  "List of all currently active markers created by `org-agenda'.")
(defvar org-agenda-last-marker-time (time-to-seconds (current-time))
  "Creation time of the last agenda marker.")

(defun org-agenda-new-marker (&optional pos)
  "Return a new agenda marker.
Org-mode keeps a list of these markers and resets them when they are
no longer in use."
  (let ((m (copy-marker (or pos (point)))))
    (setq org-agenda-last-marker-time (time-to-seconds (current-time)))
    (push m org-agenda-markers)
    m))

(defun org-agenda-reset-markers ()
  "Reset markers created by `org-agenda'."
  (while org-agenda-markers
    (move-marker (pop org-agenda-markers) nil)))

;;; Agenda timeline

(defvar org-agenda-only-exact-dates nil) ; dynamically scoped

(defun org-timeline (&optional include-all)
  "Show a time-sorted view of the entries in the current org file.
Only entries with a time stamp of today or later will be listed.  With
\\[universal-argument] prefix, all unfinished TODO items will also be shown,
under the current date.
If the buffer contains an active region, only check the region for
dates."
  (interactive "P")
  (require 'calendar)
  (org-compile-prefix-format 'timeline)
  (org-set-sorting-strategy 'timeline)
  (let* ((dopast t)
	 (dotodo include-all)
	 (doclosed org-agenda-show-log)
	 (entry buffer-file-name)
	 (date (calendar-current-date))
	 (beg (if (org-region-active-p) (region-beginning) (point-min)))
	 (end (if (org-region-active-p) (region-end) (point-max)))
	 (day-numbers (org-get-all-dates beg end 'no-ranges
					 t doclosed ; always include today
					 org-timeline-show-empty-dates))
	 (org-deadline-warning-days 0)
	 (org-agenda-only-exact-dates t)
	 (today (time-to-days (current-time)))
	 (past t)
	 args
	 s e rtn d emptyp wd)
    (setq org-agenda-redo-command
	  (list 'progn
		(list 'org-switch-to-buffer-other-window (current-buffer))
		(list 'org-timeline (list 'quote include-all))))
    (if (not dopast)
	;; Remove past dates from the list of dates.
	(setq day-numbers (delq nil (mapcar (lambda(x)
					      (if (>= x today) x nil))
					    day-numbers))))
    (org-prepare-agenda (concat "Timeline "
				(file-name-nondirectory buffer-file-name)))
    (if doclosed (push :closed args))
    (push :timestamp args)
    (push :deadline args)
    (push :scheduled args)
    (push :sexp args)
    (if dotodo (push :todo args))
    (while (setq d (pop day-numbers))
      (if (and (listp d) (eq (car d) :omitted))
	  (progn
	    (setq s (point))
	    (insert (format "\n[... %d empty days omitted]\n\n" (cdr d)))
	    (put-text-property s (1- (point)) 'face 'org-agenda-structure))
	(if (listp d) (setq d (car d) emptyp t) (setq emptyp nil))
	(if (and (>= d today)
		 dopast
		 past)
	    (progn
	      (setq past nil)
	      (insert (make-string 79 ?-) "\n")))
	(setq date (calendar-gregorian-from-absolute d)
	      wd (calendar-day-of-week date))
	(setq s (point))
	(setq rtn (and (not emptyp)
		       (apply 'org-agenda-get-day-entries entry
			      date args)))
	(if (or rtn (equal d today) org-timeline-show-empty-dates)
	    (progn
	      (insert
	       (if (stringp org-agenda-format-date)
		   (format-time-string org-agenda-format-date
				       (org-time-from-absolute date))
		 (funcall org-agenda-format-date date))
	       "\n")
	      (put-text-property s (1- (point)) 'face
				 (if (member wd org-agenda-weekend-days)
				     'org-agenda-date-weekend
				   'org-agenda-date))
	      (put-text-property s (1- (point)) 'org-date-line t)
	      (if (equal d today)
		  (put-text-property s (1- (point)) 'org-today t))
	      (and rtn (insert (org-finalize-agenda-entries rtn) "\n"))
	      (put-text-property s (1- (point)) 'day d)))))
    (goto-char (point-min))
    (goto-char (or (text-property-any (point-min) (point-max) 'org-today t)
		   (point-min)))
    (add-text-properties (point-min) (point-max) '(org-agenda-type timeline))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

(defun org-get-all-dates (beg end &optional no-ranges force-today inactive empty pre-re)
  "Return a list of all relevant day numbers from BEG to END buffer positions.
If NO-RANGES is non-nil, include only the start and end dates of a range,
not every single day in the range.  If FORCE-TODAY is non-nil, make
sure that TODAY is included in the list.  If INACTIVE is non-nil, also
inactive time stamps (those in square brackets) are included.
When EMPTY is non-nil, also include days without any entries."
  (let ((re (concat
	     (if pre-re pre-re "")
	     (if inactive org-ts-regexp-both org-ts-regexp)))
	 dates dates1 date day day1 day2 ts1 ts2)
    (if force-today
	(setq dates (list (time-to-days (current-time)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq day (time-to-days (org-time-string-to-time
				 (substring (match-string 1) 0 10))))
	(or (memq day dates) (push day dates)))
      (unless no-ranges
	(goto-char beg)
	(while (re-search-forward org-tr-regexp end t)
	  (setq ts1 (substring (match-string 1) 0 10)
		ts2 (substring (match-string 2) 0 10)
		day1 (time-to-days (org-time-string-to-time ts1))
		day2 (time-to-days (org-time-string-to-time ts2)))
	  (while (< (setq day1 (1+ day1)) day2)
	    (or (memq day1 dates) (push day1 dates)))))
      (setq dates (sort dates '<))
      (when empty
	(while (setq day (pop dates))
	  (setq day2 (car dates))
	  (push day dates1)
	  (when (and day2 empty)
	    (if (or (eq empty t)
		    (and (numberp empty) (<= (- day2 day) empty)))
		(while (< (setq day (1+ day)) day2)
		  (push (list day) dates1))
	      (push (cons :omitted (- day2 day)) dates1))))
	(setq dates (nreverse dates1)))
      dates)))

;;; Agenda Daily/Weekly

(defvar org-agenda-overriding-arguments nil) ; dynamically scoped parameter
(defvar org-agenda-start-day nil) ; dynamically scoped parameter
(defvar org-agenda-last-arguments nil
  "The arguments of the previous call to org-agenda")
(defvar org-starting-day nil) ; local variable in the agenda buffer
(defvar org-agenda-span nil) ; local variable in the agenda buffer
(defvar org-include-all-loc nil) ; local variable
(defvar org-agenda-remove-date nil) ; dynamically scoped FIXME: not used???

;;;###autoload
(defun org-agenda-list (&optional include-all start-day ndays)
  "Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With one \\[universal-argument] prefix argument INCLUDE-ALL,
all unfinished TODO items will also be shown, before the agenda.
This feature is considered obsolete, please use the TODO list or a block
agenda instead.

With a numeric prefix argument in an interactive call, the agenda will
span INCLUDE-ALL days.  Lisp programs should instead specify NDAYS to change
the number of days.  NDAYS defaults to `org-agenda-ndays'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'."
  (interactive "P")
  (if (and (integerp include-all) (> include-all 0))
      (setq ndays include-all include-all nil))
  (setq ndays (or ndays org-agenda-ndays)
	start-day (or start-day org-agenda-start-day))
  (if org-agenda-overriding-arguments
      (setq include-all (car org-agenda-overriding-arguments)
	    start-day (nth 1 org-agenda-overriding-arguments)
	    ndays (nth 2 org-agenda-overriding-arguments)))
  (if (stringp start-day)
      ;; Convert to an absolute day number
      (setq start-day (time-to-days (org-read-date nil t start-day))))
  (setq org-agenda-last-arguments (list include-all start-day ndays))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (require 'calendar)
  (let* ((org-agenda-start-on-weekday
	  (if (or (equal ndays 7) (and (null ndays) (equal 7 org-agenda-ndays)))
	      org-agenda-start-on-weekday nil))
	 (thefiles (org-agenda-files))
	 (files thefiles)
	 (today (time-to-days
		 (time-subtract (current-time)
				(list 0 (* 3600 org-extend-today-until) 0))))
	 (sd (or start-day today))
	 (start (if (or (null org-agenda-start-on-weekday)
			(< org-agenda-ndays 7))
		    sd
		  (let* ((nt (calendar-day-of-week
			      (calendar-gregorian-from-absolute sd)))
			 (n1 org-agenda-start-on-weekday)
			 (d (- nt n1)))
		    (- sd (+ (if (< d 0) 7 0) d)))))
	 (day-numbers (list start))
	 (day-cnt 0)
	 (inhibit-redisplay (not debug-on-error))
	 s e rtn rtnall file date d start-pos end-pos todayp nd wd
	 clocktable-start clocktable-end)
    (setq org-agenda-redo-command
	  (list 'org-agenda-list (list 'quote include-all) start-day ndays))
    ;; Make the list of days
    (setq ndays (or ndays org-agenda-ndays)
	  nd ndays)
    (while (> ndays 1)
      (push (1+ (car day-numbers)) day-numbers)
      (setq ndays (1- ndays)))
    (setq day-numbers (nreverse day-numbers))
    (setq clocktable-start (car day-numbers)
	  clocktable-end (1+ (or (org-last day-numbers) 0)))
    (org-prepare-agenda "Day/Week")
    (org-set-local 'org-starting-day (car day-numbers))
    (org-set-local 'org-include-all-loc include-all)
    (org-set-local 'org-agenda-span
		   (org-agenda-ndays-to-span nd))
    (when (and (or include-all org-agenda-include-all-todo)
	       (member today day-numbers))
      (setq files thefiles
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq date (calendar-gregorian-from-absolute today)
		rtn (org-agenda-get-day-entries
		     file date :todo))
	  (setq rtnall (append rtnall rtn))))
      (when rtnall
	(insert "ALL CURRENTLY OPEN TODO ITEMS:\n")
	(add-text-properties (point-min) (1- (point))
			     (list 'face 'org-agenda-structure))
	(insert (org-finalize-agenda-entries rtnall) "\n")))
    (unless org-agenda-compact-blocks
      (let* ((d1 (car day-numbers))
	     (d2 (org-last day-numbers))
	     (w1 (org-days-to-iso-week d1))
	     (w2 (org-days-to-iso-week d2)))
	(setq s (point))
	(insert (capitalize (symbol-name (org-agenda-ndays-to-span nd)))
		"-agenda"
		(if (< (- d2 d1) 350)
		    (if (= w1 w2)
			(format " (W%02d)" w1)
		      (format " (W%02d-W%02d)" w1 w2))
		  "")
		":\n"))
      (add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
						'org-date-line t)))
    (while (setq d (pop day-numbers))
      (setq date (calendar-gregorian-from-absolute d)
	    wd (calendar-day-of-week date)
	    s (point))
      (if (or (setq todayp (= d today))
	      (and (not start-pos) (= d sd)))
	  (setq start-pos (point))
	(if (and start-pos (not end-pos))
	    (setq end-pos (point))))
      (setq files thefiles
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (if org-agenda-show-log
	      (setq rtn (org-agenda-get-day-entries
			 file date
			 :deadline :scheduled :timestamp :sexp :closed))
	    (setq rtn (org-agenda-get-day-entries
		       file date
		       :deadline :scheduled :sexp :timestamp)))
	  (setq rtnall (append rtnall rtn))))
      (if org-agenda-include-diary
	  (progn
	    (require 'diary-lib)
	    (setq rtn (org-get-entries-from-diary date))
	    (setq rtnall (append rtnall rtn))))
      (if (or rtnall org-agenda-show-all-dates)
	  (progn
	    (setq day-cnt (1+ day-cnt))
	    (insert
	     (if (stringp org-agenda-format-date)
		 (format-time-string org-agenda-format-date
				     (org-time-from-absolute date))
	       (funcall org-agenda-format-date date))
	     "\n")
	    (put-text-property s (1- (point)) 'face
			       (if (member wd org-agenda-weekend-days)
				   'org-agenda-date-weekend
				 'org-agenda-date))
	    (put-text-property s (1- (point)) 'org-date-line t)
	    (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
	    (if todayp (put-text-property s (1- (point)) 'org-today t))
	    (if rtnall (insert
			(org-finalize-agenda-entries
			 (org-agenda-add-time-grid-maybe
			  rtnall nd todayp))
			"\n"))
	    (put-text-property s (1- (point)) 'day d)
	    (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))
    (when (and org-agenda-clockreport-mode clocktable-start)
      (let ((org-agenda-files (org-agenda-files))
	    ;; the above line is to ensure the restricted range!
	    (p org-agenda-clockreport-parameter-plist)
	    tbl)
	(setq p (org-plist-delete p :block))
	(setq p (plist-put p :tstart clocktable-start))
	(setq p (plist-put p :tend clocktable-end))
	(setq p (plist-put p :scope 'agenda))
	(setq tbl (apply 'org-get-clocktable p))
	(insert tbl)))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (unless (and (pos-visible-in-window-p (point-min))
		 (pos-visible-in-window-p (point-max)))
      (goto-char (1- (point-max)))
      (recenter -1)
      (if (not (pos-visible-in-window-p (or start-pos 1)))
	  (progn
	    (goto-char (or start-pos 1))
	    (recenter 1))))
    (goto-char (or start-pos 1))
    (add-text-properties (point-min) (point-max) '(org-agenda-type agenda))
    (org-finalize-agenda)
    (setq buffer-read-only t)
    (message "")))

(defun org-agenda-ndays-to-span (n)
  (cond ((< n 7) 'day) ((= n 7) 'week) ((< n 32) 'month) (t 'year)))

;;; Agenda word search

(defvar org-agenda-search-history nil)
(defvar org-todo-only nil)

(defvar org-search-syntax-table nil
  "Special syntax table for org-mode search.
In this table, we have single quotes not as word constituents, to
that when \"+Ameli\" is searchd as a work, it will also match \"Ameli's\"")

(defun org-search-syntax-table ()
  (unless org-search-syntax-table
    (setq org-search-syntax-table (copy-syntax-table org-mode-syntax-table))
    (modify-syntax-entry ?' "." org-search-syntax-table)
    (modify-syntax-entry ?` "." org-search-syntax-table))
  org-search-syntax-table)

;;;###autoload
(defun org-search-view (&optional todo-only string edit-at)
  "Show all entries that contain words or regular expressions.
If the first character of the search string is an asterisks,
search only the headlines.

With optional prefix argument TODO-ONLY, only consider entries that are
TODO entries.  The argument STRING can be used to pass a default search
string into this function.  If EDIT-AT is non-nil, it means that the
user should get a chance to edit this string, with cursor at position
EDIT-AT.

The search string is broken into \"words\" by splitting at whitespace.
The individual words are then interpreted as a boolean expression with
logical AND.  Words prefixed with a minus must not occur in the entry.
Words without a prefix or prefixed with a plus must occur in the entry.
Matching is case-insensitive and the words are enclosed by word delimiters.

Words enclosed by curly braces are interpreted as regular expressions
that must or must not match in the entry.

If the search string starts with an asterisk, search only in headlines.
If (possibly after the leading star) the search string starts with an
exclamation mark, this also means to look at TODO entries only, an effect
that can also be achieved with a prefix argument.

This command searches the agenda files, and in addition the files listed
in `org-agenda-text-search-extra-files'."
  (interactive "P")
  (org-compile-prefix-format 'search)
  (org-set-sorting-strategy 'search)
  (org-prepare-agenda "SEARCH")
  (let* ((props (list 'face nil
		      'done-face 'org-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo (format "mouse-2 or RET jump to location")))
	 regexp rtn rtnall files file pos
	 marker priority category tags c neg re
	 ee txt beg end words regexps+ regexps- hdl-only buffer beg1 str)
    (unless (and (not edit-at)
		 (stringp string)
		 (string-match "\\S-" string))
      (setq string (read-string "[+-]Word/{Regexp} ...: "
				(cond
				 ((integerp edit-at) (cons string edit-at))
				 (edit-at string))
				'org-agenda-search-history)))
    (org-set-local 'org-todo-only todo-only)
    (setq org-agenda-redo-command
	  (list 'org-search-view (if todo-only t nil) string
		'(if current-prefix-arg 1 nil)))
    (setq org-agenda-query-string string)

    (if (equal (string-to-char string) ?*)
	(setq hdl-only t
	      words (substring string 1))
      (setq words string))
    (when (equal (string-to-char words) ?!)
      (setq todo-only t
	    words (substring words 1)))
    (setq words (org-split-string words))
    (mapc (lambda (w)
	    (setq c (string-to-char w))
	    (if (equal c ?-)
		(setq neg t w (substring w 1))
	      (if (equal c ?+)
		  (setq neg nil w (substring w 1))
		(setq neg nil)))
	    (if (string-match "\\`{.*}\\'" w)
		(setq re (substring w 1 -1))
	      (setq re (concat "\\<" (regexp-quote (downcase w)) "\\>")))
	    (if neg (push re regexps-) (push re regexps+)))
	  words)
    (setq regexps+ (sort regexps+ (lambda (a b) (> (length a) (length b)))))
    (if (not regexps+)
	(setq regexp (concat "^" org-outline-regexp))
      (setq regexp (pop regexps+))
      (if hdl-only (setq regexp (concat "^" org-outline-regexp ".*?"
					regexp))))
    (setq files (org-agenda-files))
    (when (eq (car org-agenda-text-search-extra-files) 'agenda-archives)
      (pop org-agenda-text-search-extra-files)
      (setq files (org-add-archive-files files)))
    (setq files (append files org-agenda-text-search-extra-files)
	  rtnall nil)
    (while (setq file (pop files))
      (setq ee nil)
      (catch 'nextfile
	(org-check-agenda-file file)
	(setq buffer (if (file-exists-p file)
			 (org-get-agenda-file-buffer file)
		       (error "No such file %s" file)))
	(if (not buffer)
	    ;; If file does not exist, make sure an error message is sent
	    (setq rtn (list (format "ORG-AGENDA-ERROR: No such org-file %s"
				    file))))
	(with-current-buffer buffer
	  (with-syntax-table (org-search-syntax-table)
	    (unless (org-mode-p)
	      (error "Agenda file %s is not in `org-mode'" file))
	    (let ((case-fold-search t))
	      (save-excursion
		(save-restriction
		  (if org-agenda-restrict
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (goto-char (point-min))
		  (unless (or (org-on-heading-p)
			      (outline-next-heading))
		    (throw 'nextfile t))
		  (goto-char (max (point-min) (1- (point))))
		  (while (re-search-forward regexp nil t)
		    (org-back-to-heading t)
		    (skip-chars-forward "* ")
		    (setq beg (point-at-bol)
			  beg1 (point)
			  end (progn (outline-next-heading) (point)))
		    (catch :skip
		      (goto-char beg)
		      (org-agenda-skip)
		      (setq str (buffer-substring-no-properties
				 (point-at-bol)
				 (if hdl-only (point-at-eol) end)))
		      (mapc (lambda (wr) (when (string-match wr str)
					   (goto-char (1- end))
					   (throw :skip t)))
			    regexps-)
		      (mapc (lambda (wr) (unless (string-match wr str)
					   (goto-char (1- end))
					   (throw :skip t)))
			    (if todo-only
				(cons (concat "^\*+[ \t]+" org-not-done-regexp)
				      regexps+)
			      regexps+))
		      (goto-char beg)
		      (setq marker (org-agenda-new-marker (point))
			    category (org-get-category)
			    tags (org-get-tags-at (point))
			    txt (org-format-agenda-item
				 ""
				 (buffer-substring-no-properties
				  beg1 (point-at-eol))
				 category tags))
		      (org-add-props txt props
			'org-marker marker 'org-hd-marker marker
			'org-todo-regexp org-todo-regexp
			'priority 1000 'org-category category
			'type "search")
		      (push txt ee)
		      (goto-char (1- end))))))))))
      (setq rtn (nreverse ee))
      (setq rtnall (append rtnall rtn)))
    (if org-agenda-overriding-header
	(insert (org-add-props (copy-sequence org-agenda-overriding-header)
		    nil 'face 'org-agenda-structure) "\n")
      (insert "Search words: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure))
      (setq pos (point))
      (insert string "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-warning))
      (setq pos (point))
      (unless org-agenda-multi
	(insert "Press `[', `]' to add/sub word, `{', `}' to add/sub regexp, `C-u r' to edit\n")
	(add-text-properties pos (1- (point))
			     (list 'face 'org-agenda-structure))))
    (when rtnall
      (insert (org-finalize-agenda-entries rtnall) "\n"))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (add-text-properties (point-min) (point-max) '(org-agenda-type search))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

;;; Agenda TODO list

(defvar org-select-this-todo-keyword nil)
(defvar org-last-arg nil)

;;;###autoload
(defun org-todo-list (arg)
  "Show all TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using \\[universal-argument], you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'."
  (interactive "P")
  (require 'calendar)
  (org-compile-prefix-format 'todo)
  (org-set-sorting-strategy 'todo)
  (org-prepare-agenda "TODO")
  (let* ((today (time-to-days (current-time)))
	 (date (calendar-gregorian-from-absolute today))
	 (kwds org-todo-keywords-for-agenda)
	 (completion-ignore-case t)
	 (org-select-this-todo-keyword
	  (if (stringp arg) arg
	    (and arg (integerp arg) (> arg 0)
                 (nth (1- arg) kwds))))
	 rtn rtnall files file pos)
    (when (equal arg '(4))
      (setq org-select-this-todo-keyword
	    (completing-read "Keyword (or KWD1|K2D2|...): "
			     (mapcar 'list kwds) nil nil)))
    (and (equal 0 arg) (setq org-select-this-todo-keyword nil))
    (org-set-local 'org-last-arg arg)
    (setq org-agenda-redo-command
	  '(org-todo-list (or current-prefix-arg org-last-arg)))
    (setq files (org-agenda-files)
	  rtnall nil)
    (while (setq file (pop files))
      (catch 'nextfile
	(org-check-agenda-file file)
	(setq rtn (org-agenda-get-day-entries file date :todo))
	(setq rtnall (append rtnall rtn))))
    (if org-agenda-overriding-header
	(insert (org-add-props (copy-sequence org-agenda-overriding-header)
		    nil 'face 'org-agenda-structure) "\n")
      (insert "Global list of TODO items of type: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure))
      (setq pos (point))
      (insert (or org-select-this-todo-keyword "ALL") "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-warning))
      (setq pos (point))
      (unless org-agenda-multi
	(insert "Available with `N r': (0)ALL")
	(let ((n 0) s)
	  (mapc (lambda (x)
		  (setq s (format "(%d)%s" (setq n (1+ n)) x))
		  (if (> (+ (current-column) (string-width s) 1) (frame-width))
		      (insert "\n                     "))
		  (insert " " s))
		kwds))
	(insert "\n"))
      (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
    (when rtnall
      (insert (org-finalize-agenda-entries rtnall) "\n"))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (add-text-properties (point-min) (point-max) '(org-agenda-type todo))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

;;; Agenda tags match

;;;###autoload
(defun org-tags-view (&optional todo-only match)
  "Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries."
  (interactive "P")
  (org-compile-prefix-format 'tags)
  (org-set-sorting-strategy 'tags)
  (let* ((org-tags-match-list-sublevels
	  (if todo-only t org-tags-match-list-sublevels))
	 (completion-ignore-case t)
	 rtn rtnall files file pos matcher
	 buffer)
    (setq matcher (org-make-tags-matcher match)
	  match (car matcher) matcher (cdr matcher))
    (org-prepare-agenda (concat "TAGS " match))
    (setq org-agenda-query-string match)
    (setq org-agenda-redo-command
	  (list 'org-tags-view (list 'quote todo-only)
		(list 'if 'current-prefix-arg nil 'org-agenda-query-string)))
    (setq files (org-agenda-files)
	  rtnall nil)
    (while (setq file (pop files))
      (catch 'nextfile
	(org-check-agenda-file file)
	(setq buffer (if (file-exists-p file)
			 (org-get-agenda-file-buffer file)
		       (error "No such file %s" file)))
	(if (not buffer)
	    ;; If file does not exist, merror message to agenda
	    (setq rtn (list
		       (format "ORG-AGENDA-ERROR: No such org-file %s" file))
		  rtnall (append rtnall rtn))
	  (with-current-buffer buffer
	    (unless (org-mode-p)
	      (error "Agenda file %s is not in `org-mode'" file))
	    (save-excursion
	      (save-restriction
		(if org-agenda-restrict
		    (narrow-to-region org-agenda-restrict-begin
				      org-agenda-restrict-end)
		  (widen))
		(setq rtn (org-scan-tags 'agenda matcher todo-only))
		(setq rtnall (append rtnall rtn))))))))
    (if org-agenda-overriding-header
	(insert (org-add-props (copy-sequence org-agenda-overriding-header)
		    nil 'face 'org-agenda-structure) "\n")
      (insert "Headlines with TAGS match: ")
      (add-text-properties (point-min) (1- (point))
			   (list 'face 'org-agenda-structure))
      (setq pos (point))
      (insert match "\n")
      (add-text-properties pos (1- (point)) (list 'face 'org-warning))
      (setq pos (point))
      (unless org-agenda-multi
	(insert "Press `C-u r' to search again with new search string\n"))
      (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
    (when rtnall
      (insert (org-finalize-agenda-entries rtnall) "\n"))
    (goto-char (point-min))
    (org-fit-agenda-window)
    (add-text-properties (point-min) (point-max) '(org-agenda-type tags))
    (org-finalize-agenda)
    (setq buffer-read-only t)))

;;; Agenda Finding stuck projects

(defvar org-agenda-skip-regexp nil
  "Regular expression used in skipping subtrees for the agenda.
This is basically a temporary global variable that can be set and then
used by user-defined selections using `org-agenda-skip-function'.")

(defvar org-agenda-overriding-header nil
  "When this is set during todo and tags searches, will replace header.")

(defun org-agenda-skip-subtree-when-regexp-matches ()
  "Checks if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this tree, causing agenda commands
to skip this subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command."
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-entry-if (&rest conditions)
  "Skip entry if any of CONDITIONS is true.
See `org-agenda-skip-if' for details."
  (org-agenda-skip-if nil conditions))

(defun org-agenda-skip-subtree-if (&rest conditions)
  "Skip entry if any of CONDITIONS is true.
See `org-agenda-skip-if' for details."
  (org-agenda-skip-if t conditions))

(defun org-agenda-skip-if (subtree conditions)
  "Checks current entity for CONDITIONS.
If SUBTREE is non-nil, the entire subtree is checked.  Otherwise, only
the entry, i.e. the text before the next heading is checked.

CONDITIONS is a list of symbols, boolean OR is used to combine the results
from different tests.  Valid conditions are:

scheduled     Check if there is a scheduled cookie
notscheduled  Check if there is no scheduled cookie
deadline      Check if there is a deadline
notdeadline   Check if there is no deadline
regexp        Check if regexp matches
notregexp     Check if regexp does not match.

The regexp is taken from the conditions list, it must come right after
the `regexp' or `notregexp' element.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (let (beg end m)
    (org-back-to-heading t)
    (setq beg (point)
	  end (if subtree
		  (progn (org-end-of-subtree t) (point))
		(progn (outline-next-heading) (1- (point)))))
    (goto-char beg)
    (and
     (or
      (and (memq 'scheduled conditions)
	   (re-search-forward org-scheduled-time-regexp end t))
      (and (memq 'notscheduled conditions)
	   (not (re-search-forward org-scheduled-time-regexp end t)))
      (and (memq 'deadline conditions)
	   (re-search-forward org-deadline-time-regexp end t))
      (and (memq 'notdeadline conditions)
	   (not (re-search-forward org-deadline-time-regexp end t)))
      (and (setq m (memq 'regexp conditions))
	   (stringp (nth 1 m))
	   (re-search-forward (nth 1 m) end t))
      (and (setq m (memq 'notregexp conditions))
	   (stringp (nth 1 m))
	   (not (re-search-forward (nth 1 m) end t))))
     end)))

;;;###autoload
(defun org-agenda-list-stuck-projects (&rest ignore)
  "Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.
MATCH is being ignored."
  (interactive)
  (let* ((org-agenda-skip-function 'org-agenda-skip-subtree-when-regexp-matches)
	 ;; We could have used org-agenda-skip-if here.
	 (org-agenda-overriding-header "List of stuck projects: ")
	 (matcher (nth 0 org-stuck-projects))
	 (todo (nth 1 org-stuck-projects))
	 (todo-wds (if (member "*" todo)
		       (progn
			 (org-prepare-agenda-buffers (org-agenda-files))
			 (org-delete-all
			  org-done-keywords-for-agenda
			  (copy-sequence org-todo-keywords-for-agenda)))
		     todo))
	 (todo-re (concat "^\\*+[ \t]+\\("
			  (mapconcat 'identity todo-wds "\\|")
			  "\\)\\>"))
	 (tags (nth 2 org-stuck-projects))
	 (tags-re (if (member "*" tags)
		      (org-re "^\\*+ .*:[[:alnum:]_@]+:[ \t]*$")
		    (concat "^\\*+ .*:\\("
			    (mapconcat 'identity tags "\\|")
			    (org-re "\\):[[:alnum:]_@:]*[ \t]*$"))))
	 (gen-re (nth 3 org-stuck-projects))
	 (re-list
	  (delq nil
		(list
		 (if todo todo-re)
		 (if tags tags-re)
		 (and gen-re (stringp gen-re) (string-match "\\S-" gen-re)
		      gen-re)))))
    (setq org-agenda-skip-regexp
	  (if re-list
	      (mapconcat 'identity re-list "\\|")
	    (error "No information how to identify unstuck projects")))
    (org-tags-view nil matcher)
    (with-current-buffer org-agenda-buffer-name
      (setq org-agenda-redo-command
	    '(org-agenda-list-stuck-projects
	      (or current-prefix-arg org-last-arg))))))

;;; Diary integration

(defvar org-disable-agenda-to-diary nil)          ;Dynamically-scoped param.
(defvar list-diary-entries-hook)

(defun org-get-entries-from-diary (date)
  "Get the (Emacs Calendar) diary entries for DATE."
  (require 'diary-lib)
  (let* ((diary-fancy-buffer "*temporary-fancy-diary-buffer*")
         (fancy-diary-buffer diary-fancy-buffer)
	 (diary-display-hook '(fancy-diary-display))
	 (pop-up-frames nil)
	 (list-diary-entries-hook
	  (cons 'org-diary-default-entry list-diary-entries-hook))
	 (diary-file-name-prefix-function nil) ; turn this feature off
	 (diary-modify-entry-list-string-function 'org-modify-diary-entry-string)
	 entries
	 (org-disable-agenda-to-diary t))
    (save-excursion
      (save-window-excursion
	(funcall (if (fboundp 'diary-list-entries)
		     'diary-list-entries 'list-diary-entries)
		 date 1)))
    (if (not (get-buffer diary-fancy-buffer))
	(setq entries nil)
      (with-current-buffer diary-fancy-buffer
	(setq buffer-read-only nil)
	(if (zerop (buffer-size))
	    ;; No entries
	    (setq entries nil)
	  ;; Omit the date and other unnecessary stuff
	  (org-agenda-cleanup-fancy-diary)
	  ;; Add prefix to each line and extend the text properties
	  (if (zerop (buffer-size))
	      (setq entries nil)
	    (setq entries (buffer-substring (point-min) (- (point-max) 1)))))
	(set-buffer-modified-p nil)
	(kill-buffer diary-fancy-buffer)))
    (when entries
      (setq entries (org-split-string entries "\n"))
      (setq entries
	    (mapcar
	     (lambda (x)
	       (setq x (org-format-agenda-item "" x "Diary" nil 'time))
	       ;; Extend the text properties to the beginning of the line
	       (org-add-props x (text-properties-at (1- (length x)) x)
		 'type "diary" 'date date))
	     entries)))))

(defun org-agenda-cleanup-fancy-diary ()
  "Remove unwanted stuff in buffer created by `fancy-diary-display'.
This gets rid of the date, the underline under the date, and
the dummy entry installed by `org-mode' to ensure non-empty diary for each
date.  It also removes lines that contain only whitespace."
  (goto-char (point-min))
  (if (looking-at ".*?:[ \t]*")
      (progn
	(replace-match "")
	(re-search-forward "\n=+$" nil t)
	(replace-match "")
	(while (re-search-backward "^ +\n?" nil t) (replace-match "")))
    (re-search-forward "\n=+$" nil t)
    (delete-region (point-min) (min (point-max) (1+ (match-end 0)))))
  (goto-char (point-min))
  (while (re-search-forward "^ +\n" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (if (re-search-forward "^Org-mode dummy\n?" nil t)
      (replace-match "")))

;; Make sure entries from the diary have the right text properties.
(eval-after-load "diary-lib"
  '(if (boundp 'diary-modify-entry-list-string-function)
       ;; We can rely on the hook, nothing to do
       nil
     ;; Hook not avaiable, must use advice to make this work
     (defadvice add-to-diary-list (before org-mark-diary-entry activate)
       "Make the position visible."
       (if (and org-disable-agenda-to-diary  ;; called from org-agenda
		(stringp string)
		buffer-file-name)
	   (setq string (org-modify-diary-entry-string string))))))

(defun org-modify-diary-entry-string (string)
  "Add text properties to string, allowing org-mode to act on it."
  (org-add-props string nil
    'mouse-face 'highlight
    'keymap org-agenda-keymap
    'help-echo (if buffer-file-name
		   (format "mouse-2 or RET jump to diary file %s"
			   (abbreviate-file-name buffer-file-name))
		 "")
    'org-agenda-diary-link t
    'org-marker (org-agenda-new-marker (point-at-bol))))

(defun org-diary-default-entry ()
  "Add a dummy entry to the diary.
Needed to avoid empty dates which mess up holiday display."
  ;; Catch the error if dealing with the new add-to-diary-alist
  (when org-disable-agenda-to-diary
    (condition-case nil
	(org-add-to-diary-list original-date "Org-mode dummy" "")
      (error
       (org-add-to-diary-list original-date  "Org-mode dummy" "" nil)))))

(defun org-add-to-diary-list (&rest args)
  (if (fboundp 'diary-add-to-list)
      (apply 'diary-add-to-list args)
    (apply 'add-to-diary-list args)))

;;;###autoload
(defun org-diary (&rest args)
  "Return diary information from org-files.
This function can be used in a \"sexp\" diary entry in the Emacs calendar.
It accesses org files and extracts information from those files to be
listed in the diary.  The function accepts arguments specifying what
items should be listed.  The following arguments are allowed:

   :timestamp    List the headlines of items containing a date stamp or
		 date range matching the selected date.  Deadlines will
		 also be listed, on the expiration day.

   :sexp         List entries resulting from diary-like sexps.

   :deadline     List any deadlines past due, or due within
		 `org-deadline-warning-days'.  The listing occurs only
		 in the diary for *today*, not at any other date.  If
		 an entry is marked DONE, it is no longer listed.

   :scheduled    List all items which are scheduled for the given date.
		 The diary for *today* also contains items which were
		 scheduled earlier and are not yet marked DONE.

   :todo         List all TODO items from the org-file.  This may be a
		 long list - so this is not turned on by default.
		 Like deadlines, these entries only show up in the
		 diary for *today*, not at any other date.

The call in the diary file should look like this:

   &%%(org-diary) ~/path/to/some/orgfile.org

Use a separate line for each org file to check.  Or, if you omit the file name,
all files listed in `org-agenda-files' will be checked automatically:

   &%%(org-diary)

If you don't give any arguments (as in the example above), the default
arguments (:deadline :scheduled :timestamp :sexp) are used.
So the example above may also be written as

   &%%(org-diary :deadline :timestamp :sexp :scheduled)

The function expects the lisp variables `entry' and `date' to be provided
by the caller, because this is how the calendar works.  Don't use this
function from a program - use `org-agenda-get-day-entries' instead."
  (when (> (- (time-to-seconds (current-time))
	      org-agenda-last-marker-time)
	   5)
    (org-agenda-reset-markers))
  (org-compile-prefix-format 'agenda)
  (org-set-sorting-strategy 'agenda)
  (setq args (or args '(:deadline :scheduled :timestamp :sexp)))
  (let* ((files (if (and entry (stringp entry) (string-match "\\S-" entry))
		    (list entry)
		  (org-agenda-files t)))
	 file rtn results)
    (org-prepare-agenda-buffers files)
    ;; If this is called during org-agenda, don't return any entries to
    ;; the calendar.  Org Agenda will list these entries itself.
    (if org-disable-agenda-to-diary (setq files nil))
    (while (setq file (pop files))
      (setq rtn (apply 'org-agenda-get-day-entries file date args))
      (setq results (append results rtn)))
    (if results
	(concat (org-finalize-agenda-entries results) "\n"))))

;;; Agenda entry finders

(defun org-agenda-get-day-entries (file date &rest args)
  "Does the work for `org-diary' and `org-agenda'.
FILE is the path to a file to be checked for entries.  DATE is date like
the one returned by `calendar-current-date'.  ARGS are symbols indicating
which kind of entries should be extracted.  For details about these, see
the documentation of `org-diary'."
  (setq args (or args '(:deadline :scheduled :timestamp :sexp)))
  (let* ((org-startup-folded nil)
	 (org-startup-align-all-tables nil)
	 (buffer (if (file-exists-p file)
		     (org-get-agenda-file-buffer file)
		   (error "No such file %s" file)))
	 arg results rtn)
    (if (not buffer)
	;; If file does not exist, make sure an error message ends up in diary
	(list (format "ORG-AGENDA-ERROR: No such org-file %s" file))
      (with-current-buffer buffer
	(unless (org-mode-p)
	  (error "Agenda file %s is not in `org-mode'" file))
	(let ((case-fold-search nil))
	  (save-excursion
	    (save-restriction
	      (if org-agenda-restrict
		  (narrow-to-region org-agenda-restrict-begin
				    org-agenda-restrict-end)
		(widen))
	      ;; The way we repeatedly append to `results' makes it O(n^2) :-(
	      (while (setq arg (pop args))
		(cond
		 ((and (eq arg :todo)
		       (equal date (calendar-current-date)))
		  (setq rtn (org-agenda-get-todos))
		  (setq results (append results rtn)))
		 ((eq arg :timestamp)
		  (setq rtn (org-agenda-get-blocks))
		  (setq results (append results rtn))
		  (setq rtn (org-agenda-get-timestamps))
		  (setq results (append results rtn)))
		 ((eq arg :sexp)
		  (setq rtn (org-agenda-get-sexps))
		  (setq results (append results rtn)))
		 ((eq arg :scheduled)
		  (setq rtn (org-agenda-get-scheduled))
		  (setq results (append results rtn)))
		 ((eq arg :closed)
		  (setq rtn (org-agenda-get-closed))
		  (setq results (append results rtn)))
		 ((eq arg :deadline)
		  (setq rtn (org-agenda-get-deadlines))
		  (setq results (append results rtn))))))))
	results))))

(defun org-agenda-get-todos ()
  "Return the TODO information for agenda display."
  (let* ((props (list 'face nil
		      'done-face 'org-done
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (concat "^\\*+[ \t]+\\("
			 (if org-select-this-todo-keyword
			     (if (equal org-select-this-todo-keyword "*")
				 org-todo-regexp
			       (concat "\\<\\("
				       (mapconcat 'identity (org-split-string org-select-this-todo-keyword "|") "\\|")
				     "\\)\\>"))
			   org-not-done-regexp)
			 "[^\n\r]*\\)"))
	 marker priority category tags
	 ee txt beg end)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(save-match-data
	  (beginning-of-line)
	  (setq beg (point) end (progn (outline-next-heading) (point)))
	  (when (or (and org-agenda-todo-ignore-with-date (goto-char beg)
			 (re-search-forward org-ts-regexp end t))
		    (and org-agenda-todo-ignore-scheduled (goto-char beg)
			 (re-search-forward org-scheduled-time-regexp end t))
		    (and org-agenda-todo-ignore-deadlines (goto-char beg)
			 (re-search-forward org-deadline-time-regexp end t)
			 (org-deadline-close (match-string 1))))
	    (goto-char (1+ beg))
	    (or org-agenda-todo-list-sublevels (org-end-of-subtree 'invisible))
	    (throw :skip nil)))
	(goto-char beg)
	(org-agenda-skip)
	(goto-char (match-beginning 1))
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      category (org-get-category)
	      tags (org-get-tags-at (point))
	      txt (org-format-agenda-item "" (match-string 1) category tags)
	      priority (1+ (org-get-priority txt)))
	(org-add-props txt props
	  'org-marker marker 'org-hd-marker marker
	  'priority priority 'org-category category
	  'type "todo")
	(push txt ee)
	(if org-agenda-todo-list-sublevels
	    (goto-char (match-end 1))
	  (org-end-of-subtree 'invisible))))
    (nreverse ee)))

(defconst org-agenda-no-heading-message
  "No heading for this item in buffer or region.")

(defun org-agenda-get-timestamps ()
  "Return the date stamp information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (d1 (calendar-absolute-from-gregorian date))
	 (remove-re
	  (concat
	   (regexp-quote
	    (format-time-string
	     "<%Y-%m-%d"
	     (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
	   ".*?>"))
	 (regexp
	  (concat
	   (if org-agenda-include-inactive-timestamps "[[<]" "<")
	   (regexp-quote
	    (substring
	     (format-time-string
	      (car org-time-stamp-formats)
	      (apply 'encode-time  ; DATE bound by calendar
		     (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
	     1 11))
	   "\\|\\(<[0-9]+-[0-9]+-[0-9]+[^>\n]+?\\+[0-9]+[dwmy]>\\)"
	   "\\|\\(<%%\\(([^>\n]+)\\)>\\)"))
	 marker hdmarker deadlinep scheduledp clockp closedp inactivep
	 donep tmp priority category ee txt timestr tags b0 b3 e3 head)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq b0 (match-beginning 0)
	    b3 (match-beginning 3) e3 (match-end 3))
      (catch :skip
	(and (org-at-date-range-p) (throw :skip nil))
	(org-agenda-skip)
	(if (and (match-end 1)
		 (not (= d1 (org-time-string-to-absolute
			     (match-string 1) d1 nil
			     org-agenda-repeating-timestamp-show-all))))
	    (throw :skip nil))
	(if (and e3
		 (not (org-diary-sexp-entry (buffer-substring b3 e3) "" date)))
	    (throw :skip nil))
	(setq marker (org-agenda-new-marker b0)
	      category (org-get-category b0)
	      tmp (buffer-substring (max (point-min)
					 (- b0 org-ds-keyword-length))
				    b0)
	      timestr (if b3 "" (buffer-substring b0 (point-at-eol)))
	      inactivep (= (char-after b0) ?\[)
	      deadlinep (string-match org-deadline-regexp tmp)
	      scheduledp (string-match org-scheduled-regexp tmp)
	      closedp (and org-agenda-include-inactive-timestamps
			   (string-match org-closed-string tmp))
	      clockp (and org-agenda-include-inactive-timestamps
			  (or (string-match org-clock-string tmp)
			      (string-match "]-+\\'" tmp)))
	      donep (org-entry-is-done-p))
	(if (or scheduledp deadlinep closedp clockp)
	    (throw :skip t))
	(if (string-match ">" timestr)
	    ;; substring should only run to end of time stamp
	    (setq timestr (substring timestr 0 (match-end 0))))
	(save-excursion
	  (if (re-search-backward "^\\*+ " nil t)
	      (progn
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker)
		      tags (org-get-tags-at))
		(looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		(setq head (match-string 1))
		(and org-agenda-skip-timestamp-if-done donep (throw :skip t))
		(setq txt (org-format-agenda-item
			   (if inactivep "[" nil)
			   head category tags timestr nil
			   remove-re)))
	    (setq txt org-agenda-no-heading-message))
	  (setq priority (org-get-priority txt))
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker)
	  (org-add-props txt nil 'priority priority
			 'org-category category 'date date
			 'type "timestamp")
	  (push txt ee))
	(outline-next-heading)))
    (nreverse ee)))

(defun org-agenda-get-sexps ()
  "Return the sexp information for agenda display."
  (require 'diary-lib)
  (let* ((props (list 'face nil
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp "^&?%%(")
	 marker category ee txt tags entry result beg b sexp sexp-entry)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq beg (match-beginning 0))
	(goto-char (1- (match-end 0)))
	(setq b (point))
	(forward-sexp 1)
	(setq sexp (buffer-substring b (point)))
	(setq sexp-entry (if (looking-at "[ \t]*\\(\\S-.*\\)")
			     (org-trim (match-string 1))
			   ""))
	(setq result (org-diary-sexp-entry sexp sexp-entry date))
	(when result
	  (setq marker (org-agenda-new-marker beg)
		category (org-get-category beg))

	  (if (string-match "\\S-" result)
	      (setq txt result)
	    (setq txt "SEXP entry returned empty string"))

	  (setq txt (org-format-agenda-item
                     "" txt category tags 'time))
	  (org-add-props txt props 'org-marker marker)
	  (org-add-props txt nil
	    'org-category category 'date date
	    'type "sexp")
	  (push txt ee))))
    (nreverse ee)))

(defun org-agenda-get-closed ()
  "Return the logged TODO entries for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp (concat
		  "\\<\\(" org-closed-string "\\|" org-clock-string "\\) *\\["
		  (regexp-quote
		   (substring
		    (format-time-string
		     (car org-time-stamp-formats)
		     (apply 'encode-time  ; DATE bound by calendar
			    (list 0 0 0 (nth 1 date) (car date) (nth 2 date))))
		    1 11))))
	 marker hdmarker priority category tags closedp
	 ee txt timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq marker (org-agenda-new-marker (match-beginning 0))
	      closedp (equal (match-string 1) org-closed-string)
	      category (org-get-category (match-beginning 0))
	      timestr (buffer-substring (match-beginning 0) (point-at-eol))
	      ;; donep (org-entry-is-done-p)
	      )
	(if (string-match "\\]" timestr)
	    ;; substring should only run to end of time stamp
	    (setq timestr (substring timestr 0 (match-end 0))))
	(save-excursion
	  (if (re-search-backward "^\\*+ " nil t)
	      (progn
		(goto-char (match-beginning 0))
		(setq hdmarker (org-agenda-new-marker)
		      tags (org-get-tags-at))
		(looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		(setq txt (org-format-agenda-item
			   (if closedp "Closed:    " "Clocked:   ")
			   (match-string 1) category tags timestr)))
	    (setq txt org-agenda-no-heading-message))
	  (setq priority 100000)
	  (org-add-props txt props
	    'org-marker marker 'org-hd-marker hdmarker 'face 'org-done
	    'priority priority 'org-category category
	    'type "closed" 'date date
	    'undone-face 'org-warning 'done-face 'org-done)
	  (push txt ee))
	(goto-char (point-at-eol))))
    (nreverse ee)))

(defun org-agenda-get-deadlines ()
  "Return the deadline information for agenda display."
  (let* ((props (list 'mouse-face 'highlight
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-deadline-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff dfrac wdays pos pos1 category tags
	 ee txt head face s upcomingp donep timestr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq s (match-string 1)
	      pos (1- (match-beginning 1))
	      d2 (org-time-string-to-absolute
		  (match-string 1) d1 'past
		  org-agenda-repeating-timestamp-show-all)
	      diff (- d2 d1)
	      wdays (org-get-wdays s)
	      dfrac (/ (* 1.0 (- wdays diff)) (max wdays 1))
	      upcomingp (and todayp (> diff 0)))
	;; When to show a deadline in the calendar:
	;; If the expiration is within wdays warning time.
	;; Past-due deadlines are only shown on the current date
	(if (or (and (<= diff wdays)
		     (and todayp (not org-agenda-only-exact-dates)))
		(= diff 0))
	    (save-excursion
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+[ \t]+" nil t)
		  (progn
		    (goto-char (match-end 0))
		    (setq pos1 (match-beginning 0))
		    (setq tags (org-get-tags-at pos1))
		    (setq head (buffer-substring-no-properties
				(point)
				(progn (skip-chars-forward "^\r\n")
				       (point))))
		    (setq donep (string-match org-looking-at-done-regexp head))
		    (if (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
			(setq timestr
			      (concat (substring s (match-beginning 1)) " "))
		      (setq timestr 'time))
		    (if (and donep
			     (or org-agenda-skip-deadline-if-done
				 (not (= diff 0))))
			(setq txt nil)
		      (setq txt (org-format-agenda-item
				 (if (= diff 0)
				     (car org-agenda-deadline-leaders)
				   (if (functionp (nth 1 org-agenda-deadline-leaders))
				       (funcall (nth 1 org-agenda-deadline-leaders) diff date)
				     (format (nth 1 org-agenda-deadline-leaders)
					     diff)))
				 head category tags timestr))))
		(setq txt org-agenda-no-heading-message))
	      (when txt
		(setq face (org-agenda-deadline-face dfrac wdays))
		(org-add-props txt props
		  'org-marker (org-agenda-new-marker pos)
		  'org-hd-marker (org-agenda-new-marker pos1)
		  'priority (+ (- diff)
			       (org-get-priority txt))
		  'org-category category
		  'type (if upcomingp "upcoming-deadline" "deadline")
		  'date (if upcomingp date d2)
		  'face (if donep 'org-done face)
		  'undone-face face 'done-face 'org-done)
		(push txt ee))))))
    (nreverse ee)))

(defun org-agenda-deadline-face (fraction &optional wdays)
  "Return the face to displaying a deadline item.
FRACTION is what fraction of the head-warning time has passed."
  (if (equal wdays 0) (setq fraction 1.))
  (let ((faces org-agenda-deadline-faces) f)
    (catch 'exit
      (while (setq f (pop faces))
	(if (>= fraction (car f)) (throw 'exit (cdr f)))))))

(defun org-agenda-get-scheduled ()
  "Return the scheduled information for agenda display."
  (let* ((props (list 'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'done-face 'org-done
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-scheduled-time-regexp)
	 (todayp (equal date (calendar-current-date))) ; DATE bound by calendar
	 (d1 (calendar-absolute-from-gregorian date))  ; DATE bound by calendar
	 d2 diff pos pos1 category tags
	 ee txt head pastschedp donep face timestr s)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq s (match-string 1)
	      pos (1- (match-beginning 1))
	      d2 (org-time-string-to-absolute
		  (match-string 1) d1 'past
		  org-agenda-repeating-timestamp-show-all)
	      diff (- d2 d1))
	(setq pastschedp (and todayp (< diff 0)))
	;; When to show a scheduled item in the calendar:
	;; If it is on or past the date.
	(if (or (and (< diff 0)
		     (< (abs diff) org-scheduled-past-days)
		     (and todayp (not org-agenda-only-exact-dates)))
		(= diff 0))
	    (save-excursion
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+[ \t]+" nil t)
		  (progn
		    (goto-char (match-end 0))
		    (setq pos1 (match-beginning 0))
		    (setq tags (org-get-tags-at))
		    (setq head (buffer-substring-no-properties
				(point)
				(progn (skip-chars-forward "^\r\n") (point))))
		    (setq donep (string-match org-looking-at-done-regexp head))
		    (if (string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
			(setq timestr
			      (concat (substring s (match-beginning 1)) " "))
		      (setq timestr 'time))
		    (if (and donep
			     (or org-agenda-skip-scheduled-if-done
				 (not (= diff 0))))
			(setq txt nil)
		      (setq txt (org-format-agenda-item
				 (if (= diff 0)
				     (car org-agenda-scheduled-leaders)
				   (format (nth 1 org-agenda-scheduled-leaders)
					   (- 1 diff)))
				 head category tags timestr))))
		(setq txt org-agenda-no-heading-message))
	      (when txt
		(setq face (if pastschedp
			       'org-scheduled-previously
			     'org-scheduled-today))
		(org-add-props txt props
		  'undone-face face
		  'face (if donep 'org-done face)
		  'org-marker (org-agenda-new-marker pos)
		  'org-hd-marker (org-agenda-new-marker pos1)
		  'type (if pastschedp "past-scheduled" "scheduled")
		  'date (if pastschedp d2 date)
		  'priority (+ 94 (- 5 diff) (org-get-priority txt))
		  'org-category category)
		(push txt ee))))))
    (nreverse ee)))

(defun org-agenda-get-blocks ()
  "Return the date-range information for agenda display."
  (let* ((props (list 'face nil
		      'org-not-done-regexp org-not-done-regexp
		      'org-todo-regexp org-todo-regexp
		      'mouse-face 'highlight
		      'keymap org-agenda-keymap
		      'help-echo
		      (format "mouse-2 or RET jump to org file %s"
			      (abbreviate-file-name buffer-file-name))))
	 (regexp org-tr-regexp)
	 (d0 (calendar-absolute-from-gregorian date))
	 marker hdmarker ee txt d1 d2 s1 s2 timestr category tags pos
	 donep head)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch :skip
	(org-agenda-skip)
	(setq pos (point))
	(setq timestr (match-string 0)
	      s1 (match-string 1)
	      s2 (match-string 2)
	      d1 (time-to-days (org-time-string-to-time s1))
	      d2 (time-to-days (org-time-string-to-time s2)))
	(if (and (> (- d0 d1) -1) (> (- d2 d0) -1))
	    ;; Only allow days between the limits, because the normal
	    ;; date stamps will catch the limits.
	    (save-excursion
	      (setq marker (org-agenda-new-marker (point)))
	      (setq category (org-get-category))
	      (if (re-search-backward "^\\*+ " nil t)
		  (progn
		    (goto-char (match-beginning 0))
		    (setq hdmarker (org-agenda-new-marker (point)))
		    (setq tags (org-get-tags-at))
		    (looking-at "\\*+[ \t]+\\([^\r\n]+\\)")
		    (setq head (match-string 1))
		    (and org-agenda-skip-timestamp-if-done
			 (org-entry-is-done-p)
			 (throw :skip t))
		    (setq txt (org-format-agenda-item
			       (format (if (= d1 d2) "" "(%d/%d): ")
				       (1+ (- d0 d1)) (1+ (- d2 d1)))
			       head category tags
			       (if (= d0 d1) timestr))))
		(setq txt org-agenda-no-heading-message))
	      (org-add-props txt props
		'org-marker marker 'org-hd-marker hdmarker
		'type "block" 'date date
		'priority (org-get-priority txt) 'org-category category)
	      (push txt ee)))
	(goto-char pos)))
    ;; Sort the entries by expiration date.
    (nreverse ee)))

;;; Agenda presentation and sorting

(defvar org-prefix-has-time nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%t'.")
(defvar org-prefix-has-tag nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%T'.")
(defvar org-prefix-has-effort nil
  "A flag, set by `org-compile-prefix-format'.
The flag is set if the currently compiled format contains a `%e'.")

(defun org-format-agenda-item (extra txt &optional category tags dotime
				     noprefix remove-re)
  "Format TXT to be inserted into the agenda buffer.
In particular, it adds the prefix and corresponding text properties.  EXTRA
must be a string and replaces the `%s' specifier in the prefix format.
CATEGORY (string, symbol or nil) may be used to overrule the default
category taken from local variable or file name.  It will replace the `%c'
specifier in the format.  DOTIME, when non-nil, indicates that a
time-of-day should be extracted from TXT for sorting of this entry, and for
the `%t' specifier in the format.  When DOTIME is a string, this string is
searched for a time before TXT is.  NOPREFIX is a flag and indicates that
only the correctly processes TXT should be returned - this is used by
`org-agenda-change-all-lines'.  TAGS can be the tags of the headline.
Any match of REMOVE-RE will be removed from TXT."
  (save-match-data
    ;; Diary entries sometimes have extra whitespace at the beginning
    (if (string-match "^ +" txt) (setq txt (replace-match "" nil nil txt)))
    (let* ((category (or category
			 org-category
			 (if buffer-file-name
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))
			   "")))
	   ;; time, tag, effort are needed for the eval of the prefix format
	   (tag (if tags (nth (1- (length tags)) tags) ""))
	   time effort neffort
	   (ts (if dotime (concat (if (stringp dotime) dotime "") txt)))
	   (time-of-day (and dotime (org-get-time-of-day ts)))
	   stamp plain s0 s1 s2 t1 t2 rtn srp
	   duration)
      (and (org-mode-p) buffer-file-name
	   (add-to-list 'org-agenda-contributing-files buffer-file-name))
      (when (and dotime time-of-day)
	;; Extract starting and ending time and move them to prefix
	(when (or (setq stamp (string-match org-stamp-time-of-day-regexp ts))
		  (setq plain (string-match org-plain-time-of-day-regexp ts)))
	  (setq s0 (match-string 0 ts)
		srp (and stamp (match-end 3))
		s1 (match-string (if plain 1 2) ts)
		s2 (match-string (if plain 8 (if srp 4 6)) ts))

	  ;; If the times are in TXT (not in DOTIMES), and the prefix will list
	  ;; them, we might want to remove them there to avoid duplication.
	  ;; The user can turn this off with a variable.
	  (if (and org-prefix-has-time
		   org-agenda-remove-times-when-in-prefix (or stamp plain)
		   (string-match (concat (regexp-quote s0) " *") txt)
		   (not (equal ?\] (string-to-char (substring txt (match-end 0)))))
		   (if (eq org-agenda-remove-times-when-in-prefix 'beg)
		       (= (match-beginning 0) 0)
		     t))
	      (setq txt (replace-match "" nil nil txt))))
	;; Normalize the time(s) to 24 hour
	(if s1 (setq s1 (org-get-time-of-day s1 'string t)))
	(if s2 (setq s2 (org-get-time-of-day s2 'string t)))
	;; Compute the duration
	(when s1
	  (setq t1 (+ (* 60 (string-to-number (substring s1 0 2)))
		      (string-to-number (substring s1 3)))
		t2 (cond
		    (s2 (+ (* 60 (string-to-number (substring s2 0 2)))
			   (string-to-number (substring s2 3))))
		    (org-agenda-default-appointment-duration
		     (+ t1 org-agenda-default-appointment-duration))
		    (t nil)))
	  (setq duration (if t2 (- t2 t1)))))

      (when (and s1 (not s2) org-agenda-default-appointment-duration
		 (string-match "\\([0-9]+\\):\\([0-9]+\\)" s1))
	(let ((m (+ (string-to-number (match-string 2 s1))
		    (* 60 (string-to-number (match-string 1 s1)))
		    org-agenda-default-appointment-duration))
	      h)
	  (setq h (/ m 60) m (- m (* h 60)))
	  (setq s2 (format "%02d:%02d" h m))))

      (when (string-match (org-re "\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$")
			  txt)
	;; Tags are in the string
	(if (or (eq org-agenda-remove-tags t)
		(and org-agenda-remove-tags
		     org-prefix-has-tag))
	    (setq txt (replace-match "" t t txt))
	  (setq txt (replace-match
		     (concat (make-string (max (- 50 (length txt)) 1) ?\ )
			     (match-string 2 txt))
		     t t txt))))
      (when (org-mode-p)
	(setq effort
	      (condition-case nil
		  (org-get-effort
		   (or (get-text-property 0 'org-hd-marker txt)
		       (get-text-property 0 'org-marker txt)))
		(error nil)))
	(when effort
	  (setq neffort (org-hh:mm-string-to-minutes effort)
		effort (setq effort (concat "[" effort"]" )))))

      (when remove-re
	(while (string-match remove-re txt)
	  (setq txt (replace-match "" t t txt))))

      ;; Create the final string
      (if noprefix
	  (setq rtn txt)
	;; Prepare the variables needed in the eval of the compiled format
	(setq time (cond (s2 (concat s1 "-" s2))
			 (s1 (concat s1 "......"))
			 (t ""))
	      extra (or extra "")
	      category (if (symbolp category) (symbol-name category) category))
	;; Evaluate the compiled format
	(setq rtn (concat (eval org-prefix-format-compiled) txt)))

      ;; And finally add the text properties
      (org-add-props rtn nil
	'org-category (downcase category) 'tags tags
	'org-highest-priority org-highest-priority
	'org-lowest-priority org-lowest-priority
	'prefix-length (- (length rtn) (length txt))
	'time-of-day time-of-day
	'duration duration
	'effort effort
	'effort-minutes neffort
	'txt txt
	'time time
	'extra extra
	'dotime dotime))))

(defvar org-agenda-sorting-strategy) ;; because the def is in a let form
(defvar org-agenda-sorting-strategy-selected nil)

(defun org-agenda-add-time-grid-maybe (list ndays todayp)
  (catch 'exit
    (cond ((not org-agenda-use-time-grid) (throw 'exit list))
	  ((and todayp (member 'today (car org-agenda-time-grid))))
	  ((and (= ndays 1) (member 'daily (car org-agenda-time-grid))))
	  ((member 'weekly (car org-agenda-time-grid)))
	  (t (throw 'exit list)))
    (let* ((have (delq nil (mapcar
			    (lambda (x) (get-text-property 1 'time-of-day x))
			    list)))
	   (string (nth 1 org-agenda-time-grid))
	   (gridtimes (nth 2 org-agenda-time-grid))
	   (req (car org-agenda-time-grid))
	   (remove (member 'remove-match req))
	   new time)
      (if (and (member 'require-timed req) (not have))
	  ;; don't show empty grid
	  (throw 'exit list))
      (while (setq time (pop gridtimes))
	(unless (and remove (member time have))
	  (setq time (int-to-string time))
	  (push (org-format-agenda-item
		 nil string "" nil
		 (concat (substring time 0 -2) ":" (substring time -2)))
		new)
	  (put-text-property
	   1 (length (car new)) 'face 'org-time-grid (car new))))
      (if (member 'time-up org-agenda-sorting-strategy-selected)
	  (append new list)
	(append list new)))))

(defun org-compile-prefix-format (key)
  "Compile the prefix format into a Lisp form that can be evaluated.
The resulting form is returned and stored in the variable
`org-prefix-format-compiled'."
  (setq org-prefix-has-time nil org-prefix-has-tag nil
	org-prefix-has-effort nil)
  (let ((s (cond
	    ((stringp org-agenda-prefix-format)
	     org-agenda-prefix-format)
	    ((assq key org-agenda-prefix-format)
	     (cdr (assq key org-agenda-prefix-format)))
	    (t "  %-12:c%?-12t% s")))
	(start 0)
	varform vars var e c f opt)
    (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([ctse]\\)"
			 s start)
      (setq var (cdr (assoc (match-string 4 s)
			    '(("c" . category) ("t" . time) ("s" . extra)
			      ("T" . tag) ("e" . effort))))
	    c (or (match-string 3 s) "")
	    opt (match-beginning 1)
	    start (1+ (match-beginning 0)))
      (if (equal var 'time) (setq org-prefix-has-time t))
      (if (equal var 'tag)  (setq org-prefix-has-tag  t))
      (if (equal var 'effort) (setq org-prefix-has-effort t))
      (setq f (concat "%" (match-string 2 s) "s"))
      (if opt
	  (setq varform
		`(if (equal "" ,var)
		     ""
		   (format ,f (if (equal "" ,var) "" (concat ,var ,c)))))
	(setq varform `(format ,f (if (equal ,var "") "" (concat ,var ,c)))))
      (setq s (replace-match "%s" t nil s))
      (push varform vars))
    (setq vars (nreverse vars))
    (setq org-prefix-format-compiled `(format ,s ,@vars))))

(defun org-set-sorting-strategy (key)
  (if (symbolp (car org-agenda-sorting-strategy))
      ;; the old format
      (setq org-agenda-sorting-strategy-selected org-agenda-sorting-strategy)
    (setq org-agenda-sorting-strategy-selected
	  (or (cdr (assq key org-agenda-sorting-strategy))
	      (cdr (assq 'agenda org-agenda-sorting-strategy))
	      '(time-up category-keep priority-down)))))

(defun org-get-time-of-day (s &optional string mod24)
  "Check string S for a time of day.
If found, return it as a military time number between 0 and 2400.
If not found, return nil.
The optional STRING argument forces conversion into a 5 character wide string
HH:MM."
  (save-match-data
    (when
	(or (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)\\([AaPp][Mm]\\)?\\> *" s)
	    (string-match "\\<\\([012]?[0-9]\\)\\(:\\([0-5][0-9]\\)\\)?\\([AaPp][Mm]\\)\\> *" s))
     (let* ((h (string-to-number (match-string 1 s)))
	    (m (if (match-end 3) (string-to-number (match-string 3 s)) 0))
	    (ampm (if (match-end 4) (downcase (match-string 4 s))))
	    (am-p (equal ampm "am"))
	    (h1   (cond ((not ampm) h)
			((= h 12) (if am-p 0 12))
			(t (+ h (if am-p 0 12)))))
	    (h2 (if (and string mod24 (not (and (= m 0) (= h1 24))))
		    (mod h1 24) h1))
	    (t0 (+ (* 100 h2) m))
	    (t1 (concat (if (>= h1 24) "+" " ")
			(if (< t0 100) "0" "")
			(if (< t0 10)  "0" "")
			(int-to-string t0))))
       (if string (concat (substring t1 -4 -2) ":" (substring t1 -2)) t0)))))

(defun org-finalize-agenda-entries (list &optional nosort)
  "Sort and concatenate the agenda items."
  (setq list (mapcar 'org-agenda-highlight-todo list))
  (if nosort
      list
    (mapconcat 'identity (sort list 'org-entries-lessp) "\n")))

(defun org-agenda-highlight-todo (x)
  (let (re pl)
    (if (eq x 'line)
	(save-excursion
	  (beginning-of-line 1)
	  (setq re (get-text-property (point) 'org-todo-regexp))
	  (goto-char (+ (point) (or (get-text-property (point) 'prefix-length) 0)))
	  (when (looking-at (concat "[ \t]*\\.*" re " +"))
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'face (org-get-todo-face 0)))
	    (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
	      (delete-region (match-beginning 1) (1- (match-end 0)))
	      (goto-char (match-beginning 1))
	      (insert (format org-agenda-todo-keyword-format s)))))
      (setq re (concat (get-text-property 0 'org-todo-regexp x))
	    pl (get-text-property 0 'prefix-length x))
      (when (and re
		 (equal (string-match (concat "\\(\\.*\\)" re "\\( +\\)")
				      x (or pl 0)) pl))
	(add-text-properties
	 (or (match-end 1) (match-end 0)) (match-end 0)
	 (list 'face (org-get-todo-face (match-string 2 x)))
	 x)
	(setq x (concat (substring x 0 (match-end 1))
			(format org-agenda-todo-keyword-format
				(match-string 2 x))
			" "
			(substring x (match-end 3)))))
      x)))

(defsubst org-cmp-priority (a b)
  "Compare the priorities of string A and B."
  (let ((pa (or (get-text-property 1 'priority a) 0))
	(pb (or (get-text-property 1 'priority b) 0)))
    (cond ((> pa pb) +1)
	  ((< pa pb) -1)
	  (t nil))))

(defsubst org-cmp-effort (a b)
  "Compare the priorities of string A and B."
  (let* ((def (if org-sort-agenda-noeffort-is-high 32767 -1))
	 (ea (or (get-text-property 1 'effort-minutes a) def))
	 (eb (or (get-text-property 1 'effort-minutes b) def)))
    (cond ((> ea eb) +1)
	  ((< ea eb) -1)
	  (t nil))))

(defsubst org-cmp-category (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ca (or (get-text-property 1 'org-category a) ""))
	(cb (or (get-text-property 1 'org-category b) "")))
    (cond ((string-lessp ca cb) -1)
	  ((string-lessp cb ca) +1)
	  (t nil))))

(defsubst org-cmp-tag (a b)
  "Compare the string values of categories of strings A and B."
  (let ((ta (car (last (get-text-property 1 'tags a))))
	(tb (car (last (get-text-property 1 'tags b)))))
    (cond ((not ta) +1)
	  ((not tb) -1)
	  ((string-lessp ta tb) -1)
	  ((string-lessp tb ta) +1)
	  (t nil))))

(defsubst org-cmp-time (a b)
  "Compare the time-of-day values of strings A and B."
  (let* ((def (if org-sort-agenda-notime-is-late 9901 -1))
	 (ta (or (get-text-property 1 'time-of-day a) def))
	 (tb (or (get-text-property 1 'time-of-day b) def)))
    (cond ((< ta tb) -1)
	  ((< tb ta) +1)
	  (t nil))))

(defun org-entries-lessp (a b)
  "Predicate for sorting agenda entries."
  ;; The following variables will be used when the form is evaluated.
  ;; So even though the compiler complains, keep them.
  (let* ((time-up (org-cmp-time a b))
	 (time-down (if time-up (- time-up) nil))
	 (priority-up (org-cmp-priority a b))
	 (priority-down (if priority-up (- priority-up) nil))
	 (effort-up (org-cmp-effort a b))
	 (effort-down (if effort-up (- effort-up) nil))
	 (category-up (org-cmp-category a b))
	 (category-down (if category-up (- category-up) nil))
	 (category-keep (if category-up +1 nil))
	 (tag-up (org-cmp-tag a b))
	 (tag-down (if tag-up (- tag-up) nil)))
    (cdr (assoc
	  (eval (cons 'or org-agenda-sorting-strategy-selected))
	  '((-1 . t) (1 . nil) (nil . nil))))))

;;; Agenda restriction lock

(defvar org-agenda-restriction-lock-overlay (org-make-overlay 1 1)
  "Overlay to mark the headline to which arenda commands are restricted.")
(org-overlay-put org-agenda-restriction-lock-overlay
		 'face 'org-agenda-restriction-lock)
(org-overlay-put org-agenda-restriction-lock-overlay
		 'help-echo "Agendas are currently limited to this subtree.")
(org-detach-overlay org-agenda-restriction-lock-overlay)

(defun org-agenda-set-restriction-lock (&optional type)
  "Set restriction lock for agenda, to current subtree or file.
Restriction will be the file if TYPE is `file', or if type is the
universal prefix '(4), or if the cursor is before the first headline
in the file.  Otherwise, restriction will be to the current subtree."
  (interactive "P")
  (and (equal type '(4)) (setq type 'file))
  (setq type (cond
	      (type type)
	      ((org-at-heading-p) 'subtree)
	      ((condition-case nil (org-back-to-heading t) (error nil))
	       'subtree)
	      (t 'file)))
  (if (eq type 'subtree)
      (progn
	(setq org-agenda-restrict t)
	(setq org-agenda-overriding-restriction 'subtree)
	(put 'org-agenda-files 'org-restrict
	     (list (buffer-file-name (buffer-base-buffer))))
	(org-back-to-heading t)
	(org-move-overlay org-agenda-restriction-lock-overlay (point) (point-at-eol))
	(move-marker org-agenda-restrict-begin (point))
	(move-marker org-agenda-restrict-end
		     (save-excursion (org-end-of-subtree t)))
	(message "Locking agenda restriction to subtree"))
    (put 'org-agenda-files 'org-restrict
	 (list (buffer-file-name (buffer-base-buffer))))
    (setq org-agenda-restrict nil)
    (setq org-agenda-overriding-restriction 'file)
    (move-marker org-agenda-restrict-begin nil)
    (move-marker org-agenda-restrict-end nil)
    (message "Locking agenda restriction to file"))
  (setq current-prefix-arg nil)
  (org-agenda-maybe-redo))

(defun org-agenda-remove-restriction-lock (&optional noupdate)
  "Remove the agenda restriction lock."
  (interactive "P")
  (org-detach-overlay org-agenda-restriction-lock-overlay)
  (org-detach-overlay org-speedbar-restriction-lock-overlay)
  (setq org-agenda-overriding-restriction nil)
  (setq org-agenda-restrict nil)
  (put 'org-agenda-files 'org-restrict nil)
  (move-marker org-agenda-restrict-begin nil)
  (move-marker org-agenda-restrict-end nil)
  (setq current-prefix-arg nil)
  (message "Agenda restriction lock removed")
  (or noupdate (org-agenda-maybe-redo)))

(defun org-agenda-maybe-redo ()
  "If there is any window showing the agenda view, update it."
  (let ((w (get-buffer-window org-agenda-buffer-name t))
	(w0 (selected-window)))
    (when w
      (select-window w)
      (org-agenda-redo)
      (select-window w0)
      (if org-agenda-overriding-restriction
	  (message "Agenda view shifted to new %s restriction"
		   org-agenda-overriding-restriction)
	(message "Agenda restriction lock removed")))))

;;; Agenda commands

(defun org-agenda-check-type (error &rest types)
  "Check if agenda buffer is of allowed type.
If ERROR is non-nil, throw an error, otherwise just return nil."
  (if (memq org-agenda-type types)
      t
    (if error
	(error "Not allowed in %s-type agenda buffers" org-agenda-type)
      nil)))

(defun org-agenda-quit ()
  "Exit agenda by removing the window or the buffer."
  (interactive)
  (if org-agenda-columns-active
      (org-columns-quit)
    (let ((buf (current-buffer)))
      (if (not (one-window-p)) (delete-window))
      (kill-buffer buf)
      (org-agenda-reset-markers)
      (org-columns-remove-overlays))
    ;; Maybe restore the pre-agenda window configuration.
    (and org-agenda-restore-windows-after-quit
	 (not (eq org-agenda-window-setup 'other-frame))
	 org-pre-agenda-window-conf
	 (set-window-configuration org-pre-agenda-window-conf))))

(defun org-agenda-exit ()
  "Exit agenda by removing the window or the buffer.
Also kill all Org-mode buffers which have been loaded by `org-agenda'.
Org-mode buffers visited directly by the user will not be touched."
  (interactive)
  (org-release-buffers org-agenda-new-buffers)
  (setq org-agenda-new-buffers nil)
  (org-agenda-quit))

(defun org-agenda-execute (arg)
  "Execute another agenda command, keeping same window.\\<global-map>
So this is just a shortcut for `\\[org-agenda]', available in the agenda."
  (interactive "P")
  (let ((org-agenda-window-setup 'current-window))
    (org-agenda arg)))

(defun org-save-all-org-buffers ()
  "Save all Org-mode buffers without user confirmation."
  (interactive)
  (message "Saving all Org-mode buffers...")
  (save-some-buffers t 'org-mode-p)
  (message "Saving all Org-mode buffers... done"))

(defun org-agenda-redo ()
  "Rebuild Agenda.
When this is the global TODO list, a prefix argument will be interpreted."
  (interactive)
  (let* ((org-agenda-keep-modes t)
	 (cols org-agenda-columns-active)
	 (line (org-current-line))
	 (window-line (- line (org-current-line (window-start))))
	 (lprops (get 'org-agenda-redo-command 'org-lprops)))
    (and cols (org-columns-quit))
    (message "Rebuilding agenda buffer...")
    (org-let lprops '(eval org-agenda-redo-command))
    (setq org-agenda-undo-list nil
	  org-agenda-pending-undo-list nil)
    (message "Rebuilding agenda buffer...done")
    (and cols (interactive-p) (org-agenda-columns))
    (goto-line line)
    (recenter window-line)))

(defun org-agenda-manipulate-query-add ()
  "Manipulate the query by adding a search term with positive selection.
Positive selection means, the term must be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\[))
(defun org-agenda-manipulate-query-subtract ()
  "Manipulate the query by adding a search term with negative selection.
Negative selection means, term must not be matched for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\]))
(defun org-agenda-manipulate-query-add-re ()
  "Manipulate the query by adding a search regexp with positive selection.
Positive selection means, the regexp must match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\{))
(defun org-agenda-manipulate-query-subtract-re ()
  "Manipulate the query by adding a search regexp with negative selection.
Negative selection means, regexp must not match for selection of an entry."
  (interactive)
  (org-agenda-manipulate-query ?\}))
(defun org-agenda-manipulate-query (char)
  (cond
   ((memq org-agenda-type '(timeline agenda))
    (if (y-or-n-p "Re-display with inactive time stamps included? ")
	(let ((org-agenda-include-inactive-timestamps t))
	  (org-agenda-redo))
      (error "Abort")))
   ((eq org-agenda-type 'search)
    (org-add-to-string
     'org-agenda-query-string
     (cdr (assoc char '((?\[ . " +") (?\] . " -")
			(?\{ . " +{}") (?\} . " -{}")))))
    (setq org-agenda-redo-command
	  (list 'org-search-view
		org-todo-only
		org-agenda-query-string
		(+ (length org-agenda-query-string)
		   (if (member char '(?\{ ?\})) 0 1))))
    (set-register org-agenda-query-register org-agenda-query-string)
    (org-agenda-redo))
   (t (error "Cannot manipulate query for %s-type agenda buffers"
	     org-agenda-type))))

(defun org-add-to-string (var string)
  (set var (concat (symbol-value var) string)))

(defun org-agenda-goto-date (date)
  "Jump to DATE in agenda."
  (interactive (list (org-read-date)))
  (org-agenda-list nil date))

(defun org-agenda-goto-today ()
  "Go to today."
  (interactive)
  (org-agenda-check-type t 'timeline 'agenda)
  (let ((tdpos (text-property-any (point-min) (point-max) 'org-today t)))
    (cond
     (tdpos (goto-char tdpos))
     ((eq org-agenda-type 'agenda)
      (let* ((sd (time-to-days
		  (time-subtract (current-time)
				 (list 0 (* 3600 org-extend-today-until) 0))))
	     (comp (org-agenda-compute-time-span sd org-agenda-span))
	     (org-agenda-overriding-arguments org-agenda-last-arguments))
	(setf (nth 1 org-agenda-overriding-arguments) (car comp))
	(setf (nth 2 org-agenda-overriding-arguments) (cdr comp))
	(org-agenda-redo)
	(org-agenda-find-same-or-today-or-agenda)))
     (t (error "Cannot find today")))))

(defun org-agenda-find-same-or-today-or-agenda (&optional cnt)
  (goto-char
   (or (and cnt (text-property-any (point-min) (point-max) 'org-day-cnt cnt))
       (text-property-any (point-min) (point-max) 'org-today t)
       (text-property-any (point-min) (point-max) 'org-agenda-type 'agenda)
       (point-min))))

(defun org-agenda-later (arg)
  "Go forward in time by thee current span.
With prefix ARG, go forward that many times the current span."
  (interactive "p")
  (org-agenda-check-type t 'agenda)
  (let* ((span org-agenda-span)
	 (sd org-starting-day)
	 (greg (calendar-gregorian-from-absolute sd))
	 (cnt (get-text-property (point) 'org-day-cnt))
	 greg2 nd)
    (cond
     ((eq span 'day)
      (setq sd (+ arg sd) nd 1))
     ((eq span 'week)
      (setq sd (+ (* 7 arg) sd) nd 7))
     ((eq span 'month)
      (setq greg2 (list (+ (car greg) arg) (nth 1 greg) (nth 2 greg))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar greg2 (1+ (car greg2)))
      (setq nd (- (calendar-absolute-from-gregorian greg2) sd)))
     ((eq span 'year)
      (setq greg2 (list (car greg) (nth 1 greg) (+ arg (nth 2 greg)))
	    sd (calendar-absolute-from-gregorian greg2))
      (setcar (nthcdr 2 greg2) (1+ (nth 2 greg2)))
      (setq nd (- (calendar-absolute-from-gregorian greg2) sd))))
    (let ((org-agenda-overriding-arguments
	   (list (car org-agenda-last-arguments) sd nd t)))
      (org-agenda-redo)
      (org-agenda-find-same-or-today-or-agenda cnt))))

(defun org-agenda-earlier (arg)
  "Go backward in time by the current span.
With prefix ARG, go backward that many times the current span."
  (interactive "p")
  (org-agenda-later (- arg)))

(defun org-agenda-day-view (&optional day-of-year)
  "Switch to daily view for agenda.
With argument DAY-OF-YEAR, switch to that day of the year."
  (interactive "P")
  (setq org-agenda-ndays 1)
  (org-agenda-change-time-span 'day day-of-year))
(defun org-agenda-week-view (&optional iso-week)
  "Switch to daily view for agenda.
With argument ISO-WEEK, switch to the corresponding ISO week.
If ISO-WEEK has more then 2 digits, only the last two encode the
week.  Any digits before this encode a year.  So 200712 means
week 12 of year 2007.  Years in the range 1938-2037 can also be
written as 2-digit years."
  (interactive "P")
  (setq org-agenda-ndays 7)
  (org-agenda-change-time-span 'week iso-week))
(defun org-agenda-month-view (&optional month)
  "Switch to daily view for agenda.
With argument MONTH, switch to that month."
  (interactive "P")
  (org-agenda-change-time-span 'month month))
(defun org-agenda-year-view (&optional year)
  "Switch to daily view for agenda.
With argument YEAR, switch to that year.
If MONTH has more then 2 digits, only the last two encode the
month.  Any digits before this encode a year.  So 200712 means
December year 2007.  Years in the range 1938-2037 can also be
written as 2-digit years."
  (interactive "P")
  (when year
    (setq year (org-small-year-to-year year)))
  (if (y-or-n-p "Are you sure you want to compute the agenda for an entire year? ")
      (org-agenda-change-time-span 'year year)
    (error "Abort")))

(defun org-agenda-change-time-span (span &optional n)
  "Change the agenda view to SPAN.
SPAN may be `day', `week', `month', `year'."
  (org-agenda-check-type t 'agenda)
  (if (and (not n) (equal org-agenda-span span))
      (error "Viewing span is already \"%s\"" span))
  (let* ((sd (or (get-text-property (point) 'day)
		org-starting-day))
	 (computed (org-agenda-compute-time-span sd span n))
	 (org-agenda-overriding-arguments
	  (list (car org-agenda-last-arguments)
		(car computed) (cdr computed) t)))
    (org-agenda-redo)
    (org-agenda-find-same-or-today-or-agenda))
  (org-agenda-set-mode-name)
  (message "Switched to %s view" span))

(defun org-agenda-compute-time-span (sd span &optional n)
  "Compute starting date and number of days for agenda.
SPAN may be `day', `week', `month', `year'.  The return value
is a cons cell with the starting date and the number of days,
so that the date SD will be in that range."
  (let* ((greg (calendar-gregorian-from-absolute sd))
	 (dg (nth 1 greg))
	 (mg (car greg))
	 (yg (nth 2 greg))
	 nd w1 y1 m1 thisweek)
    (cond
     ((eq span 'day)
      (when n
	(setq sd (+ (calendar-absolute-from-gregorian
		     (list mg 1 yg))
		    n -1)))
      (setq nd 1))
     ((eq span 'week)
      (let* ((nt (calendar-day-of-week
		  (calendar-gregorian-from-absolute sd)))
	     (d (if org-agenda-start-on-weekday
		    (- nt org-agenda-start-on-weekday)
		  0)))
	(setq sd (- sd (+ (if (< d 0) 7 0) d)))
	(when n
	  (require 'cal-iso)
	  (setq thisweek (car (calendar-iso-from-absolute sd)))
	  (when (> n 99)
	    (setq y1 (org-small-year-to-year (/ n 100))
		  n (mod n 100)))
	  (setq sd
		(calendar-absolute-from-iso
		 (list n 1
		       (or y1 (nth 2 (calendar-iso-from-absolute sd)))))))
	(setq nd 7)))
     ((eq span 'month)
      (when (and n (> n 99))
	(setq y1 (org-small-year-to-year (/ n 100))
	      n (mod n 100)))
      (setq sd (calendar-absolute-from-gregorian
		(list (or n mg) 1 (or y1 yg)))
	    nd (- (calendar-absolute-from-gregorian
		   (list (1+ (or n mg)) 1 (or y1 yg)))
		  sd)))
     ((eq span 'year)
      (setq sd (calendar-absolute-from-gregorian
		(list 1 1 (or n yg)))
	    nd (- (calendar-absolute-from-gregorian
		   (list 1 1 (1+ (or n yg))))
		  sd))))
    (cons sd nd)))

(defun org-agenda-next-date-line (&optional arg)
  "Jump to the next line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (beginning-of-line 1)
  ;; This does not work if user makes date format that starts with a blank
  (if (looking-at "^\\S-") (forward-char 1))
  (if (not (re-search-forward "^\\S-" nil t arg))
      (progn
	(backward-char 1)
	(error "No next date after this line in this buffer")))
  (goto-char (match-beginning 0)))

(defun org-agenda-previous-date-line (&optional arg)
  "Jump to the previous line indicating a date in agenda buffer."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (beginning-of-line 1)
  (if (not (re-search-backward "^\\S-" nil t arg))
      (error "No previous date before this line in this buffer")))

;; Initialize the highlight
(defvar org-hl (org-make-overlay 1 1))
(org-overlay-put org-hl 'face 'highlight)

(defun org-highlight (begin end &optional buffer)
  "Highlight a region with overlay."
  (funcall (if (featurep 'xemacs) 'set-extent-endpoints 'move-overlay)
	   org-hl begin end (or buffer (current-buffer))))

(defun org-unhighlight ()
  "Detach overlay INDEX."
  (funcall (if (featurep 'xemacs) 'detach-extent 'delete-overlay) org-hl))

;; FIXME this is currently not used.
(defun org-highlight-until-next-command (beg end &optional buffer)
  "Move the highlight overlay to BEG/END, remove it before the next command."
  (org-highlight beg end buffer)
  (add-hook 'pre-command-hook 'org-unhighlight-once))
(defun org-unhighlight-once ()
  "Remove the highlight from its position, and this function from the hook."
  (remove-hook 'pre-command-hook 'org-unhighlight-once)
  (org-unhighlight))

(defun org-agenda-follow-mode ()
  "Toggle follow mode in an agenda buffer."
  (interactive)
  (setq org-agenda-follow-mode (not org-agenda-follow-mode))
  (org-agenda-set-mode-name)
  (message "Follow mode is %s"
	   (if org-agenda-follow-mode "on" "off")))

(defun org-agenda-clockreport-mode ()
  "Toggle clocktable mode in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-clockreport-mode (not org-agenda-clockreport-mode))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Clocktable mode is %s"
	   (if org-agenda-clockreport-mode "on" "off")))

(defun org-agenda-log-mode ()
  "Toggle log mode in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
  (setq org-agenda-show-log (not org-agenda-show-log))
  (org-agenda-set-mode-name)
  (org-agenda-redo)
  (message "Log mode is %s"
	   (if org-agenda-show-log "on" "off")))

(defun org-agenda-toggle-diary ()
  "Toggle diary inclusion in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-include-diary (not org-agenda-include-diary))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Diary inclusion turned %s"
	   (if org-agenda-include-diary "on" "off")))

(defun org-agenda-toggle-time-grid ()
  "Toggle time grid in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-agenda-use-time-grid (not org-agenda-use-time-grid))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Time-grid turned %s"
	   (if org-agenda-use-time-grid "on" "off")))

(defun org-agenda-set-mode-name ()
  "Set the mode name to indicate all the small mode settings."
  (setq mode-name
	(concat "Org-Agenda"
		(if (equal org-agenda-ndays 1) " Day"    "")
		(if (equal org-agenda-ndays 7) " Week"   "")
		(if org-agenda-follow-mode     " Follow" "")
		(if org-agenda-include-diary   " Diary"  "")
		(if org-agenda-use-time-grid   " Grid"   "")
		(if org-agenda-show-log        " Log"    "")
		(if org-agenda-clockreport-mode " Clock"   "")))
  (force-mode-line-update))

(defun org-agenda-post-command-hook ()
  (and (eolp) (not (bolp)) (backward-char 1))
  (setq org-agenda-type (get-text-property (point) 'org-agenda-type))
  (if (and org-agenda-follow-mode
	   (get-text-property (point) 'org-marker))
      (org-agenda-show)))

(defun org-agenda-show-priority ()
  "Show the priority of the current item.
This priority is composed of the main priority given with the [#A] cookies,
and by additional input from the age of a schedules or deadline entry."
  (interactive)
  (let* ((pri (get-text-property (point-at-bol) 'priority)))
    (message "Priority is %d" (if pri pri -1000))))

(defun org-agenda-show-tags ()
  "Show the tags applicable to the current item."
  (interactive)
  (let* ((tags (get-text-property (point-at-bol) 'tags)))
    (if tags
	(message "Tags are :%s:"
		 (org-no-properties (mapconcat 'identity tags ":")))
      (message "No tags associated with this line"))))

(defun org-agenda-goto (&optional highlight)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer-other-window buffer)
    (widen)
    (goto-char pos)
    (when (org-mode-p)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))) ; show the next heading
    (recenter (/ (window-height) 2))
    (run-hooks 'org-agenda-after-show-hook)
    (and highlight (org-highlight (point-at-bol) (point-at-eol)))))

(defvar org-agenda-after-show-hook nil
  "Normal hook run after an item has been shown from the agenda.
Point is in the buffer where the item originated.")

(defun org-agenda-kill ()
  "Kill the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (type (get-text-property (point) 'type))
	 dbeg dend (n 0) conf)
    (org-with-remote-undo buffer
     (with-current-buffer buffer
       (save-excursion
	 (goto-char pos)
	 (if (and (org-mode-p) (not (member type '("sexp"))))
	     (setq dbeg (progn (org-back-to-heading t) (point))
		   dend (org-end-of-subtree t t))
	   (setq dbeg (point-at-bol)
		 dend (min (point-max) (1+ (point-at-eol)))))
	 (goto-char dbeg)
	 (while (re-search-forward "^[ \t]*\\S-" dend t) (setq n (1+ n)))))
     (setq conf (or (eq t org-agenda-confirm-kill)
		    (and (numberp org-agenda-confirm-kill)
			 (> n org-agenda-confirm-kill))))
     (and conf
	  (not (y-or-n-p
		(format "Delete entry with %d lines in buffer \"%s\"? "
			n (buffer-name buffer))))
	  (error "Abort"))
     (org-remove-subtree-entries-from-agenda buffer dbeg dend)
     (with-current-buffer buffer (delete-region dbeg dend))
     (message "Agenda item and source killed"))))

(defun org-agenda-archive ()
  "Archive the entry or subtree belonging to the current agenda entry."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(if (org-mode-p)
	    (save-excursion
	      (goto-char pos)
	      (org-remove-subtree-entries-from-agenda)
	      (org-back-to-heading t)
	      (org-archive-subtree))
	  (error "Archiving works only in Org-mode files"))))))

(defun org-agenda-archive-to-archive-sibling ()
  "Move the entry to the archive sibling."
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Not in agenda"))
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(if (org-mode-p)
	    (save-excursion
	      (goto-char pos)
	      (org-remove-subtree-entries-from-agenda)
	      (org-back-to-heading t)
	      (org-archive-to-archive-sibling))
	  (error "Archiving works only in Org-mode files"))))))

(defun org-remove-subtree-entries-from-agenda (&optional buf beg end)
  "Remove all lines in the agenda that correspond to a given subtree.
The subtree is the one in buffer BUF, starting at BEG and ending at END.
If this information is not given, the function uses the tree at point."
  (let ((buf (or buf (current-buffer))) m p)
    (save-excursion
      (unless (and beg end)
	(org-back-to-heading t)
	(setq beg (point))
	(org-end-of-subtree t)
	(setq end (point)))
      (set-buffer (get-buffer org-agenda-buffer-name))
      (save-excursion
	(goto-char (point-max))
	(beginning-of-line 1)
	(while (not (bobp))
	  (when (and (setq m (get-text-property (point) 'org-marker))
		     (equal buf (marker-buffer m))
		     (setq p (marker-position m))
		     (>= p beg)
		     (<= p end))
	    (let ((inhibit-read-only t))
	      (delete-region (point-at-bol) (1+ (point-at-eol)))))
	  (beginning-of-line 0))))))

(defun org-agenda-open-link ()
  "Follow the link in the current line, if any."
  (interactive)
  (org-agenda-copy-local-variable 'org-link-abbrev-alist-local)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (org-open-at-point))))

(defun org-agenda-copy-local-variable (var)
  "Get a variable from a referenced buffer and install it here."
  (let ((m (get-text-property (point) 'org-marker)))
    (when (and m (buffer-live-p (marker-buffer m)))
      (org-set-local var (with-current-buffer (marker-buffer m)
			   (symbol-value var))))))

(defun org-agenda-switch-to (&optional delete-other-windows)
  "Go to the Org-mode file which contains the item at point."
  (interactive)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (switch-to-buffer buffer)
    (and delete-other-windows (delete-other-windows))
    (widen)
    (goto-char pos)
    (when (org-mode-p)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil))))))  ; show the next heading

(defun org-agenda-goto-mouse (ev)
  "Go to the Org-mode file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-goto))

(defun org-agenda-show ()
  "Display the Org-mode file which contains the item at point."
  (interactive)
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (select-window win)))

(defun org-agenda-recenter (arg)
  "Display the Org-mode file which contains the item at point and recenter."
  (interactive "P")
  (let ((win (selected-window)))
    (org-agenda-goto t)
    (recenter arg)
    (select-window win)))

(defun org-agenda-show-mouse (ev)
  "Display the Org-mode file which contains the item at the mouse click."
  (interactive "e")
  (mouse-set-point ev)
  (org-agenda-show))

(defun org-agenda-check-no-diary ()
  "Check if the entry is a diary link and abort if yes."
  (if (get-text-property (point) 'org-agenda-diary-link)
      (org-agenda-error)))

(defun org-agenda-error ()
  (error "Command not allowed in this line"))

(defun org-agenda-tree-to-indirect-buffer ()
  "Show the subtree corresponding to the current entry in an indirect buffer.
This calls the command `org-tree-to-indirect-buffer' from the original
Org-mode buffer.
With numerical prefix arg ARG, go up to this level and then take that tree.
With a C-u prefix, make a separate frame for this tree (i.e. don't use the
dedicated frame)."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
	(goto-char pos)
	(call-interactively 'org-tree-to-indirect-buffer)))))

(defvar org-last-heading-marker (make-marker)
  "Marker pointing to the headline that last changed its TODO state
by a remote command from the agenda.")

(defun org-agenda-todo-nextset ()
  "Switch TODO entry to next sequence."
  (interactive)
  (org-agenda-todo 'nextset))

(defun org-agenda-todo-previousset ()
  "Switch TODO entry to previous sequence."
  (interactive)
  (org-agenda-todo 'previousset))

(defun org-agenda-todo (&optional arg)
  "Cycle TODO state of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((col (current-column))
	 (marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(org-todo arg)
	(and (bolp) (forward-char 1))
	(setq newhead (org-get-heading))
	(save-excursion
	  (org-back-to-heading)
	  (move-marker org-last-heading-marker (point))))
      (beginning-of-line 1)
      (save-excursion
	(org-agenda-change-all-lines newhead hdmarker 'fixface))
      (org-move-to-column col))))

(defun org-agenda-add-note (&optional arg)
  "Add a time-stamped note to the entry at point."
  (interactive "P")
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (inhibit-read-only t))
    (with-current-buffer buffer
      (widen)
      (goto-char pos)
      (org-show-context 'agenda)
      (save-excursion
	(and (outline-next-heading)
	     (org-flag-heading nil)))   ; show the next heading
      (org-add-note))))

(defun org-agenda-change-all-lines (newhead hdmarker &optional fixface)
  "Change all lines in the agenda buffer which match HDMARKER.
The new content of the line will be NEWHEAD (as modified by
`org-format-agenda-item').  HDMARKER is checked with
`equal' against all `org-hd-marker' text properties in the file.
If FIXFACE is non-nil, the face of each item is modified acording to
the new TODO state."
  (let* ((inhibit-read-only t)
	 props m pl undone-face done-face finish new dotime cat tags)
    (save-excursion
      (goto-char (point-max))
      (beginning-of-line 1)
      (while (not finish)
	(setq finish (bobp))
	(when (and (setq m (get-text-property (point) 'org-hd-marker))
		   (equal m hdmarker))
	  (setq props (text-properties-at (point))
		dotime (get-text-property (point) 'dotime)
		cat (get-text-property (point) 'org-category)
		tags (get-text-property (point) 'tags)
		new (org-format-agenda-item "x" newhead cat tags dotime 'noprefix)
		pl (get-text-property (point) 'prefix-length)
		undone-face (get-text-property (point) 'undone-face)
		done-face (get-text-property (point) 'done-face))
	  (org-move-to-column pl)
	  (cond
	   ((equal new "")
	    (beginning-of-line 1)
	    (and (looking-at ".*\n?") (replace-match "")))
	   ((looking-at ".*")
	    (replace-match new t t)
	    (beginning-of-line 1)
	    (add-text-properties (point-at-bol) (point-at-eol) props)
	    (when fixface
	      (add-text-properties
	       (point-at-bol) (point-at-eol)
	       (list 'face
		     (if org-last-todo-state-is-todo
			 undone-face done-face))))
	    (org-agenda-highlight-todo 'line)
	    (beginning-of-line 1))
	   (t (error "Line update did not work"))))
	(beginning-of-line 0)))
    (org-finalize-agenda)))

(defun org-agenda-align-tags (&optional line)
  "Align all tags in agenda items to `org-agenda-tags-column'."
  (let ((inhibit-read-only t) l c)
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (re-search-forward (org-re "\\([ \t]+\\)\\(:[[:alnum:]_@:]+:\\)[ \t]*$")
				(if line (point-at-eol) nil) t)
	(add-text-properties
	 (match-beginning 2) (match-end 2)
	 (list 'face (delq nil (list 'org-tag (get-text-property
					       (match-beginning 2) 'face)))))
	(setq l (- (match-end 2) (match-beginning 2))
	      c (if (< org-agenda-tags-column 0)
		    (- (abs org-agenda-tags-column) l)
		  org-agenda-tags-column))
	(delete-region (match-beginning 1) (match-end 1))
	(goto-char (match-beginning 1))
	(insert (org-add-props
		    (make-string (max 1 (- c (current-column))) ?\ )
		    (text-properties-at (point))))))))

(defun org-agenda-priority-up ()
  "Increase the priority of line at point, also in Org-mode file."
  (interactive)
  (org-agenda-priority 'up))

(defun org-agenda-priority-down ()
  "Decrease the priority of line at point, also in Org-mode file."
  (interactive)
  (org-agenda-priority 'down))

(defun org-agenda-priority (&optional force-direction)
  "Set the priority of line at point, also in Org-mode file.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (hdmarker (get-text-property (point) 'org-hd-marker))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(funcall 'org-priority force-direction)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

;; FIXME: should fix the tags property of the agenda line.
(defun org-agenda-set-tags ()
  "Set tags for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (if (and (org-region-active-p) (interactive-p))
      (call-interactively 'org-change-tag-in-region)
    (org-agenda-show)   ;;; FIXME This is a stupid hack and should not be needed
    (let* ((hdmarker (or (get-text-property (point) 'org-hd-marker)
			 (org-agenda-error)))
	   (buffer (marker-buffer hdmarker))
	   (pos (marker-position hdmarker))
	   (inhibit-read-only t)
	   newhead)
      (org-with-remote-undo buffer
	(with-current-buffer buffer
	  (widen)
	  (goto-char pos)
	  (save-excursion
	    (org-show-context 'agenda))
	  (save-excursion
	    (and (outline-next-heading)
		 (org-flag-heading nil)))   ; show the next heading
	  (goto-char pos)
	  (call-interactively 'org-set-tags)
	  (end-of-line 1)
	  (setq newhead (org-get-heading)))
	(org-agenda-change-all-lines newhead hdmarker)
	(beginning-of-line 1)))))

(defun org-agenda-toggle-archive-tag ()
  "Toggle the archive tag for the current entry."
  (interactive)
  (org-agenda-check-no-diary)
  (org-agenda-show)   ;;; FIXME This is a stupid hack and should not be needed
  (let* ((hdmarker (or (get-text-property (point) 'org-hd-marker)
                       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(save-excursion
	  (and (outline-next-heading)
	       (org-flag-heading nil)))   ; show the next heading
	(call-interactively 'org-toggle-archive-tag)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

(defun org-agenda-date-later (arg &optional what)
  "Change the date of this item to one day later."
  (interactive "p")
  (org-agenda-check-type t 'agenda 'timeline)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
     (with-current-buffer buffer
       (widen)
       (goto-char pos)
       (if (not (org-at-timestamp-p))
	   (error "Cannot find time stamp"))
       (org-timestamp-change arg (or what 'day)))
     (org-agenda-show-new-time marker org-last-changed-timestamp))
    (message "Time stamp changed to %s" org-last-changed-timestamp)))

(defun org-agenda-date-earlier (arg &optional what)
  "Change the date of this item to one day earlier."
  (interactive "p")
  (org-agenda-date-later (- arg) what))

(defun org-agenda-show-new-time (marker stamp &optional prefix)
  "Show new date stamp via text properties."
  ;; We use text properties to make this undoable
  (let ((inhibit-read-only t))
    (setq stamp (concat " " prefix " => " stamp))
    (save-excursion
      (goto-char (point-max))
      (while (not (bobp))
	(when (equal marker (get-text-property (point) 'org-marker))
	  (org-move-to-column (- (window-width) (length stamp)) t)
          (if (featurep 'xemacs)
	      ;; Use `duplicable' property to trigger undo recording
              (let ((ex (make-extent nil nil))
                    (gl (make-glyph stamp)))
                (set-glyph-face gl 'secondary-selection)
                (set-extent-properties
                 ex (list 'invisible t 'end-glyph gl 'duplicable t))
                (insert-extent ex (1- (point)) (point-at-eol)))
            (add-text-properties
             (1- (point)) (point-at-eol)
	     (list 'display (org-add-props stamp nil
			      'face 'secondary-selection))))
	  (beginning-of-line 1))
	(beginning-of-line 0)))))

(defun org-agenda-date-prompt (arg)
  "Change the date of this item.  Date is prompted for, with default today.
The prefix ARG is passed to the `org-time-stamp' command and can therefore
be used to request time specification in the time stamp."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(if (not (org-at-timestamp-p))
	    (error "Cannot find time stamp"))
	(org-time-stamp arg)
	(message "Time stamp changed to %s" org-last-changed-timestamp)))))

(defun org-agenda-schedule (arg)
  "Schedule the item at point."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (type (marker-insertion-type marker))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (org-insert-labeled-timestamps-at-point nil)
	 ts)
    (when type (message "%s" type) (sit-for 3))
    (set-marker-insertion-type marker t)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-schedule arg)))
      (org-agenda-show-new-time marker ts "S"))
    (message "Item scheduled for %s" ts)))

(defun org-agenda-deadline (arg)
  "Schedule the item at point."
  (interactive "P")
  (org-agenda-check-type t 'agenda 'timeline 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let* ((marker (or (get-text-property (point) 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (org-insert-labeled-timestamps-at-point nil)
	 ts)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(setq ts (org-deadline arg)))
      (org-agenda-show-new-time marker ts "S"))
	(message "Deadline for this item set to %s" ts)))

(defun org-agenda-clock-in (&optional arg)
  "Start the clock on the currently selected item."
  (interactive "P")
  (org-agenda-check-no-diary)
  (if (equal arg '(4))
      (org-clock-in arg)
    (let* ((marker (or (get-text-property (point) 'org-marker)
		       (org-agenda-error)))
	   (pos (marker-position marker)))
      (org-with-remote-undo (marker-buffer marker)
        (with-current-buffer (marker-buffer marker)
	  (widen)
	  (goto-char pos)
	  (org-clock-in arg))))))

(defun org-agenda-clock-out (&optional arg)
  "Stop the currently running clock."
  (interactive "P")
  (unless (marker-buffer org-clock-marker)
    (error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-out)))

(defun org-agenda-clock-cancel (&optional arg)
  "Cancel the currently running clock."
  (interactive "P")
  (unless (marker-buffer org-clock-marker)
    (error "No running clock"))
  (org-with-remote-undo (marker-buffer org-clock-marker)
    (org-clock-cancel)))

(defun org-agenda-diary-entry ()
  "Make a diary entry, like the `i' command from the calendar.
All the standard commands work: block, weekly etc."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
  (require 'diary-lib)
  (let* ((char (progn
		 (message "Diary entry: [d]ay [w]eekly [m]onthly [y]early [a]nniversary [b]lock [c]yclic")
		 (read-char-exclusive)))
	 (cmd (cdr (assoc char
			  '((?d . insert-diary-entry)
			    (?w . insert-weekly-diary-entry)
			    (?m . insert-monthly-diary-entry)
			    (?y . insert-yearly-diary-entry)
			    (?a . insert-anniversary-diary-entry)
			    (?b . insert-block-diary-entry)
			    (?c . insert-cyclic-diary-entry)))))
	 (oldf (symbol-function 'calendar-cursor-to-date))
;	 (buf (get-file-buffer (substitute-in-file-name diary-file)))
	 (point (point))
	 (mark (or (mark t) (point))))
    (unless cmd
      (error "No command associated with <%c>" char))
    (unless (and (get-text-property point 'day)
		 (or (not (equal ?b char))
		     (get-text-property mark 'day)))
      (error "Don't know which date to use for diary entry"))
    ;; We implement this by hacking the `calendar-cursor-to-date' function
    ;; and the `calendar-mark-ring' variable.  Saves a lot of code.
    (let ((calendar-mark-ring
	   (list (calendar-gregorian-from-absolute
		  (or (get-text-property mark 'day)
		      (get-text-property point 'day))))))
      (unwind-protect
	  (progn
	    (fset 'calendar-cursor-to-date
		  (lambda (&optional error)
		    (calendar-gregorian-from-absolute
		     (get-text-property point 'day))))
	      (call-interactively cmd))
	(fset 'calendar-cursor-to-date oldf)))))


(defun org-agenda-execute-calendar-command (cmd)
  "Execute a calendar command from the agenda, with the date associated to
the cursor position."
  (org-agenda-check-type t 'agenda 'timeline)
  (require 'diary-lib)
  (unless (get-text-property (point) 'day)
    (error "Don't know which date to use for calendar command"))
  (let* ((oldf (symbol-function 'calendar-cursor-to-date))
	 (point (point))
	 (date (calendar-gregorian-from-absolute
		(get-text-property point 'day)))
         ;; the following 2 vars are needed in the calendar
	 (displayed-month (car date))
	 (displayed-year (nth 2 date)))
      (unwind-protect
	  (progn
	    (fset 'calendar-cursor-to-date
		  (lambda (&optional error)
		    (calendar-gregorian-from-absolute
		     (get-text-property point 'day))))
	    (call-interactively cmd))
	(fset 'calendar-cursor-to-date oldf))))

(defun org-agenda-phases-of-moon ()
  "Display the phases of the moon for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'calendar-phases-of-moon))

(defun org-agenda-holidays ()
  "Display the holidays for the 3 months around the cursor date."
  (interactive)
  (org-agenda-execute-calendar-command 'list-calendar-holidays))

(defvar calendar-longitude)
(defvar calendar-latitude)
(defvar calendar-location-name)

(defun org-agenda-sunrise-sunset (arg)
  "Display sunrise and sunset for the cursor date.
Latitude and longitude can be specified with the variables
`calendar-latitude' and `calendar-longitude'.  When called with prefix
argument, latitude and longitude will be prompted for."
  (interactive "P")
  (require 'solar)
  (let ((calendar-longitude (if arg nil calendar-longitude))
	(calendar-latitude  (if arg nil calendar-latitude))
	(calendar-location-name
	 (if arg "the given coordinates" calendar-location-name)))
    (org-agenda-execute-calendar-command 'calendar-sunrise-sunset)))

(defun org-agenda-goto-calendar ()
  "Open the Emacs calendar with the date at the cursor."
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
  (let* ((day (or (get-text-property (point) 'day)
		  (error "Don't know which date to open in calendar")))
	 (date (calendar-gregorian-from-absolute day))
	 (calendar-move-hook nil)
	 (calendar-view-holidays-initially-flag nil)
	 (calendar-view-diary-initially-flag nil)
	 (view-calendar-holidays-initially nil)
	 (calendar-view-diary-initially-flag nil)
	 (calendar-view-holidays-initially-flag nil)
	 (view-diary-entries-initially nil))
    (calendar)
    (calendar-goto-date date)))

;;;###autoload
(defun org-calendar-goto-agenda ()
  "Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'."
  (interactive)
  (org-agenda-list nil (calendar-absolute-from-gregorian
			(calendar-cursor-to-date))
		   nil))

(defun org-agenda-convert-date ()
  (interactive)
  (org-agenda-check-type t 'agenda 'timeline)
  (let ((day (get-text-property (point) 'day))
	date s)
    (unless day
      (error "Don't know which date to convert"))
    (setq date (calendar-gregorian-from-absolute day))
    (setq s (concat
	     "Gregorian:  " (calendar-date-string date) "\n"
	     "ISO:        " (calendar-iso-date-string date) "\n"
	     "Day of Yr:  " (calendar-day-of-year-string date) "\n"
	     "Julian:     " (calendar-julian-date-string date) "\n"
	     "Astron. JD: " (calendar-astro-date-string date)
	     " (Julian date number at noon UTC)\n"
	     "Hebrew:     " (calendar-hebrew-date-string date) " (until sunset)\n"
	     "Islamic:    " (calendar-islamic-date-string date) " (until sunset)\n"
	     "French:     " (calendar-french-date-string date) "\n"
	     "Baha'i:     " (calendar-bahai-date-string date) " (until sunset)\n"
	     "Mayan:      " (calendar-mayan-date-string date) "\n"
	     "Coptic:     " (calendar-coptic-date-string date) "\n"
	     "Ethiopic:   " (calendar-ethiopic-date-string date) "\n"
	     "Persian:    " (calendar-persian-date-string date) "\n"
	     "Chinese:    " (calendar-chinese-date-string date) "\n"))
    (with-output-to-temp-buffer "*Dates*"
      (princ s))
    (if (fboundp 'fit-window-to-buffer)
	(fit-window-to-buffer (get-buffer-window "*Dates*")))))

;;; Appointment reminders

(defvar appt-time-msg-list)

;;;###autoload
(defun org-agenda-to-appt (&optional refresh filter)
  "Activate appointments found in `org-agenda-files'.
With a \\[universal-argument] prefix, refresh the list of
appointements.

If FILTER is t, interactively prompt the user for a regular
expression, and filter out entries that don't match it.

If FILTER is a string, use this string as a regular expression
for filtering entries out.

FILTER can also be an alist with the car of each cell being
either 'headline or 'category.  For example:

  '((headline \"IMPORTANT\")
    (category \"Work\"))

will only add headlines containing IMPORTANT or headlines
belonging to the \"Work\" category."
  (interactive "P")
  (require 'calendar)
  (if refresh (setq appt-time-msg-list nil))
  (if (eq filter t)
      (setq filter (read-from-minibuffer "Regexp filter: ")))
  (let* ((cnt 0) ; count added events
	 (org-agenda-new-buffers nil)
	 (org-deadline-warning-days 0)
	 (today (org-date-to-gregorian
		 (time-to-days (current-time))))
	 (files (org-agenda-files)) entries file)
    ;; Get all entries which may contain an appt
    (while (setq file (pop files))
      (setq entries
	    (append entries
		    (org-agenda-get-day-entries
		     file today :timestamp :scheduled :deadline))))
    (setq entries (delq nil entries))
    ;; Map thru entries and find if we should filter them out
    (mapc
     (lambda(x)
       (let* ((evt (org-trim (get-text-property 1 'txt x)))
	      (cat (get-text-property 1 'org-category x))
	      (tod (get-text-property 1 'time-of-day x))
	      (ok (or (null filter)
		      (and (stringp filter) (string-match filter evt))
		      (and (listp filter)
			   (or (string-match
				(cadr (assoc 'category filter)) cat)
			       (string-match
				(cadr (assoc 'headline filter)) evt))))))
	 ;; FIXME: Shall we remove text-properties for the appt text?
	 ;; (setq evt (set-text-properties 0 (length evt) nil evt))
	 (when (and ok tod)
	   (setq tod (number-to-string tod)
		 tod (when (string-match
			    "\\([0-9]\\{1,2\\}\\)\\([0-9]\\{2\\}\\)" tod)
		       (concat (match-string 1 tod) ":"
			       (match-string 2 tod))))
	   (appt-add tod evt)
	   (setq cnt (1+ cnt))))) entries)
    (org-release-buffers org-agenda-new-buffers)
    (if (eq cnt 0)
	(message "No event to add")
      (message "Added %d event%s for today" cnt (if (> cnt 1) "s" "")))))

(provide 'org-agenda)

;;; org-agenda.el ends here

