
;;;### (autoloads (org-cycle-agenda-files org-iswitchb org-open-link-from-string
;;;;;;  org-open-at-point-global org-insert-link-global org-store-link
;;;;;;  org-run-like-in-org-mode turn-on-orgstruct++ turn-on-orgstruct
;;;;;;  orgstruct-mode org-global-cycle org-cycle org-mode) "org"
;;;;;;  "lisp/org.el" (18488 62361))
;;; Generated autoloads from lisp/org.el

(autoload 'org-mode "org" "\
Outline-based notes management and organizer, alias
\"Carsten's outline-mode for keeping track of everything.\"

Org-mode develops organizational tasks around a NOTES file which
contains information about projects as plain text.  Org-mode is
implemented on top of outline-mode, which is ideal to keep the content
of large files well structured.  It supports ToDo items, deadlines and
time stamps, which magically appear in the diary listing of the Emacs
calendar.  Tables are easily created with a built-in table editor.
Plain text URL-like links connect to websites, emails (VM), Usenet
messages (Gnus), BBDB entries, and any files related to the project.
For printing and sharing of notes, an Org-mode file (or a part of it)
can be exported as a structured ASCII or HTML file.

The following commands are available:

\\{org-mode-map}

\(fn)" t nil)

(autoload 'org-cycle "org" "\
Visibility cycling for Org-mode.

- When this function is called with a prefix argument, rotate the entire
  buffer through 3 states (global cycling)
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
  When called with two C-c C-u prefixes, switch to the startup visibility,
  determined by the variable `org-startup-folded', and by any VISIBILITY
  properties in the buffer.

- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states (local cycling)
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.
               From this state, you can move to one of the children
               and zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.

- When there is a numeric prefix, go up to a heading with level ARG, do
  a `show-subtree' and return to the previous cursor position.  If ARG
  is negative, go up that many levels.

- When point is not at the beginning of a headline, execute the global
  binding for TAB, which is re-indenting the line.  See the option
  `org-cycle-emulate-tab' for details.

- Special case: if point is at the beginning of the buffer and there is
  no headline in line 1, this function will act as if called with prefix arg.
  But only if also the variable `org-cycle-global-at-bob' is t.

\(fn &optional ARG)" t nil)

(autoload 'org-global-cycle "org" "\
Cycle the global visibility.  For details see `org-cycle'.
With C-u prefix arg, switch to startup visibility.
With a numeric prefix, show all headlines up to that level.

\(fn &optional ARG)" t nil)

(autoload 'orgstruct-mode "org" "\
Toggle the minor more `orgstruct-mode'.
This mode is for using Org-mode structure commands in other modes.
The following key behave as if Org-mode was active, if the cursor
is on a headline, or on a plain list item (both in the definition
of Org-mode).

M-up        Move entry/item up
M-down	    Move entry/item down
M-left	    Promote
M-right	    Demote
M-S-up	    Move entry/item up
M-S-down    Move entry/item down
M-S-left    Promote subtree
M-S-right   Demote subtree
M-q	    Fill paragraph and items like in Org-mode
C-c ^	    Sort entries
C-c -	    Cycle list bullet
TAB         Cycle item visibility
M-RET       Insert new heading/item
S-M-RET     Insert new TODO heading / Chekbox item
C-c C-c     Set tags / toggle checkbox

\(fn &optional ARG)" t nil)

(autoload 'turn-on-orgstruct "org" "\
Unconditionally turn on `orgstruct-mode'.

\(fn)" nil nil)

(autoload 'turn-on-orgstruct++ "org" "\
Unconditionally turn on `orgstruct-mode', and force org-mode indentations.
In addition to setting orgstruct-mode, this also exports all indentation and
autofilling variables from org-mode into the buffer.  Note that turning
off orgstruct-mode will *not* remove these additional settings.

\(fn)" nil nil)

(autoload 'org-run-like-in-org-mode "org" "\
Not documented

\(fn CMD)" nil nil)

(autoload 'org-store-link "org" "\
\\<org-mode-map>Store an org-link to the current location.
This link is added to `org-stored-links' and can later be inserted
into an org-buffer with \\[org-insert-link].

For some link types, a prefix arg is interpreted:
For links to usenet articles, arg negates `org-usenet-links-prefer-google'.
For file links, arg negates `org-context-in-file-links'.

\(fn ARG)" t nil)

(autoload 'org-insert-link-global "org" "\
Insert a link like Org-mode does.
This command can be called in any mode to insert a link in Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-at-point-global "org" "\
Follow a link like Org-mode does.
This command can be called in any mode to follow a link that has
Org-mode syntax.

\(fn)" t nil)

(autoload 'org-open-link-from-string "org" "\
Open a link in the string S, as if it was in Org-mode.

\(fn S &optional ARG)" t nil)

(autoload 'org-iswitchb "org" "\
Use `iswitchb-read-buffer' to prompt for an Org buffer to switch to.
With a prefix argument, restrict available to files.
With two prefix arguments, restrict available buffers to agenda files.

Due to some yet unresolved reason, global function
`iswitchb-mode' needs to be active for this function to work.

\(fn &optional ARG)" t nil)

(autoload 'org-cycle-agenda-files "org" "\
Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file.

\(fn)" t nil)

;;;***

;;;### (autoloads (org-agenda-to-appt org-calendar-goto-agenda org-diary
;;;;;;  org-agenda-list-stuck-projects org-tags-view org-todo-list
;;;;;;  org-search-view org-agenda-list org-batch-store-agenda-views
;;;;;;  org-store-agenda-views org-batch-agenda-csv org-batch-agenda
;;;;;;  org-agenda) "org-agenda" "lisp/org-agenda.el" (18488 31723))
;;; Generated autoloads from lisp/org-agenda.el

(autoload 'org-agenda "org-agenda" "\
Dispatch agenda commands to collect entries to the agenda buffer.
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
\(if active).

\(fn ARG &optional KEYS RESTRICTION)" t nil)

(autoload 'org-batch-agenda "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
If CMD-KEY is a string of length 1, it is used as a key in
`org-agenda-custom-commands' and triggers this command.  If it is a
longer string it is used as a tags/todo match string.
Paramters are alternating variable names and values that will be bound
before running the agenda command.

\(fn CMD-KEY &rest PARAMETERS)" nil (quote macro))

(autoload 'org-batch-agenda-csv "org-agenda" "\
Run an agenda command in batch mode and send the result to STDOUT.
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
agenda-day   The day in the agenda where this is listed

\(fn CMD-KEY &rest PARAMETERS)" nil (quote macro))

(autoload 'org-store-agenda-views "org-agenda" "\
Not documented

\(fn &rest PARAMETERS)" t nil)

(autoload 'org-batch-store-agenda-views "org-agenda" "\
Run all custom agenda commands that have a file argument.

\(fn &rest PARAMETERS)" nil (quote macro))

(autoload 'org-agenda-list "org-agenda" "\
Produce a daily/weekly view from all files in variable `org-agenda-files'.
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
given in `org-agenda-start-on-weekday'.

\(fn &optional INCLUDE-ALL START-DAY NDAYS)" t nil)

(autoload 'org-search-view "org-agenda" "\
Show all entries that contain words or regular expressions.
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
in `org-agenda-text-search-extra-files'.

\(fn &optional TODO-ONLY STRING EDIT-AT)" t nil)

(autoload 'org-todo-list "org-agenda" "\
Show all TODO entries from all agenda file in a single list.
The prefix arg can be used to select a specific TODO keyword and limit
the list to these.  When using \\[universal-argument], you will be prompted
for a keyword.  A numeric prefix directly selects the Nth keyword in
`org-todo-keywords-1'.

\(fn ARG)" t nil)

(autoload 'org-tags-view "org-agenda" "\
Show all headlines for all `org-agenda-files' matching a TAGS criterion.
The prefix arg TODO-ONLY limits the search to TODO entries.

\(fn &optional TODO-ONLY MATCH)" t nil)

(autoload 'org-agenda-list-stuck-projects "org-agenda" "\
Create agenda view for projects that are stuck.
Stuck projects are project that have no next actions.  For the definitions
of what a project is and how to check if it stuck, customize the variable
`org-stuck-projects'.
MATCH is being ignored.

\(fn &rest IGNORE)" t nil)

(autoload 'org-diary "org-agenda" "\
Return diary information from org-files.
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
function from a program - use `org-agenda-get-day-entries' instead.

\(fn &rest ARGS)" nil nil)

(autoload 'org-calendar-goto-agenda "org-agenda" "\
Compute the Org-mode agenda for the calendar date displayed at the cursor.
This is a command that has to be installed in `calendar-mode-map'.

\(fn)" t nil)

(autoload 'org-agenda-to-appt "org-agenda" "\
Activate appointments found in `org-agenda-files'.
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
belonging to the \"Work\" category.

\(fn &optional REFRESH FILTER)" t nil)

;;;***

;;;### (autoloads (org-bbdb-anniversaries) "org-bbdb" "lisp/org-bbdb.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-bbdb.el

(autoload 'org-bbdb-anniversaries "org-bbdb" "\
Extract anniversaries from BBDB for display in the agenda.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-get-clocktable) "org-clock" "lisp/org-clock.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-clock.el

(autoload 'org-get-clocktable "org-clock" "\
Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned.

\(fn &rest PROPS)" nil nil)

;;;***

;;;### (autoloads (org-export-as-xoxo org-export-icalendar-combine-agenda-files
;;;;;;  org-export-icalendar-all-agenda-files org-export-icalendar-this-file
;;;;;;  org-export-htmlize-generate-css org-export-as-html org-export-region-as-html
;;;;;;  org-replace-region-by-html org-export-as-html-to-buffer org-export-as-html-batch
;;;;;;  org-export-as-html-and-open org-insert-export-options-template
;;;;;;  org-export-visible org-export-as-ascii org-export) "org-exp"
;;;;;;  "lisp/org-exp.el" (18488 31723))
;;; Generated autoloads from lisp/org-exp.el

(autoload 'org-export "org-exp" "\
Export dispatcher for Org-mode.
When `org-export-run-in-background' is non-nil, try to run the command
in the background.  This will be done only for commands that write
to a file.  For details see the docstring of `org-export-run-in-background'.

The prefix argument ARG will be passed to the exporter.  However, if
ARG is a double universal prefix `C-u C-u', that means to inverse the
value of `org-export-run-in-background'.

\(fn &optional ARG)" t nil)

(autoload 'org-export-as-ascii "org-exp" "\
Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3.

\(fn ARG)" t nil)

(autoload 'org-export-visible "org-exp" "\
Create a copy of the visible part of the current buffer, and export it.
The copy is created in a temporary buffer and removed after use.
TYPE is the final key (as a string) that also select the export command in
the `C-c C-e' export dispatcher.
As a special case, if the you type SPC at the prompt, the temporary
org-mode file will not be removed but presented to you so that you can
continue to use it.  The prefix arg ARG is passed through to the exporting
command.

\(fn TYPE ARG)" t nil)

(autoload 'org-insert-export-options-template "org-exp" "\
Insert into the buffer a template with information for exporting.

\(fn)" t nil)

(autoload 'org-export-as-html-and-open "org-exp" "\
Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists.

\(fn ARG)" t nil)

(autoload 'org-export-as-html-batch "org-exp" "\
Call `org-export-as-html', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-html-batch

\(fn)" nil nil)

(autoload 'org-export-as-html-to-buffer "org-exp" "\
Call `org-exort-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'.

\(fn ARG)" t nil)

(autoload 'org-replace-region-by-html "org-exp" "\
Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-html "org-exp" "\
Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-html "org-exp" "\
Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  When HIDDEN is non-nil, don't display the HTML buffer.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

(autoload 'org-export-htmlize-generate-css "org-exp" "\
Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-htmlize-output-type' to `css', calls to
the function `org-export-htmlize-region-for-paste' will produce code
that uses these same face definitions.

\(fn)" t nil)

(autoload 'org-export-icalendar-this-file "org-exp" "\
Export current file as an iCalendar file.
The iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'.

\(fn)" t nil)

(autoload 'org-export-icalendar-all-agenda-files "org-exp" "\
Export all files in `org-agenda-files' to iCalendar .ics files.
Each iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'.

\(fn)" t nil)

(autoload 'org-export-icalendar-combine-agenda-files "org-exp" "\
Export all files in `org-agenda-files' to a single combined iCalendar file.
The file is stored under the name `org-combined-agenda-icalendar-file'.

\(fn)" t nil)

(autoload 'org-export-as-xoxo "org-exp" "\
Export the org buffer as XOXO.
The XOXO buffer is named *xoxo-<source buffer name>*

\(fn &optional BUFFER)" t nil)

;;;***

;;;### (autoloads (org-export-as-latex org-export-region-as-latex
;;;;;;  org-replace-region-by-latex org-export-as-latex-to-buffer
;;;;;;  org-export-as-latex-batch) "org-export-latex" "lisp/org-export-latex.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-export-latex.el

(autoload 'org-export-as-latex-batch "org-export-latex" "\
Call `org-export-as-latex', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-latex-batch

\(fn)" nil nil)

(autoload 'org-export-as-latex-to-buffer "org-export-latex" "\
Call `org-exort-as-latex` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-latex'.

\(fn ARG)" t nil)

(autoload 'org-replace-region-by-latex "org-export-latex" "\
Replace the region from BEG to END with its LaTeX export.
It assumes the region has `org-mode' syntax, and then convert it to
LaTeX.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an LaTeX buffer and
then use this command to convert it.

\(fn BEG END)" t nil)

(autoload 'org-export-region-as-latex "org-export-latex" "\
Convert region from BEG to END in `org-mode' buffer to LaTeX.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted LaTeX.  If BUFFER is the symbol `string', return the
produced LaTeX as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq latex (org-export-region-as-latex beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only retunr the buffer.

\(fn BEG END &optional BODY-ONLY BUFFER)" t nil)

(autoload 'org-export-as-latex "org-export-latex" "\
Export current buffer to a LaTeX file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-latex-low-levels'.  The default is to
convert them as description lists.  When HIDDEN is non-nil, don't
display the LaTeX buffer.  EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting LaTeX as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of egin{document}...nd{document},
without even the egin{document} and nd{document} commands.
when PUB-DIR is set, use this as the publishing directory.

\(fn ARG &optional HIDDEN EXT-PLIST TO-BUFFER BODY-ONLY PUB-DIR)" t nil)

;;;***

;;;### (autoloads (org-id-find org-id-goto org-id-get-with-outline-drilling
;;;;;;  org-id-get-with-outline-path-completion org-id-get org-id-copy
;;;;;;  org-id-get-create) "org-id" "lisp/org-id.el" (18488 31723))
;;; Generated autoloads from lisp/org-id.el

(autoload 'org-id-get-create "org-id" "\
Create an ID for the current entry and return it.
If the entry already has an ID, just return it.
With optional argument FORCE, force the creation of a new ID.

\(fn &optional FORCE)" t nil)

(autoload 'org-id-copy "org-id" "\
Copy the ID of the entry at point to the kill ring.
Create an ID if necessary.

\(fn)" t nil)

(autoload 'org-id-get "org-id" "\
Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, the function returns nil.
However, when CREATE is non nil, create an ID if none is present already.
PREFIX will be passed through to `org-id-new'.
In any case, the ID of the entry is returned.

\(fn &optional POM CREATE PREFIX)" nil nil)

(autoload 'org-id-get-with-outline-path-completion "org-id" "\
Use outline-path-completion to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define the eligible
headlines.  When omitted, all headlines in all agenda files are
eligible.
It returns the ID of the entry.  If necessary, the ID is created.

\(fn &optional TARGETS)" nil nil)

(autoload 'org-id-get-with-outline-drilling "org-id" "\
Use an outline-cycling interface to retrieve the ID of an entry.
This only finds entries in the current buffer, using `org-get-location'.
It returns the ID of the entry.  If necessary, the ID is created.

\(fn &optional TARGETS)" nil nil)

(autoload 'org-id-goto "org-id" "\
Switch to the buffer containing the entry with id ID.
Move the cursor to that entry in that buffer.

\(fn ID)" t nil)

(autoload 'org-id-find "org-id" "\
Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker.

\(fn ID &optional MARKERP)" nil nil)

;;;***

;;;### (autoloads (org-irc-store-link) "org-irc" "lisp/org-irc.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-irc.el

(autoload 'org-irc-store-link "org-irc" "\
Dispatch to the appropriate function to store a link to an IRC session.

\(fn)" nil nil)

;;;***

;;;### (autoloads (org-publish-current-project org-publish-current-file
;;;;;;  org-publish-all org-publish) "org-publish" "lisp/org-publish.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-publish.el

(autoload 'org-publish "org-publish" "\
Publish PROJECT.

\(fn PROJECT &optional FORCE)" t nil)

(autoload 'org-publish-all "org-publish" "\
Publish all projects.
With prefix argument, force publish all files.

\(fn &optional FORCE)" t nil)

(autoload 'org-publish-current-file "org-publish" "\
Publish the current file.
With prefix argument, force publish the file.

\(fn &optional FORCE)" t nil)

(autoload 'org-publish-current-project "org-publish" "\
Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads (org-remember-handler org-remember org-remember-apply-template
;;;;;;  org-remember-annotation org-remember-insinuate) "org-remember"
;;;;;;  "lisp/org-remember.el" (18488 31723))
;;; Generated autoloads from lisp/org-remember.el

(autoload 'org-remember-insinuate "org-remember" "\
Setup remember.el for use wiht Org-mode.

\(fn)" nil nil)

(autoload 'org-remember-annotation "org-remember" "\
Return a link to the current location as an annotation for remember.el.
If you are using Org-mode files as target for data storage with
remember.el, then the annotations should include a link compatible with the
conventions in Org-mode.  This function returns such a link.

\(fn)" nil nil)

(autoload 'org-remember-apply-template "org-remember" "\
Initialize *remember* buffer with template, invoke `org-mode'.
This function should be placed into `remember-mode-hook' and in fact requires
to be run from that hook to function properly.

\(fn &optional USE-CHAR SKIP-INTERACTIVE)" nil nil)

(autoload 'org-remember "org-remember" "\
Call `remember'.  If this is already a remember buffer, re-apply template.
If there is an active region, make sure remember uses it as initial content
of the remember buffer.

When called interactively with a `C-u' prefix argument GOTO, don't remember
anything, just go to the file/headline where the selected template usually
stores its notes.  With a double prefix arg `C-u C-u', go to the last
note stored by remember.

Lisp programs can set ORG-FORCE-REMEMBER-TEMPLATE-CHAR to a character
associated with a template in `org-remember-templates'.

\(fn &optional GOTO ORG-FORCE-REMEMBER-TEMPLATE-CHAR)" t nil)

(autoload 'org-remember-handler "org-remember" "\
Store stuff from remember.el into an org file.
First prompts for an org file.  If the user just presses return, the value
of `org-default-notes-file' is used.
Then the command offers the headings tree of the selected file in order to
file the text at a specific location.
You can either immediately press RET to get the note appended to the
file, or you can use vertical cursor motion and visibility cycling (TAB) to
find a better place.  Then press RET or <left> or <right> in insert the note.

Key      Cursor position   Note gets inserted
-----------------------------------------------------------------------------
RET      buffer-start      as level 1 heading at end of file
RET      on headline       as sublevel of the heading at cursor
RET      no heading        at cursor position, level taken from context.
			   Or use prefix arg to specify level manually.
<left>   on headline       as same level, before current heading
<right>  on headline       as same level, after current heading

So the fastest way to store the note is to press RET RET to append it to
the default file.  This way your current train of thought is not
interrupted, in accordance with the principles of remember.el.
You can also get the fast execution without prompting by using
C-u C-c C-c to exit the remember buffer.  See also the variable
`org-remember-store-without-prompt'.

Before being stored away, the function ensures that the text has a
headline, i.e. a first line that starts with a \"*\".  If not, a headline
is constructed from the current date and some additional data.

If the variable `org-adapt-indentation' is non-nil, the entire text is
also indented so that it starts in the same column as the headline
\(i.e. after the stars).

See also the variable `org-reverse-note-order'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (orgtbl-mode turn-on-orgtbl) "org-table" "lisp/org-table.el"
;;;;;;  (18488 31723))
;;; Generated autoloads from lisp/org-table.el

(autoload 'turn-on-orgtbl "org-table" "\
Unconditionally turn on `orgtbl-mode'.

\(fn)" nil nil)

(autoload 'orgtbl-mode "org-table" "\
The `org-mode' table editor as a minor mode for use in other modes.

\(fn &optional ARG)" t nil)

;;;***

(provide (quote org-install))
