;;; bbdb-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bbdb-initialize bbdb-version bbdb-mode) "bbdb"
;;;;;;  "bbdb.el" (20703 15654))
;;; Generated autoloads from bbdb.el

(defsubst bbdb-records nil "\
Return a list of all BBDB records; read in and parse the db if necessary.
This function also notices if the corresponding file on disk has been modified." (with-current-buffer (bbdb-buffer) bbdb-records))

(autoload 'bbdb-mode "bbdb" "\
Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<bbdb-mode-map>
\\[bbdb-add-mail-alias]	 Add new mail alias to visible records or remove it.
\\[bbdb-edit-field]	 Edit the field on the current line.
\\[bbdb-delete-field-or-record]	 Delete the field on the current line.  If the current line is the
	 first line of a record, then delete the entire record.
\\[bbdb-insert-field]	 Insert a new field into the current record.  Note that this
	 will let you add new fields of your own as well.
\\[bbdb-transpose-fields]	 Swap the field on the current line with the previous field.
\\[bbdb-dial]	 Dial the current phone field.
\\[bbdb-next-record], \\[bbdb-prev-record]	 Move to the next or the previous displayed record, respectively.
\\[bbdb-create]	 Create a new record.
\\[bbdb-toggle-records-layout]	 Toggle whether the current record is displayed in a one-line
	 listing, or a full multi-line listing.
\\[bbdb-do-all-records]\\[bbdb-toggle-records-layout]	 Do that for all displayed records.
\\[bbdb-merge-records]	 Merge the contents of the current record with some other, and then
	 delete the current record.
\\[bbdb-omit-record]	 Remove the current record from the display without deleting it from
	 the database.  This is often a useful thing to do before using one
	 of the `*' commands.
\\[bbdb]	 Search for records in the database (on all fields).
\\[bbdb-search-mail]	 Search for records by mail address.
\\[bbdb-search-organization]	 Search for records by organization.
\\[bbdb-search-xfields]	 Search for records by xfields.
\\[bbdb-search-name]	 Search for records by name.
\\[bbdb-search-changed]	 Display records that have changed since the database was saved.
\\[bbdb-mail]	 Compose mail to the person represented by the current record.
\\[bbdb-do-all-records]\\[bbdb-mail]	 Compose mail to everyone whose record is displayed.
\\[bbdb-save]	 Save the BBDB file to disk.
\\[bbdb-print]	 Create a TeX file containing a pretty-printed version of all the
	 records in the database.
\\[bbdb-do-all-records]\\[bbdb-print]	 Do that for the displayed records only.
\\[other-window]	 Move to another window.
\\[bbdb-info]	 Read the Info documentation for BBDB.
\\[bbdb-help]	 Display a one line command summary in the echo area.
\\[bbdb-browse-url]	 Visit Web sites listed in the `url' field(s) of the current record.

For address completion using the names and mail addresses in the database:
	 in Sendmail mode, type \\<mail-mode-map>\\[bbdb-complete-mail].
	 in Message mode, type \\<message-mode-map>\\[bbdb-complete-mail].

Important variables:
	 `bbdb-auto-revert'
	 `bbdb-canonicalize-redundant-mails'
	 `bbdb-case-fold-search'
	 `bbdb-completion-list'
	 `bbdb-default-area-code'
	 `bbdb-default-domain'
	 `bbdb-layout'
	 `bbdb-file'
	 `bbdb-phone-style'
	 `bbdb-check-auto-save-file'
	 `bbdb-pop-up-layout'
	 `bbdb-pop-up-window-size'
	 `bbdb-add-name'
	 `bbdb-add-aka'
	 `bbdb-add-mails'
	 `bbdb-new-mails-primary'
	 `bbdb-read-only'
	 `bbdb-mua-pop-up'
	 `bbdb-user-mail-address-re'

There are numerous hooks.  M-x apropos ^bbdb.*hook RET

\\{bbdb-mode-map}

\(fn)" t nil)

(autoload 'bbdb-version "bbdb" "\
Return string describing the version of BBDB.
With prefix ARG, insert string at point.

\(fn &optional ARG)" t nil)

(autoload 'bbdb-initialize "bbdb" "\
Initialize BBDB for MUAS.
List MUAS may include the following symbols
to initialize the respective mail/news readers and composers:
  gnus       Gnus mail/news reader.
  mh-e       MH-E mail reader.
  rmail      Rmail mail reader.
  vm         VM mail reader.
  mail       Mail (M-x mail).
  message    Message mode.
See also `bbdb-mua-auto-update-init'.  The latter is a separate function
as this allows one to initialize the auto update feature for some MUAs only,
for example only for outgoing messages.

\(fn &rest MUAS)" nil nil)

;;;***

;;;### (autoloads (bbdb-anniv-diary-entries) "bbdb-anniv" "bbdb-anniv.el"
;;;;;;  (20703 15654))
;;; Generated autoloads from bbdb-anniv.el

(autoload 'bbdb-anniv-diary-entries "bbdb-anniv" "\
Add anniversaries from BBDB records to `diary-list-entries'.
This obeys `calendar-date-style' via `diary-date-forms'.
To enable this feature, put the following into your .emacs:

 (add-hook 'diary-list-entries-hook 'bbdb-anniv-diary-entries)

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-help bbdb-info bbdb-copy-records-as-kill
;;;;;;  bbdb-grab-url bbdb-browse-url bbdb-dial bbdb-add-mail-alias
;;;;;;  bbdb-mail-aliases bbdb-complete-mail bbdb-completing-read-mails
;;;;;;  bbdb-completion-predicate bbdb-mail bbdb-dwim-mail bbdb-sort-xfields
;;;;;;  bbdb-sort-phones bbdb-sort-addresses bbdb-merge-records bbdb-omit-record
;;;;;;  bbdb-display-records-with-layout bbdb-display-records-completely
;;;;;;  bbdb-toggle-records-layout bbdb-display-current-record bbdb-display-all-records
;;;;;;  bbdb-delete-records bbdb-delete-field-or-record bbdb-transpose-fields
;;;;;;  bbdb-edit-field bbdb-insert-field bbdb-create bbdb-creation-no-change
;;;;;;  bbdb-creation-newer bbdb-creation-older bbdb-timestamp-newer
;;;;;;  bbdb-timestamp-older bbdb-search-changed bbdb-search-xfields
;;;;;;  bbdb-search-phone bbdb-search-mail bbdb-search-address bbdb-search-organization
;;;;;;  bbdb-search-name bbdb bbdb-search-invert bbdb-append-display
;;;;;;  bbdb-append-display-p bbdb-do-records bbdb-do-all-records)
;;;;;;  "bbdb-com" "bbdb-com.el" (20703 15654))
;;; Generated autoloads from bbdb-com.el

(autoload 'bbdb-do-all-records "bbdb-com" "\
Command prefix for operating on all records currently displayed.
This only works for certain commands.

\(fn)" t nil)

(autoload 'bbdb-do-records "bbdb-com" "\
Return list of records to operate on.
Normally this list includes only the current record.
It includes all currently displayed records if the command prefix \\<bbdb-mode-map>\\[bbdb-do-all-records] is used.
If FULL is non-nil, the list of records includes display information.

\(fn &optional FULL)" nil nil)

(autoload 'bbdb-append-display-p "bbdb-com" "\
Return variable `bbdb-append-display' and reset.

\(fn)" nil nil)

(autoload 'bbdb-append-display "bbdb-com" "\
Toggle appending next searched records in the *BBDB* buffer.
With prefix ARG \\[universal-argument] always append.
With ARG a positive number append for that many times.
With ARG a negative number do not append.

\(fn &optional ARG)" t nil)

(autoload 'bbdb-search-invert "bbdb-com" "\
Toggle inversion of the next search command.
With prefix ARG a positive number, invert next search.
With prefix ARG a negative number, do not invert next search.

\(fn &optional ARG)" t nil)

(autoload 'bbdb "bbdb-com" "\
Display all records in the BBDB matching REGEXP
in either the name(s), organization, address, phone, mail, or xfields.

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-name "bbdb-com" "\
Display all records in the BBDB matching REGEXP in the name
\(or ``alternate'' names).

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-organization "bbdb-com" "\
Display all records in the BBDB matching REGEXP in the organization field.

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-address "bbdb-com" "\
Display all records in the BBDB matching REGEXP in the address fields.

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-mail "bbdb-com" "\
Display all records in the BBDB matching REGEXP in the mail address.

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-phone "bbdb-com" "\
Display all records in the BBDB matching REGEXP in the phones field.

\(fn REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-xfields "bbdb-com" "\
Display all BBDB records for which xfield FIELD matches REGEXP.

\(fn FIELD REGEXP &optional LAYOUT)" t nil)

(autoload 'bbdb-search-changed "bbdb-com" "\
Display all records in the bbdb database which have changed since
the database was last saved.

\(fn &optional LAYOUT)" t nil)

(autoload 'bbdb-timestamp-older "bbdb-com" "\
Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE &optional LAYOUT)" t nil)

(autoload 'bbdb-timestamp-newer "bbdb-com" "\
Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE &optional LAYOUT)" t nil)

(autoload 'bbdb-creation-older "bbdb-com" "\
Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE &optional LAYOUT)" t nil)

(autoload 'bbdb-creation-newer "bbdb-com" "\
Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format.

\(fn DATE &optional LAYOUT)" t nil)

(autoload 'bbdb-creation-no-change "bbdb-com" "\
Display records that have the same timestamp and creation-date.

\(fn &optional LAYOUT)" t nil)

(autoload 'bbdb-create "bbdb-com" "\
Add a new RECORD to the bbdb database ; prompts for all relevant info
using the echo area, inserts the new record in BBDB, sorted alphabetically,
and offers to save the BBDB file.  DO NOT call this from a program.
Call `bbdb-create-internal' instead.

\(fn RECORD)" t nil)

(autoload 'bbdb-insert-field "bbdb-com" "\
Add a new field to the current record; the FIELD type and VALUE
are prompted for if not supplied.

If you are inserting a new phone-number field, the phone number style
is controlled via `bbdb-phone-style'.  A prefix C-u inverts the style,

If you are inserting a new mail address, you can have BBDB append a
default domain to any mail address that does not contain one.  Set
`bbdb-default-domain' to a string such as \"mycompany.com\" (or,
depending on your environment, (getenv \"DOMAINNAME\")), and
\"@mycompany.com\" will be appended to an address that is entered as
just a username.  A prefix arg C-u (or a `bbdb-default-domain'
value of \"\", the default) means do not alter the address.

\(fn RECORD FIELD VALUE)" t nil)

(autoload 'bbdb-edit-field "bbdb-com" "\
Edit the contents of FIELD of RECORD.
If point is in the middle of a multi-line field (e.g., address),
then the entire field is edited, not just the current line.
For editing phone numbers or addresses, VALUE must be the phone number
or address that gets edited. An error is thrown when attempting to edit
a phone number or address with VALUE being nil.

\(fn RECORD FIELD &optional VALUE FLAG)" t nil)

(autoload 'bbdb-transpose-fields "bbdb-com" "\
Transpose previous and current field of a BBDB record.
With numeric prefix ARG, take previous field and move it past ARG fields.
With region active or ARG 0, transpose field point is in and field mark is in.

Both fields must be in the same record, and must be of the same basic type
\(that is, you can use this command to change the order in which phone numbers
or email addresses are listed, but you cannot use it to make an address appear
before a phone number; the order of field types is fixed).

If the current field is the name field, transpose first and last name,
irrespective of the value of ARG.

\(fn ARG)" t nil)

(autoload 'bbdb-delete-field-or-record "bbdb-com" "\
For RECORDS delete FIELD.
If FIELD is the `name' field, delete RECORDS from datanbase.
Only then RECORDS may be more than one record.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records',
and FIELD is the field point is on.
If prefix NOPROMPT is non-nil, do not confirm deletion.

\(fn RECORDS FIELD &optional NOPROMPT)" t nil)

(autoload 'bbdb-delete-records "bbdb-com" "\
Delete RECORDS.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If prefix NOPROMPT is non-nil, do not confirm deletion.

\(fn RECORDS &optional NOPROMPT)" t nil)

(autoload 'bbdb-display-all-records "bbdb-com" "\
Show all records.
If invoked in a *BBDB* buffer point stays on the currently visible record.
Inverse of `bbdb-display-current-record'.

\(fn &optional LAYOUT)" t nil)

(autoload 'bbdb-display-current-record "bbdb-com" "\
Narrow to current record.  Inverse of `bbdb-display-all-records'.

\(fn &optional LAYOUT)" t nil)

(autoload 'bbdb-toggle-records-layout "bbdb-com" "\
Toggle layout of RECORDS (elided or expanded).
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
With prefix ARG 0, RECORDS are displayed elided.
With any other non-nil ARG, RECORDS are displayed expanded.

\(fn RECORDS &optional ARG)" t nil)

(autoload 'bbdb-display-records-completely "bbdb-com" "\
Display RECORDS using layout `full-multi-line' (i.e., display all fields).
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.

\(fn RECORDS)" t nil)

(autoload 'bbdb-display-records-with-layout "bbdb-com" "\
Display RECORDS using LAYOUT.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.

\(fn RECORDS LAYOUT)" t nil)

(autoload 'bbdb-omit-record "bbdb-com" "\
Remove current record from the display without deleting it from BBDB.
With prefix N, omit the next N records.  If negative, omit backwards.

\(fn N)" t nil)

(autoload 'bbdb-merge-records "bbdb-com" "\
Merge OLD-RECORD into NEW-RECORD.
This copies all the data in OLD-RECORD into NEW-RECORD.  Then OLD-RECORD
is deleted.  If both records have names and/or organizations, ask which to use.
Phone numbers, addresses, and mail addresses are simply concatenated.

Interactively, OLD-RECORD is the current record.  NEW-RECORD is prompted for.
With prefix arg NEW-RECORD defaults to the first record with the same name.

\(fn OLD-RECORD NEW-RECORD)" t nil)

(autoload 'bbdb-sort-addresses "bbdb-com" "\
Sort the addresses in RECORDS according to the label.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook').

\(fn RECORDS &optional UPDATE)" t nil)

(autoload 'bbdb-sort-phones "bbdb-com" "\
Sort the phones in RECORDS according to the label.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook').

\(fn RECORDS &optional UPDATE)" t nil)

(autoload 'bbdb-sort-xfields "bbdb-com" "\
Sort the xfields in RECORDS according to `bbdb-xfields-sort-order'.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity (for example, when used
in `bbdb-change-hook').

\(fn RECORDS &optional UPDATE)" t nil)

(autoload 'bbdb-dwim-mail "bbdb-com" "\
Return a string to use as the mail address of RECORD.
The mail address is formatted like \"Firstname Lastname <address>\".
If both the first name and last name are constituents of the address
as in John.Doe@Some.Host, and `bbdb-mail-avoid-redundancy' is non-nil,
then the address is used as is.
If `bbdb-mail-avoid-redundancy' is 'mail-only the name is never included.
MAIL may be a mail address to be used for RECORD.
If MAIL is an integer, use the MAILth mail address of RECORD.
If Mail is nil use the first mail address of RECORD.

\(fn RECORD &optional MAIL)" nil nil)

(autoload 'bbdb-mail "bbdb-com" "\
Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose.

\(fn RECORDS &optional SUBJECT N VERBOSE)" t nil)

(autoload 'bbdb-completion-predicate "bbdb-com" "\
For use as the third argument to `completing-read'.
Obey `bbdb-completion-list'.

\(fn SYMBOL)" nil nil)

(autoload 'bbdb-completing-read-mails "bbdb-com" "\
Like `read-string', but allows `bbdb-complete-mail' style completion.

\(fn PROMPT &optional DEFAULT)" nil nil)

(autoload 'bbdb-complete-mail "bbdb-com" "\
In a mail buffer, complete the user name or mail before point.
Completion happens up to the preceeding newline, colon, or comma,
or the value of START-POS).
Return non-nil if there is a valid completion, else return nil.

Completion behaviour can be controlled with `bbdb-completion-list'.
If what has been typed is unique, insert an entry of the form
\"User Name <mail>\" (although see `bbdb-mail-allow-redundancy').
If it is a valid completion but not unique, a list of completions is displayed.
If the completion is done and `bbdb-complete-mail-allow-cycling' is
t then cycle through the mails for the matching record.
With prefix CYCLE-COMPLETION-BUFFER non-nil, display a list of all mails
available for cycling.

Set variable `bbdb-complete-mail' non-nil for enabling this feature
as part of the MUA insinuation.

\(fn &optional START-POS CYCLE-COMPLETION-BUFFER)" t nil)

(define-obsolete-function-alias 'bbdb-complete-name 'bbdb-complete-mail)

(autoload 'bbdb-mail-aliases "bbdb-com" "\
Define mail aliases for the records in the database.
Define a mail alias for every record that has a `mail-alias' field
which is the contents of that field.
If there are multiple comma-separated words in the `mail-alias' field,
then all of those words will be defined as aliases for that person.

If multiple records in the database have the same mail alias,
then that alias expands to a comma-separated list of the mail addresses
of all of these people.
Add this command to `mail-setup-hook'.

Mail aliases are (re)built only if `bbdb-mail-aliases-need-rebuilt' is non-nil
because the database was newly loaded or it has been edited.
Rebuilding the aliases is enforced if prefix FORCE-REBUILT is t.

\(fn &optional FORCE-REBUILT NOISY)" t nil)

(autoload 'bbdb-add-mail-alias "bbdb-com" "\
Add ALIAS to RECORD.
If pefix DELETE is non-nil, remove ALIAS from RECORD.

\(fn RECORD &optional ALIAS DELETE)" t nil)

(autoload 'bbdb-dial "bbdb-com" "\
Dial the number at point.
If the point is at the beginning of a record, dial the first phone number.
Use rules from `bbdb-dial-local-prefix-alist' unless prefix FORCE-AREA-CODE
is non-nil.  Do not dial the extension.

\(fn PHONE FORCE-AREA-CODE)" t nil)

(autoload 'bbdb-browse-url "bbdb-com" "\
Brwose URLs stored in the `url' field of RECORDS.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL.

\(fn RECORDS &optional WHICH)" t nil)

(autoload 'bbdb-grab-url "bbdb-com" "\
Grab URL and store it in RECORD.

\(fn RECORD URL)" t nil)

(autoload 'bbdb-copy-records-as-kill "bbdb-com" "\
Copy displayed RECORDS to kill ring.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.

\(fn RECORDS)" t nil)

(autoload 'bbdb-info "bbdb-com" "\


\(fn)" t nil)

(autoload 'bbdb-help "bbdb-com" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-gnus bbdb/gnus-score) "bbdb-gnus"
;;;;;;  "bbdb-gnus.el" (20703 15654))
;;; Generated autoloads from bbdb-gnus.el

(autoload 'bbdb/gnus-score "bbdb-gnus" "\
This returns a score alist for Gnus.  A score pair will be made for
every member of the mail field in records which also have a gnus-score
field.  This allows the BBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile.

\(fn GROUP)" nil nil)

(autoload 'bbdb-insinuate-gnus "bbdb-gnus" "\
Hook BBDB into Gnus.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-ispell-export) "bbdb-ispell" "bbdb-ispell.el"
;;;;;;  (20703 15654))
;;; Generated autoloads from bbdb-ispell.el

(autoload 'bbdb-ispell-export "bbdb-ispell" "\
Export BBDB records to ispell personal dictionaries.

\(fn)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-mail bbdb-insinuate-message) "bbdb-message"
;;;;;;  "bbdb-message.el" (20703 15654))
;;; Generated autoloads from bbdb-message.el

(autoload 'bbdb-insinuate-message "bbdb-message" "\
Hook BBDB into Message Mode.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

(autoload 'bbdb-insinuate-mail "bbdb-message" "\
Hook BBDB into Mail Mode.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-mh) "bbdb-mhe" "bbdb-mhe.el" (20703
;;;;;;  15654))
;;; Generated autoloads from bbdb-mhe.el

(autoload 'bbdb-insinuate-mh "bbdb-mhe" "\
Call this function to hook BBDB into MH-E.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-undocumented-variables bbdb-migrate) "bbdb-migrate"
;;;;;;  "bbdb-migrate.el" (20703 15654))
;;; Generated autoloads from bbdb-migrate.el

(autoload 'bbdb-migrate "bbdb-migrate" "\
Migrate the BBDB from the version on disk to the current version
\(in `bbdb-file-format').

\(fn RECORDS OLD-FORMAT)" nil nil)

(autoload 'bbdb-undocumented-variables "bbdb-migrate" "\
Return list of undocumented variables in NAME-SPACE.
NAME-SPACE defaults to \"bbdb-\".  Use a prefix arg to specify NAME-SPACE
interactively.  If MESSAGE is non-nil (as in interactive calls) display
the list in the message area.

This command may come handy to identify BBDB variables in your init file
that are not used anymore by the current version of BBDB.  Yet this fails
for outdated BBDB variables that are set via your personal `custom-file'.

\(fn &optional NAME-SPACE MESSAGE)" t nil)

;;;***

;;;### (autoloads (bbdb-canonicalize-mail-1 bbdb-auto-notes bbdb-mua-auto-update-init
;;;;;;  bbdb-mua-auto-update bbdb-mua-edit-field-recipients bbdb-mua-edit-field-sender
;;;;;;  bbdb-mua-edit-field bbdb-mua-annotate-recipients bbdb-mua-annotate-sender
;;;;;;  bbdb-display-all-recipients bbdb-mua-display-recipients bbdb-mua-display-sender
;;;;;;  bbdb-mua-display-records bbdb-update-records bbdb-select-message
;;;;;;  bbdb-ignore-message bbdb-accept-message bbdb-message-header)
;;;;;;  "bbdb-mua" "bbdb-mua.el" (20703 15654))
;;; Generated autoloads from bbdb-mua.el

(autoload 'bbdb-message-header "bbdb-mua" "\
For the current message return the value of HEADER.
MIME encoded headers are decoded.  Return nil if HEADER does not exist.

\(fn HEADER)" nil nil)

(autoload 'bbdb-accept-message "bbdb-mua" "\
For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages matching
`bbdb-accept-message-alist'.  If INVERT is non-nil, accept messages
not matching `bbdb-ignore-message-alist'.

\(fn &optional INVERT)" nil nil)

(autoload 'bbdb-ignore-message "bbdb-mua" "\
For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages not matching
`bbdb-ignore-message-alist'.  If INVERT is non-nil, accept messages
matching `bbdb-accept-message-alist'.

\(fn &optional INVERT)" nil nil)

(autoload 'bbdb-select-message "bbdb-mua" "\
For use with variable `bbdb-mua-update-interactive-p' and friends.
Return the value of variable `bbdb-update-records-p' for messages both matching
`bbdb-accept-message-alist' and not matching `bbdb-ignore-message-alist'.

\(fn)" nil nil)

(autoload 'bbdb-update-records "bbdb-mua" "\
Return the list of BBDB records matching ADDRESS-LIST.
ADDRESS-LIST is a list of mail addresses.  (It can be extracted from
a mail message using `bbdb-get-address-components'.)
UPDATE-P may take the following values:
 search       Search for existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values.
 nil          Take the MUA-specific variable `bbdb/MUA-update-records-p'
                which may take one of the above values.
                If this still gives nil, `bbdb-update-records' returns nil.

Usually this function is called by the wrapper `bbdb-mua-update-records'.

\(fn ADDRESS-LIST &optional UPDATE-P)" nil nil)

(autoload 'bbdb-mua-display-records "bbdb-mua" "\
Display the BBDB record(s) for the addresses in this message.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding BBDB records are displayed.
UPDATE-P determines whether only existing BBDB records are displayed
or whether also new records are created for these mail addresses.

HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'.

\(fn &optional HEADER-CLASS UPDATE-P)" t nil)

(autoload 'bbdb-mua-display-sender "bbdb-mua" "\
Display the BBDB record(s) for the sender of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'.

\(fn &optional UPDATE-P)" t nil)

(autoload 'bbdb-mua-display-recipients "bbdb-mua" "\
Display the BBDB record(s) for the recipients of this message.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, see function `bbdb-mua-update-interactive-p'.

\(fn &optional UPDATE-P)" t nil)

(autoload 'bbdb-display-all-recipients "bbdb-mua" "\
Display BBDB records for all addresses of the message in this buffer.
If the records do not exist, they are generated.

\(fn &optional HEADER-CLASS)" t nil)

(autoload 'bbdb-mua-annotate-sender "bbdb-mua" "\
Add STRING to notes field of the BBDB record(s) of message sender(s).
If prefix REPLACE is non-nil, replace the existing notes entry (if any).
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.

\(fn STRING &optional REPLACE UPDATE-P)" t nil)

(autoload 'bbdb-mua-annotate-recipients "bbdb-mua" "\
Add STRING to notes field of the BBDB records of message recipients.
If prefix REPLACE is non-nil, replace the existing notes entry (if any).
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.

\(fn STRING &optional REPLACE UPDATE-P)" t nil)

(autoload 'bbdb-mua-edit-field "bbdb-mua" "\
Edit FIELD of the BBDB record(s) of message sender(s) or recipients.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.
HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.

\(fn FIELD &optional UPDATE-P HEADER-CLASS)" t nil)

(autoload 'bbdb-mua-edit-field-sender "bbdb-mua" "\
Edit FIELD of record corresponding to sender of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.

\(fn &optional FIELD UPDATE-P)" t nil)

(autoload 'bbdb-mua-edit-field-recipients "bbdb-mua" "\
Edit FIELD of record corresponding to recipient of this message.
FIELD defaults to 'notes.  With prefix arg, ask for FIELD.
UPDATE-P may take the same values as `bbdb-update-records-p'.
For interactive calls, use car of `bbdb-mua-update-interactive-p'.

\(fn &optional FIELD UPDATE-P)" t nil)

(autoload 'bbdb-mua-auto-update "bbdb-mua" "\
Update BBDB automatically based on incoming and outgoing messages.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding BBDB records are updated.
UPDATE-P determines whether only existing BBDB records are taken
or whether also new records are created for these mail addresses.
Return matching records.

HEADER-CLASS is defined in `bbdb-message-headers'.  If it is nil,
use all classes in `bbdb-message-headers'.
UPDATE-P may take the same values as `bbdb-mua-auto-update-p'.
If UPDATE-P is nil, use `bbdb-mua-auto-update-p' (which see).

If `bbdb-mua-pop-up' is non-nil, the *BBDB* buffer is displayed
along with the MUA window(s), showing the matching records.
If this is nil, BBDB is updated silently.

This function is intended for noninteractive use via appropriate MUA hooks.
Call `bbdb-mua-auto-update-init' in your init file to put this function
into the respective MUA hooks.
See `bbdb-mua-display-records' and friends for interactive commands.

\(fn &optional HEADER-CLASS UPDATE-P)" nil nil)

(autoload 'bbdb-mua-auto-update-init "bbdb-mua" "\
For MUAS add `bbdb-mua-auto-update' to their presentation hook.
If a MUA is not an element of MUAS, `bbdb-mua-auto-update' is removed
from the respective presentation hook.

Call this function in your init file to use the auto update feature with MUAS.
This function is separate from the general function `bbdb-initialize'
as this allows one to initialize the auto update feature for some MUAs only,
for example only for outgoing messages.

See `bbdb-mua-auto-update' for details about the auto update feature.

\(fn &rest MUAS)" nil nil)

(autoload 'bbdb-auto-notes "bbdb-mua" "\
Automatically annotate RECORD based on the headers of the current message.
See the variables `bbdb-auto-notes-rules', `bbdb-auto-notes-ignore-messages'
and `bbdb-auto-notes-ignore-headers'.
For use as an element of `bbdb-notice-mail-hook'.

\(fn RECORD)" nil nil)

(autoload 'bbdb-canonicalize-mail-1 "bbdb-mua" "\
Example of `bbdb-canonicalize-mail-function'.

\(fn ADDRESS)" nil nil)

;;;***

;;;### (autoloads (bbdb-print) "bbdb-print" "bbdb-print.el" (20703
;;;;;;  15654))
;;; Generated autoloads from bbdb-print.el

(autoload 'bbdb-print "bbdb-print" "\
Make a TeX FILE for printing RECORDS.
Interactively, use BBDB prefix \\<bbdb-mode-map>\\[bbdb-do-all-records], see `bbdb-do-all-records'.
With prefix BRIEF non-nil, make a brief (one line per record) printout.
There are various variables for customizing the content and format
of the printout, notably the variables `bbdb-print-alist' and
`bbdb-print-require'.

\(fn RECORDS FILE BRIEF)" t nil)

;;;***

;;;### (autoloads (bbdb-insinuate-rmail) "bbdb-rmail" "bbdb-rmail.el"
;;;;;;  (20703 15654))
;;; Generated autoloads from bbdb-rmail.el

(autoload 'bbdb-insinuate-rmail "bbdb-rmail" "\
Hook BBDB into RMAIL.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (bbdb-insinuate-vm bbdb/vm-virtual-folder bbdb/vm-auto-folder
;;;;;;  bbdb/vm-virtual-real-folders bbdb/vm-virtual-folder-field
;;;;;;  bbdb/vm-auto-folder-field bbdb/vm-auto-folder-headers) "bbdb-vm"
;;;;;;  "bbdb-vm.el" (20703 15654))
;;; Generated autoloads from bbdb-vm.el

(defvar bbdb/vm-auto-folder-headers '("From:" "To:" "CC:") "\
The headers used by `bbdb/vm-auto-folder'.
The order in this list is the order how matching will be performed.")

(custom-autoload 'bbdb/vm-auto-folder-headers "bbdb-vm" t)

(defvar bbdb/vm-auto-folder-field 'vm-folder "\
The field which `bbdb/vm-auto-folder' searches for.")

(custom-autoload 'bbdb/vm-auto-folder-field "bbdb-vm" t)

(defvar bbdb/vm-virtual-folder-field 'vm-virtual "\
The field which `bbdb/vm-virtual-folder' searches for.")

(custom-autoload 'bbdb/vm-virtual-folder-field "bbdb-vm" t)

(defvar bbdb/vm-virtual-real-folders nil "\
Real folders used for defining virtual folders.
If nil use `vm-primary-inbox'.")

(custom-autoload 'bbdb/vm-virtual-real-folders "bbdb-vm" t)

(autoload 'bbdb/vm-auto-folder "bbdb-vm" "\
Add entries to `vm-auto-folder-alist' for the records in BBDB.
For each record that has a `vm-folder' attribute, add an element
\(MAIL-REGEXP . FOLDER-NAME) to `vm-auto-folder-alist'.
The element gets added to the sublists of `vm-auto-folder-alist'
specified in `bbdb/vm-auto-folder-headers'.
MAIL-REGEXP matches the mail addresses of the BBDB record.
The value of the `vm-folder' attribute becomes FOLDER-NAME.
The `vm-folder' attribute is defined via `bbdb/vm-auto-folder-field'.

Add this function to `bbdb-before-save-hook' and your .vm.

\(fn)" t nil)

(autoload 'bbdb/vm-virtual-folder "bbdb-vm" "\
Create `vm-virtual-folder-alist' according to the records in BBDB.
For each record that has a `vm-virtual' attribute, add or modify the
corresponding VIRTUAL-FOLDER-NAME element of `vm-virtual-folder-alist'.

  (VIRTUAL-FOLDER-NAME ((FOLDER-NAME ...)
                        (author-or-recipient MAIL-REGEXP)))

VIRTUAL-FOLDER-NAME is the first element of the `vm-virtual' attribute.
FOLDER-NAME ... are either the remaining attributes of vm-virtual,
or `bbdb/vm-virtual-real-folders' or `vm-primary-inbox'.
MAIL-REGEXP matches the mail addresses of the BBDB record.
The `vm-virtual' attribute is defined via `bbdb/vm-virtual-folder-field'.

Add this function to `bbdb-before-save-hook' and your .vm.

\(fn)" t nil)

(autoload 'bbdb-insinuate-vm "bbdb-vm" "\
Hook BBDB into VM.
Do not call this in your init file.  Use `bbdb-initialize'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("bbdb-pkg.el") (20703 15654 206264))

;;;***

(provide 'bbdb-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bbdb-autoloads.el ends here
