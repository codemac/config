Commentary:

* Using org-readme
Org readme is used to:

- Create/Update a "History" section in the Readme.org based on the changelog
  section of the Emacs Log.
- Create/Update a "Library Information" Section Based on the Emacs lisp header.
- Create/Update a "Possible Dependencies" Section Based on the Emacs
  lisp header.
- Create/Update a "Functions" Section based on the functions defined
  in the single lisp library.
- Create/Update a "Variables" Section based on the variables defined
  in the single lisp library.

All other sections of the Readme.org are then put into the
"Commentary" section of the readme.org.

In addition this library defines `org-readme-sync',  a convenience function that:

- Asks for a commentary about the library change.
  - To exit/save press `C-c C-c'
- Asks if this is a minor revision
  - If it is a minor revision, bumps the revision up so the new
    library will be posted to marmalade-repo.org
  - The package will attempt to add the readme to the info
    documentation system within emacs.
- Syncs the Readme.org with the lisp file as described above.
- Updates emacswiki with the library description and the library
  itself (requires yaoddmuse).
- Updates Marmalade-repo if the library version is different than the
  version in the server (requires http-post-simple).
- Updates the git repository with the differences that you posted.
- If you are using github, this library creates a melpa recipie.
- If you are using github, this library creates a el-get recipie.

When `org-readme-sync' is called in a `Readme.org' file that is not a
single lisp file, the function exports the readme in EmacsWiki format
and posts it to the EmacsWiki.
** EmacsWiki Page Names
EmacsWiki Page names are generated from the file.  `org-readme.el'
would generate a page of OrgReadme.

** Why each required library is needed
There are a few required libraries.  This is a list of the require
libraries and why they are needed.

|------------------+--------------------------------------|
| Library          | Why it is needed                     |
|------------------+--------------------------------------|
| yaoddmuse        | Publish to emacswiki                 |
| http-post-simple | Publish to marmalade-repo.org        |
| header2          | To create header and changelog       |
| lib-requires     | To generate the library dependencies |
|------------------+--------------------------------------|
