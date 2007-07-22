" Vim syntax file
" Language:             For brainstorming things
" Maintainer:           Martin Stubenschrott
" Last Change:          2005 Feb 13

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Preprocessor
syn region issPreProc start="^\s*#" end="$"

" Section
syn region SquareBrackets     start="\[" end="\]" contains=Semester,Statement,CurlyBrackets

" Bracket expressions
syn region Statement	start="(" end=")" contains=Semester,SquareBrackets,CurlyBrackets

" folder constant
syn match  CurlyBrackets    "{[^{]*}" contains=Semester,SquareBrackets,Statement

" URL
syn match  MoreMsg       "http[s]\=:\/\/.*$"


" Capitalized words
syn match Identifier 	"[A-Z][a-zA-Z0-9_]\+"

" Questions and conclusions
syn match Special 	"^> .*"
syn match Special 	"->"

" string
syn region String    start=+"+  end=+"+ contains=SquareBrackets

" comments
syn match Define	"#.*"
syn match Comment	"##.*"
syn match Comment	"/\*.*\*/"

syn match Error 	"ERROR"
syn match Number 	"OK"
" lines which are done already -> dark green
syn match MoreMsg	".*DONE.*"
syn match MoreMsg	"^---.*"
" SS2004 WS2005, etc.
syn match Semester	"\(W\|S\)S...."


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_iss_syntax_inits")
  if version < 508
    let did_iss_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

   " The default methods for highlighting.  Can be overridden later
   HiLink issHeader     Special
   HiLink issComment    Comment
   HiLink CurlyBrackets      Type
   HiLink issName       Type
   HiLink SquareBrackets     Special
   HiLink issValue      String
   HiLink issURL        Include
   HiLink issPreProc    PreProc 
   HiLink Semester	Special

  delcommand HiLink
endif

let b:current_syntax = "brainstorm"

" vim:ts=8
