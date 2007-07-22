" {{{ File header information
"	vim:ff=unix ts=4 ss=4
"	vim60:fdm=marker
" \file		FeralToggleCommentify.vim
"
" \brief	Adds, removes or toggles comment characters on col1 at the touch
"			of a key. Ranges are supported as are custom text to
"			insert/remove/toggle.
" Maintainer: (of this version anyway)
" \author	Robert KellyIV <Sreny@SverGbc.Pbz> (Rot13ed)
" Based On: {{{
" \note		Based On Vincent Nijs's ToggleCommentify.vim v1.2 Last Change:
"			Sunday, June 9th, 2001
" \author	Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" \note		Some comment definitions extracted from EnhancedCommentify.vim by
"			Meikel Brandmeyer
" \author	Meikel Brandmeyer <Brandels_Mikesh@web.de>
" }}}
" Contrabutions From: {{{
"	Bernhard Wagner		(12-Nov-2002 xml changes)
" }}}
"
" \date		Wed, 13 Nov 2002 05:28 Pacific Standard Time
" \version	$Id: feraltogglecommentify.vim,v 1.7 2002/11/13 13:53:50 root Exp $
" Version:	1.53
" History: {{{
"	[Feral:317/02@05:27] 1.53
"		Addition:	Incorperated Bernhard Wagner's 12-Nov-2002 v1.52 xml
"			changes
"		Improvment:	Will make default mappings to <M-c> only if no map to
"			<Plug>FtcTC and <M-c> is unused.
"	[Feral:309/02@18:44] 1.52
"		Merged DLAC with this; DLAC = duplicate line and comment, simple
"			mappings that are handy to use to save and comment a line before
"			you mangle it. However this mangles mark z
"		Bugfix:	mismatched <Plug> names; This is <Plug>FtcTc
"	[Feral:308/02@02:11] 1.51
"		Hacked in eol comments and added html's <!-- --> comments.
"	[Feral:300/02@07:03] 1.5
"		Rewrite, condenced toggle, comment and uncomment into one function,
"			much less duplicate code this way, hehe.
"		Now saves the cursor position (I believe) properly.
"		TODO could be better by handeling ranges internal.
"	[Feral:249/02@00:41] v1.4:
"	Bugfix, a comment char of + failed.
"	Modification: commands always place cursor on col 1 this is for
"	consistancy... comment left on col1, uncomment on first word.
"		I would prefer the cursor's column be unchanged however a range
"		command starts with cursor on col1, thus the cursor col would have to
"		be saved in a map or something that calls the commands.
"
"	Sat, 20  Jul  2002 02:26:29 Pacific Daylight Time:	v1.3
"	Massive changes. Proper handling of ranges (not that it didn't work before
"		mind you)
"	Made ToggleCommentify, UnCommentify and Commentify commands (:TC, :UC and
"		:CC respectivly) so you can choose what to do to a line, or line
"		range. (I like to comment off entire sections of code, sometimes those
"		sections contain line comments.. toggleing in this instance has
"		undesired results.) Now I can simply highlight and :CC
"
"	Another (possibly) nice addition is the ability to specify the comment
"		symbol on after the command, I.e. :CC // which would add // chars to
"		the start of the range of lines.
"
"	The range defaults to the currnet line, as per VIM documentation.
"
" }}}
"
" Note:		If someone wants this to not work on blank lines or has comment
"			strings or any other suggestions just email me and I'll look into
"			it.
"
" Examples:
"	:CC--
"	:CC --
"	" spaces are ate between the command and the string, if you want a space
"	use \ , i.e.
"	:CC\ --
"
" Holding: Based on {{{
"
" ToggleCommentify.vim
" Maintainer:	Vincent Nijs <Vincent.Nijs@econ.kuleuven.ac.be>
" Version:		1.2	
" Last Change:	Sunday, June 9th, 2001

" Disription:
" This is a (very) simple script to comment lines in a program. Currently supported languages are 
" C, C++, PHP, the vim scripting language, python, and ox. Given the simplicity of the program it 
" very easy to add support for new languages. The comments in the file should provide sufficient 
" information on how to proceed. 

" Install Details:
" You can put the functions in the attachment directly into your .vimrc or in a separate file to 
" be sourced. If you choose for the latter option add a statement such as ... 
" execute source ~/vim/myVimFiles/ToggleCommentify.vim	|" DO PUT source ... vim BETWEEN DOUBLE QUOTES !!)
" ... to your .vimrc file 
" To call the functions add the following mappings in your .vimrc. 
" map <M-c> :call ToggleCommentify()<CR>j 
" imap <M-c> <ESC>:call ToggleCommentify()<CR>j 
" The nice thing about these mapping is that you don't have to select a visual block to comment 
" ... just keep the ALT-key pressed down and tap on 'c' as often as you need. 

" Note: some people have reported that <M-c> doesn't work for them ... try <\-c> instead.

" [Feral:201/02@02:57] Old version; what I have been using (works great!) up
" untill I got playing with this :) .. changes from when I downloaded from
" vim.sf.net ... few more fileTypes recognised ... I think a bug fix with the
" makefile file type (been a long while thought..).. changed how isCommented
" is calcualted to accomidate longer commentsymbols.
"function! ToggleCommentify(Style) " {{{
"	let lineString = getline(".")
"	" [Feral:201/02@01:15] But but I want to comment empty lines!
""	if lineString != $									" don't comment empty lines
"	let fileType = &ft								" finding out the file-type, and specifying the comment symbol
"	" {{{ Supported file types each have an if here, look at 'vim' filetype as an example.
"	" [Feral:201/02@01:17] ftf is my hypertext markup format. (which is to say txt with a few special chars)
"	if fileType == 'ox' || fileType == 'cpp' || fileType == 'php' || fileType == 'java'
"		let commentSymbol = '//'
"	elseif fileType == 'vim'
"		let commentSymbol = '"'
"	elseif fileType == 'lisp' || fileType == 'scheme' || fileType == 'dosini'
"		let commentSymbol = ';'
"	elseif fileType == 'tex'
"		let commentSymbol = '%'
"	elseif fileType == 'caos'
"		let commentSymbol = '*'
"	elseif fileType == 'm4' || fileType == 'config' || fileType == 'automake'
"		let commentSymbol = 'dnl '
"	elseif fileType == 'python' || fileType == 'perl' || fileType == 'make' || fileType =~ '[^w]sh$' || fileType == 'tcl' || fileType == 'jproperties'
"		let commentSymbol = '#'
"	elseif fileType == 'vb' || fileType == 'aspvbs'
"		let commentSymbol == "'"
"	elseif fileType == 'plsql' || fileType == 'lua'
"		let commentSymbol = '--'
"	else
""		execute 'echo "ToggleCommentify has not (yet) been implemented for this file-type"'
""		let commentSymbol = ''
""		execute 'echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"'
"		echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"
"		let commentSymbol = '//'
"	endif
"	" }}}
"
"	" [Feral:201/02@01:24] toggle the comment (what it was, and default)
"	if a:Style == 'c'
"		call Commentify(commentSymbol)
"	elseif a:Style == 'u'
"		call UnCommentify(commentSymbol)
"	else
"		let isCommented = strpart(lineString,0,strlen(commentSymbol) )		" FERAL: extract the first x chars of the line, where x is the width/length of the comment symbol.
"		if isCommented == commentSymbol
"			call UnCommentify(commentSymbol)			" if the line is already commented, uncomment
"		else
"			call Commentify(commentSymbol)				" if the line is uncommented, comment
"		endif
"	endif
"
""	endif
"endfunction " }}}
" }}}
"
" }}}


if exists("loaded_feraltogglecommentify")
	finish
endif
let loaded_feraltogglecommentify = 1


" Look to the vim entry here to add your own file types.
function s:FindCommentify() " {{{
	" finding out the file-type, and specifying the comment symbol
	let fileType = &ft

	if fileType == 'ox' || fileType == 'cpp' || fileType == 'php' || fileType == 'java'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:201/02@01:17] ftf is my hypertext markup format. (which is to say
	"	txt with a few special chars)
	elseif fileType == 'ftf'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:283/02@04:14] torque-script
	elseif fileType == 'torquescript'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	"[Feral:303/02@19:20] fte
	"[Feral:317/02@05:23] fte is a template expansion system; unreleased as of
	"	yet.
	elseif fileType == 'fte'
		let commentSymbol_L = 'FTE:'
		let commentSymbol_R = ''
	"[Feral:308/02@02:02] html -- first start/end comment
	"12-Nov-2002: Bernhard Wagner's xml handling
	elseif fileType == 'html' || fileType == 'xml'
		let commentSymbol_L = '<!-- '
		let commentSymbol_R = ' -->'
"[Feral:308/02@02:03] old style c /* */ comments; just because. -- not testing it now thought!
"	elseif fileType == 'c'
"		let commentSymbol_L = '/*'
"		let commentSymbol_R = '*/'
	" The rest...
	elseif fileType == 'pov'
		let commentSymbol_L = '//'
		let commentSymbol_R = ''
	elseif fileType == 'vim'
		let commentSymbol_L = '"'
		let commentSymbol_R = ''
	elseif fileType == 'lisp' || fileType == 'scheme' || fileType == 'dosini'
		let commentSymbol_L = ';'
		let commentSymbol_R = ''
	elseif fileType == 'tex'
		let commentSymbol_L = '%'
		let commentSymbol_R = ''
	elseif fileType == 'caos'
		let commentSymbol_L = '*'
		let commentSymbol_R = ''
	elseif fileType == 'm4' || fileType == 'config' || fileType == 'automake'
		let commentSymbol_L = 'dnl '
		let commentSymbol_R = ''
	elseif fileType == 'python' || fileType == 'perl' || fileType == 'make' || fileType =~ '[^w]sh$' || fileType == 'tcl' || fileType == 'jproperties'
		let commentSymbol_L = '#'
		let commentSymbol_R = ''
	elseif fileType == 'vb' || fileType == 'aspvbs'
		let commentSymbol_L == "'"
		let commentSymbol_R = ''
	elseif fileType == 'plsql' || fileType == 'lua'
		let commentSymbol_L = '--'
		let commentSymbol_R = ''
	else
		execute 'echo "ToggleCommentify has not (yet) been implemented for this file-type"'
		let commentSymbol_L = ''
		let commentSymbol_R = ''
"		echo "ToggleCommentify: Unknown filetype, defaulting to CPP style //"
"		let commentSymbol_L = '//'
"		let commentSymbol_R = ''
	endif

	" this function is ment to be executed as a way of returning two vars; see
	" :h return
	return "let CommentSymbol_L = '" . commentSymbol_L . "' | let CommentSymbol_R = '" . commentSymbol_R ."'"

endfunction " }}}

function s:DoCommentify(DaMode, DaOldCol, ...) " {{{
	"[Feral:300/02@07:24] To work with the range param, just add in the LR
	"sniplet, the LR.openfold sniplet and then add LR as the range to the
	"below subistutes. (I don't want to test these changes now!) TODO!
	"i.e. {{{
"//"	let LR = a:firstline.",".a:lastline
"//""	echo confirm(LR)
"//"
"//"	" expand folds (required), else :s will operate on the entire fold count
"//"	"	times, with count being the number of lines in the fold.
"//"	execute ":silent! ".LR."foldopen!"
"//"
"//"	silent! execute LR.':s/^\(FTE:.\{-}\)\s\+FTE:.*$/\1/'
"//"	"
"//" }}}

	if(a:0 == 0)
		execute s:FindCommentify()
	elseif a:0 == 2
		let CommentSymbol_L = a:1
		let CommentSymbol_R = a:2
	else
		let CommentSymbol_L = a:1
		let CommentSymbol_R = ""
	endif

	" [Feral:201/02@01:46] GATE: nothing to do if we have no comment symbol.
	"[Feral:308/02@02:04] CommentSymbol_R is allowed to be blank so we only
	"	check CommentSymbol_L
	if strlen(CommentSymbol_L) == 0
		return
	endif

"	echo confirm(a:DaMode."\n".a:DaOldCol."\n".CommentSymbol_L)


	" Save where we are
	let SavedMark = line('.').'G'.a:DaOldCol.'|'
	normal! H
	let SavedMark = 'normal! '.line('.').'Gzt'.SavedMark
	execute SavedMark



"	" [Feral:201/02@01:15] I want to comment empty lines so this is remed out.
"	let IsBlankLineString = getline(".")
"	if IsBlankLineString != $
"		" don't comment empty lines
"		return
"	endif



	" [Feral:201/02@03:43] folded lines must be opend because a substitute
	" operation on a fold effects all lines of the fold. When called from a
	" range the result is that the lines of the fold have the substitute
	" command executed on them as many times as there is folded lines.
	" So, as a HACK if there is a fold, open it.
	if(foldclosed(line(".")) != -1)
		:foldopen
	endif

	let lineString = getline(".")
	" FERAL: extract the first x chars of the line, where x is the width/length of the comment symbol.
	let isCommented = strpart(lineString,0,strlen(CommentSymbol_L) )


	" 0 = toggle
	" 1 = comment
	" 2 = uncomment.
"	if a:DaMode == 1
	let ModeOfOperation = a:DaMode
	if ModeOfOperation == 0
		if isCommented == CommentSymbol_L
			" already commented, so uncomment.
			let ModeOfOperation = 2
		else
			" not already commented, so comment.
			let ModeOfOperation = 1
		endif
	endif

	let CommentSymbol_L = escape(CommentSymbol_L, '/\\')
	let CommentSymbol_R = escape(CommentSymbol_R, '/\\')
	set nohlsearch
	if ModeOfOperation == 2
		" Uncomment -- remove the comment markers.
		silent execute ':s/^'.CommentSymbol_L.'//'
		if strlen(CommentSymbol_R)
			silent execute ':s/'.CommentSymbol_R.'$//'
		endif
	else
		" else ModeOfOperation == 1
		" Comment -- add the comment markers.
		silent execute ':s/^/'.CommentSymbol_L.'/'
		if strlen(CommentSymbol_R)
			silent execute ':s/$/'.CommentSymbol_R.'/'
		endif
	endif
	set hlsearch


	" Return to where we were
	execute SavedMark
	unlet SavedMark
endfunction
" }}}

"*****************************************************************
" Commands: {{{
"*****************************************************************
"Holding: {{{
":command -nargs=? -range TC :<line1>,<line2>call <SID>ToggleCommentify(<f-args>)
":command -nargs=? -range CC :<line1>,<line2>call <SID>Commentify(<f-args>)
":command -nargs=? -range UC :<line1>,<line2>call <SID>UnCommentify(<f-args>)
"}}}
if !exists(":TC")
	:command -nargs=? -range TC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(0, b:FTCSaveCol, <f-args>)|:unlet b:FTCSaveCol
endif
if !exists(":CC")
	:command -nargs=? -range CC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(1, b:FTCSaveCol, <f-args>)|:unlet b:FTCSaveCol
endif
if !exists(":UC")
	:command -nargs=? -range UC		:let b:FTCSaveCol = virtcol('.')|<line1>,<line2>call <SID>DoCommentify(2, b:FTCSaveCol, <f-args>)|:unlet b:FTCSaveCol
endif

if !hasmapto('<Plug>FtcTc') && mapcheck("<M-c>", "nvi") == ""
	nmap <unique>	<M-c>	<Plug>FtcTc
	vmap <unique>	<M-c>	<Plug>FtcTc
	imap <unique>	<M-c>	<esc><Plug>FtcTc
endif
noremap <unique> <script> <Plug>FtcTc  :TC<CR>j



"[Feral:317/02@05:40] This is basicaly a hack; hopefully I'll COMBAK to this
"	someday and clean it up. (there is no reason for a <plug> to rely on the
"	:commands the script defines for example)
" DLAC -- duplicate line(s) and comment.
" Mangles mark z
if exists(":CC") && exists(":UC")
	if !hasmapto('<Plug>FtcDlacNormal') && mapcheck("<S-C-c>", "n") == ""
		" Normal Same keys as Multi-Edit, fwiw.
		"[Feral:314/02@19:28] Save shift is not recognised; these come out as
		"	<C-c>, dern!
		nmap <unique>	<S-C-c>	<plug>FtcDlacNormal
	endif
	if !hasmapto('<Plug>FtcDlacVisual') && mapcheck("<S-C-c>", "v") == ""
		" visual maping to handle multiple lines...
		vmap <unique>	<S-C-c>	<plug>FtcDlacVisual
	endif

	noremap		<unique> <script> <Plug>FtcDlacNormal	mzyyp`z:CC<CR>j
	vnoremap	<unique> <script> <Plug>FtcDlacVisual	mz:CC<cr>gvyPgv:UC<CR>`z
endif

" }}}

"End of file
