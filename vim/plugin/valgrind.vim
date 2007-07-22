" Vim global plugin for valgrind
" Maintainer: Rainer M. Schmid <rms@trolltech.com>
" Version: $Id: valgrind.vim,v 1.2 2003/03/31 08:43:27 rms Exp $


" General:
"
" Put this script in your .vim/plugin directory. It adds the command
" ":Valgrind" to run valgrind with the specified options. I usually start this
" with the following small script:
"
"    #!/bin/sh
"    vim -c "Valgrind $*" -c "only"
"
" Options:
"
" You can configure the behaviour with the following variables:
"
" g:valgrind_command
"    The command to start valgrind. If this variable is not set, "valgrind" is
"    used.
"
" g:valgrind_arguments
"    The arguments that should always be used when running valgrind. If this
"    variable is not set, "--num-callers=5000" is used.
"
" g:valgrind_use_horizontal_window
"    If this variable is set to a value not equal 0, the script uses
"    horizontal splits to show new windows. Otherwise it uses vertical splits.
"    The default is to use vertical splits.
"
" g:valgrind_win_width
"    Specifies the width of the window that shows the valgrind output. This
"    variable is only used with vertical splits. Defaults to 30.
"
" g:valgrind_win_height
"    Specifies the height of the window that shows the valgrind output. This
"    variable is only used with vertical splits. Defaults to 10.
"
" Example:
"
" If you want valgrind to always do leak checking, put the following into your
" .vimrc:
"
"     let g:valgrind_arguments='--leak-check=yes --num-callers=5000'

" Startup {{{

if exists("loaded_valgrind")
    finish
endif
let loaded_valgrind = 1

" save cpo (we use line-continuation)
let s:save_cpo = &cpo
set cpo&vim

" }}}

"--------------------------------------------
" Global mappings and commands
"--------------------------------------------
" Commands {{{

if !exists(":Valgrind")
    command -nargs=1 Valgrind call <SID>Valgrind(<f-args>)
endif


" }}}
" Options {{{

if !exists('g:valgrind_win_width')
    let g:valgrind_win_width = 30
endif
if !exists('g:valgrind_win_height')
    let g:valgrind_win_height = 10
endif

" }}}

"--------------------------------------------
" Functions
"--------------------------------------------
" Valgrind( filename ) {{{

function s:Valgrind( ... )
    let l:tmpfile=tempname()

    " construct the commandline and execute it
    let l:run_valgrind='!'
    if exists("g:valgrind_command")
	let l:run_valgrind=l:run_valgrind.g:valgrind_command
    else
	let l:run_valgrind=l:run_valgrind.'valgrind'
    endif
    if exists("g:valgrind_arguments")
	let l:run_valgrind=l:run_valgrind.' '.g:valgrind_arguments
    else
	let l:run_valgrind=l:run_valgrind.' --num-callers=5000'
    endif
    let l:i = 1
    while l:i <= a:0
	execute 'let l:run_valgrind=l:run_valgrind." ".a:'.l:i
	let l:i = l:i + 1
    endwhile
    let l:run_valgrind=l:run_valgrind.' 2>&1| tee '.l:tmpfile
    execute l:run_valgrind

    " show the result with the non-valgrind output stripped
    silent execute 'split '.l:tmpfile
    silent execute 'g!/^==\d*==/d'
    silent execute '%s/^==\d*== //e'
    silent execute '1'

    " make the buffer non-editable
    setl buftype=nowrite
    setl nobuflisted
    setl bufhidden=hide
    setl nomodifiable
    setl nowrap

    " syntax highlighting
    if has('syntax')
        syntax match ValgrindComment '^" .*'

        highlight clear ValgrindComment
        highlight link ValgrindComment Comment
    endif

    " fold settings
    if has('folding')
	setl foldenable
	setl foldmethod=expr
	setl foldexpr=getline(v:lnum)=~'^\\s*$'&&getline(v:lnum+1)=~'\\S'?'<1':1
    endif

    " show help
    call <SID>Show_Help(1)

    " mappings to go to error
    nnoremap <buffer> <silent> <CR> :call <SID>Jump_To_Error(0,0)<CR>
    nnoremap <buffer> <silent> o :call <SID>Jump_To_Error(1,0)<CR>
    nnoremap <buffer> <silent> <2-LeftMouse> :call <SID>Jump_To_Error(0,0)<CR>
    nnoremap <buffer> <silent> <Space> :call <SID>Jump_To_Error(0,1)<CR>
    " mappings for fold handlin
    nnoremap <buffer> <silent> + :silent! foldopen<CR>
    nnoremap <buffer> <silent> - :silent! foldclose<CR>
    nnoremap <buffer> <silent> * :silent! %foldopen!<CR>
    nnoremap <buffer> <silent> <kPlus> :silent! foldopen<CR>
    nnoremap <buffer> <silent> <kMinus> :silent! foldclose<CR>
    nnoremap <buffer> <silent> <kMultiply> :silent! %foldopen!<CR>
    " misc. mappings
    nnoremap <buffer> <silent> x :call <SID>Zoom_Window()<CR>
    nnoremap <buffer> <silent> ? :call <SID>Show_Help(0)<CR>
    nnoremap <buffer> <silent> q :close<CR>
endfunction

" }}}
" Find_File( filename ) {{{

function s:Find_File( filename )
    if filereadable( a:filename )
	return a:filename
    else
	" ### implement me
	"echo globpath( &path, a:filename )
    endif
endfunction

" }}}
" Jump_To_Error( new_window, stay_valgrind_window ) {{{

function s:Jump_To_Error( new_window, stay_valgrind_window )
    " do not process empty lines
    let l:curline = getline('.')
    if l:curline == ''
        return
    endif

    " if inside a fold, open it
    if foldclosed('.') != -1
	execute "foldopen"
        return
    endif

    " if the line doesn't start with "   at" or "   by" , return
    if match( l:curline, "   at" ) != 0 &&  match( l:curline, "   by" ) != 0
        return
    endif

    " determine file and line to go to
    let l:curline = substitute( substitute( l:curline, '.*(', '', '' ), ').*', '', '' )
    let l:filename = s:Find_File( substitute( l:curline, ':\d*$', '', '' ) )
    if l:filename == "" 
	return
    endif
    let l:linenumber = substitute( l:curline, '.*:', '', '' )

    " Goto the window containing the file with name l:filename. If the window
    " is not there, open a new window
    if bufname( l:filename ) == ""
	let l:bufnum = -1
    else
	let l:bufnum = bufnr( bufname( l:filename ) )
    endif
    let l:winnum = bufwinnr( l:bufnum )
    let l:val_winnum = winnr()
    if l:bufnum == -1 || l:winnum == -1
	" first find or create a suitable window
        if exists("g:valgrind_use_horizontal_window") && g:valgrind_use_horizontal_window
	    wincmd j
	    let l:winnum = winnr()
	    if l:winnum == l:val_winnum
		execute 'leftabove new'
		let l:winnum = winnr()
	    endif
        else
	    wincmd l
	    let l:winnum = winnr()
	    if l:winnum == l:val_winnum
		execute 'rightbelow vertical new'
		let l:winnum = winnr()
	    endif
        endif

	" open the file in that window
	if ( l:bufnum == -1 )
	    silent! execute 'edit '.l:filename
	else
	    silent! execute 'edit #'.l:bufnum
	endif
	if v:errmsg != ""
	    echoerr v:errmsg
	    execute l:val_winnum.'wincmd w'
	    return
	endif
        execute l:val_winnum.'wincmd w'
        if exists("g:valgrind_use_horizontal_window") && g:valgrind_use_horizontal_window
	    execute 'resize '.g:valgrind_win_height
	else
	    execute 'vertical resize '.g:valgrind_win_width
	endif
        execute l:winnum.'wincmd w'
    else
        execute l:winnum.'wincmd w'
        if a:new_window
            split
        endif
    endif

    " Goto the line l:linenumber and open a fold, if there is one.
    execute l:linenumber
    if foldclosed('.') != -1
	execute "foldopen"
    endif
    normal zz

    if ( a:stay_valgrind_window )
        execute l:val_winnum.'wincmd w'
    endif
endfunction

" }}}
" Zoom_Window() {{{

function s:Zoom_Window()
    if !exists("s:win_maximized")
	let s:win_maximized = 0
    endif
    if s:win_maximized
        if exists("g:valgrind_use_horizontal_window") && g:valgrind_use_horizontal_window
            execute 'resize ' . g:valgrind_win_height
        else
            execute 'vertical resize ' . g:valgrind_win_width
        endif
        let s:win_maximized = 0
    else
        if exists("g:valgrind_use_horizontal_window") && g:valgrind_use_horizontal_window
            resize
        else
            vertical resize
        endif
        let s:win_maximized = 1
    endif
endfunction

" }}}
" Show_Help( first_time ) {{{

function s:Show_Help( first_time )
    setl modifiable

    if !a:first_time
	normal G$
	if ( search( '^$', 'w' ) > 0 )
	    normal d1G
	endif
    endif

    if exists("s:show_help") && s:show_help == 1
	call append(0, '" <enter> : Jump to error')
	call append(1, '" o : Jump to error in new window')
	call append(2, '" <space> : Show error')
	call append(3, '" x : Zoom-out/Zoom-in valgrind window')
	call append(4, '" + : Open a fold')
	call append(5, '" - : Close a fold')
	call append(6, '" * : Open all folds')
	call append(7, '" q : Close the valgrind window')
	call append(8, '" ? : Remove help text')
	call append(9, '')
        let s:show_help = 0
    else
	call append(0, '" Press ? to display help text')
	call append(1, '')
        let s:show_help = 1
    endif

    normal 1G
    foldopen

    setl nomodifiable
endfunction

" }}}

" Cleanup {{{

" restore cpo
let &cpo = s:save_cpo

" }}}

" vim600:foldmethod=marker
