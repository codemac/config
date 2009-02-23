" Most recent update: Mon 23 Feb 2009 01:35:10 PM EST

""""""""""""""""""""""""""""""""""""""""""
" GUI options, and colorscheme selection "
""""""""""""""""""""""""""""""""""""""""""

set guioptions=aegit
set mouse=a
set ttymouse=xterm
map <F12> :browse confirm e<CR>
"color darktango
color zenburn

"""""""""""""
" Functions "
"""""""""""""

" Add timestamp to rc files
fun! <SID>UpdateRcHeader()
    let l:c=col(".")
    let l:l=line(".")
    1,10s-\(Most recent update:\).*-\="Most recent update: ".strftime("%c")-
    call cursor(l:l, l:c)
endfun

" Set up the status line
fun! <SID>SetStatusLine()
    let l:s1="%-3.3n\\ %f\\ %h%m%r%w"
    let l:s2="[%{strlen(&filetype)?&filetype:'?'},%{&encoding},%{&fileformat}]"
    let l:s3="%=\\ 0x%-8B\\ \\ %-14.(%l,%c%V%)\\ %<%P"
    execute "set statusline=" . l:s1 . l:s2 . l:s3
endfun


""""""""""""
" Settings "
""""""""""""
if has("win32")
	let Tlist_Ctags_Cmd='C:\Progra~1\ctags\ctags.exe'
	map <M-Space> :simalt ~<CR>
elseif has("macunix")
	" the Carbon clipboard is screwed up, and messes up newlines
	set cb=
endif
" Vim7 only settings
if v:version >= 700
    try
        setlocal numberwidth=3
    catch
    endtry
	set cursorline
	" Set special characters
	set listchars+=tab:»·,trail:·,extends:~,nbsp:.
endif

" Encoding
if ($TERM == "rxvt-unicode") && (&termencoding == "")
    set termencoding=utf-8
endif
set encoding=utf-8

" Basic options
set nocompatible
set history=500
set viminfo='1000,f1,:1000,/1000
set shortmess+=aI
set showmode
set showcmd
set modeline
set wildmenu
set nobackup

" Indent, tab, and wrap settings
set noexpandtab
set shiftwidth=4
set softtabstop=4
set tabstop=8
set shiftround
set autoindent
set smartindent
set nowrap
set formatoptions+=nl
set whichwrap=h,l,~,[,]
set backspace=eol,start,indent

" Search options
set ignorecase
set incsearch
set gdefault
set showmatch

" Set a toggle for pasting input
set pastetoggle=<F10>
" graphical options
";set guifont=Bitstream\ Vera\ Sans\ Mono\ 10
set guifont=Consolas\ 10

" Set bracket matching and comment formats
set matchpairs+=<:>
set comments-=s1:/*,mb:*,ex:*/
set comments+=s:/*,mb:**,ex:*/
set comments+=fb:*
set comments+=b:\"
set comments+=n::

" Use less space for line numbering if possible

" Turn line numbers on by default
set number

" Use css for generated html files
let html_use_css=1

" Setup a funky statusline
set laststatus=2
call <SID>SetStatusLine()

" Set taglist plugin options
let Tlist_Use_Right_Window = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Compact_Format = 1
let Tlist_File_Fold_Auto_Close = 0
let Tlist_Inc_Winwidth = 1

" valgrind
let g:valgrind_arguments = "--leak-check=yes --num-callers=5000 --time-stamp=yes"
let g:valgrind_use_horizontal_window = 1
let g:valgrind_win_height = 7


" Basic abbreviations
iab teh the
iab DATE <C-R>=strftime("%B %d, %Y (%A, %H:%M)")<CR>

" Enable filetype detection
filetype on
filetype plugin on
filetype indent on

" Set folding options
"set foldenable
"set foldmethod=syntax
map <F8> :set foldenable<CR>:set foldmethod=syntax<CR>zC

" Set ctags stuff
set tags=tags;

" mangels cool block quoting function
function! VBlockquote(...) range
    " put `| ' at beginning of line
    exe a:firstline.",".a:lastline."s/^/| /"
    " remove trailing whitespaces
    exe a:firstline.",".a:lastline.'s/^| $/|/e'
    " generate tail
    exe a:lastline."put ='`----'"
    " set mark
    normal m'
    " generate title
    let @z = ',----'
    if (a:0 != 0)
        " -> extra argument a:1
        let @z = @z."[ ".a:1." ]"
    endif
    exe a:firstline."put! z"
    " jump back to mark
    normal ''
endfunction

vmap bq :call VBlockquote("
""""""""""""""""
" Autocommands "
""""""""""""""""

" Clear autocmds - starts from a clean slate
autocmd!

" Fix filetype detection
au BufNewFile,BufRead .torsmorc* set filetype=rc
au BufNewFile,BufRead *.inc set filetype=php
au BufNewFile,BufRead *.sys set filetype=php
au BufNewFile,BufRead grub.conf set filetype=grub
au BufNewFile,BufRead *.dentry set filetype=dentry
au BufNewFile,BufRead *.blog set filetype=blog

" C file specific options
au FileType c,cpp set cindent
au FileType c,cpp set formatoptions+=ro
" au FileType c,cpp set makeprg=gcc\ -Wall\ -O2\ -o\ %<\ %

" HTML abbreviations
au FileType html,xhtml,php,eruby imap bbb <br />
au FileType html,xhtml,php,eruby imap aaa <a href=""><left><left>
au FileType html,xhtml,php,eruby imap iii <img src="" /><left><left><left><left>
au FileType html,xhtml,php,eruby imap ddd <div class=""><left><left>

" Compile and run keymappings
au FileType c,cpp map <F5> :!./%:r<CR>
au FileType java map <F5> :make %<CR>
au FileType sh,php,perl,python,ruby map <F5> :!./%<CR>
au FileType java map <F6> :java %:r
au FileType c,cpp map <F6> :make<CR>
au FileType php map <F6> :!php &<CR>
au FileType python map <F6> :!python %<CR>
au FileType perl map <F6> :!perl %<CR>
au FileType ruby map <F6> :!ruby %<CR>
au FileType html,xhtml map <F5> :!firefox %<CR>
au FileType ruby setlocal sts=2 sw=2				" Enable width of 2 for ruby tabbing

" MS Word document reading
au BufReadPre *.doc set ro
au BufReadPre *.doc set hlsearch!
au BufReadPost *.doc %!antiword "%"

" Update header in some files before saving
au BufWritePre *vimrc  :call <SID>UpdateRcHeader()
au BufWritePre *bashrc :call <SID>UpdateRcHeader()
au BufWritePre /etc/profile :call <SID>UpdateRcHeader()
au BufWritePre *Xdefaults :call <SID>UpdateRcHeader()
au BufWritePre *muttrc :call <SID>UpdateRcHeader()

"""""""""""""""
" Keymappings "
"""""""""""""""

" Easy help
map! <F1> <C-C><F1>
vmap <F1> <C-C><F1>
omap <F1> <C-C><F1>
nnoremap <F1> :help<Space>

map <F3> <Leader>mbt

" Turn of highlighting
map ; :noh<CR>


" Show nonprinting characters
map <F4> :set list!<CR>
inoremap <F4> <ESC>:set list!<CR>a

" Toggle between windows
"nnoremap <F4> <C-W>W
"nnoremap <F5> <C-W>w

" Toggle taglist script
map <F7> :Tlist<CR>

" Toggle line numbers
map <F8> :set number!<CR>

" Toggle dark/light default colour theme for shitty terms
map <F2> :let &background = ( &background == "dark" ? "light" : "dark" )<CR>

" Swap around between buffers
nnoremap <C-N> :bn<CR>
nnoremap <C-I> :bn<CR>
nnoremap <C-P> :bp<CR>
map \ <C-^><CR>

" Convert to html
nnoremap <C-L> :runtime<Space>syntax/2html.vim<CR>

" Fast quit -- removed so I can use macros!
"map q :q<CR>
"map Q :qa<CR>

" Cursor keys suck. Use ctrl with home keys to move in insert mode.
imap <C-h> <Left>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-l> <Right>

" Use o in insert mode
imap <C-O> <end><cr>
" Do Toggle Commentify
map <M-c> :call ToggleCommentify()<CR>j
imap <M-c> <ESC>:call ToggleCommentify()<CR>j

" 256 colors  WHY DO NO TERMINALS SUPPORT 256 COLORS YET DEAR GOD RACGOLY8FA
" DLIASRAROCH LA8O GAONID LAROCG AORD I
"
" anger.
" let &t_SI = "\<Esc>]12;yellow\x7"
" let &t_EI = "\<Esc>]12;grey\x7"
let &t_Co = 256


""""""""""""""""""""""""""""
" Enable syntax hilighting "
""""""""""""""""""""""""""""

syntax on

"""""""""""" DVORAK FTW LOLZ! "
noremap d h
noremap h j
noremap t k
noremap n l
noremap k d
noremap l n
noremap j t
noremap ^Wd ^Wh
noremap ^Wh ^Wj
noremap ^Wt ^Wk
noremap ^Wn ^Wl
inoremap ^] ^[A
inoremap ð ^N 

""" No arrow keys, you know how to use dhtn
"nmap <right> <nop>
"nmap <left> <nop>
"nmap <up> <nop>
"nmap <down> <nop>
"imap <right> <nop>
"imap <left> <nop>
"imap <up> <nop>
"imap <down> <nop>

""""""""""""""""""""""""""
"     NETAPP
""""""""""""""""""""""""""

cab co :!rcmd p4 edit %:p 
cab unco :!rcmd p4 revert %:p 
