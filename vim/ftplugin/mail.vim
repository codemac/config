" Nuke nested levels of quotations
map ,nq :%g/^>>/d<CR>

" Clear Empty Lines
map ,cel :%s/^\s\+$//e
map ,cqoq :%s/^>\s\+/> /e

" remove all > On blah... stuff left behind in quoted text - huggie
nmap ,cqmh :g/^\([>*] \)\+On.*wrote:$/d<CR>

" Clear blank lines after my On foo wrote: 
map ,db /^On.*wrote:$/e

" Kill more than 1 empty quoted lines
nmap ,ceql :g/^\(>\)\{2,}\s*$/d<CR>
nmap ,cqel :%s/^> \s*$//<CR>

" Kill power quote - change wierd "> blah>" to >>
nmap ,kpq :s/^> *[a-zA-Z]*>/>>/e

" kill space runs (3 or more spaces become 2 space)
nmap ,ksr :%s/   \+/  /g

" remove quoted sig
map ,rq /^> *-- 

"    ,Sl = "squeeze lines"
"    Turn all blocks of empty lines (within current visual)
"    into *one* empty line:
map ,dl :g/^$/,/./-j

" Condense multiple Re:'s
" map ,re 1G/^Subject:>CR<:s/\(Re: \)\+/Re: /e<CR>
map ,re call MailCondenseRe()

" Sven's wondeful change subject macro
map ,cs 1G/^Subject: <CR>yypIX-Old-<ESC>-W
vmap ,qp    :s/^/> /<CR>


" Clean the Email Function
function! CleanEmail()
" Remove empty quoted lines
    normal ,ceql
" Remove the empty lines after an unquoted On blah stuff
    normal ,db
" Clear empty lines and turn into space to write in
    normal ,cqel
" Remove blocks of empty lines
    normal ,dl
" Remove quoted On blah stuff
"    normal ,cqmh
" Remove many Re:'s from the Subject line
    normal ,re
endfun

function! Fixflowed()
   " save position
   let l = line(".")
   let c = col(".")
   normal G$
   " whiles are used to avoid nasty error messages
   " add spaces to the end of every line
   while search('\([^]> :]\)\n\(>[> ]*[^> ]\)','w') > 0
      s/\([^]> :]\)\n\(>[> ]*[^> ]\)/\1 \r\2/g
   endwhile
   " now, fix the wockas spacing from the text
   while search('^\([> ]*>\)\([^> ]\)','w') > 0
      s/^\([> ]*>\)\([^> ]\)/\1 \2/
   endwhile
   " now, compress the wockas
   while search('^\(>>*\) \(>>*\( [^>]\)*\)', 'w') > 0
      s/^\(>>*\) \(>>*\( [^>]\)*\)/\1\2/
   endwhile
   " restore the original location, such as it is
   execute l . " normal " . c . "|"
endfun

function DelSig()
    let modified=&modified
    let lnum = line(".")
    let cnum = col(".")
    normal! H
    let scrtop = line(".")
    normal! G
    execute '?-- $?,$d'
    call cursor( scrtop, 0 )
    normal! zt
    call cursor( lnum, cnum )
    if modified == 0
        set nomodified
    endif
endfun

nnoremap ,ds :silent call DelSig()

set list
set listchars=trail:_,tab:>.
set expandtab
set textwidth=75
set comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:- 
set noshowmatch
set ft=headers
syn on

" Autoflow paragraphs you edit as you type, no more gq!
set fo=aw2t

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

silent call CleanEmail()
