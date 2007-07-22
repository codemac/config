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

set list
set listchars=trail:_,tab:>.
set expandtab
set textwidth=100
set comments=s1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:- 
set noshowmatch
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
