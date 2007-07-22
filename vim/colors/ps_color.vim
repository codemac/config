" Vim colour file --- PSC
" Maintainer:	Pan Shizhu <dicpan@hotmail.com>
" Last Change:	17 November 2004
" Version:	2.82
" URL:		http://vim.sourceforge.net/scripts/script.php?script_id=760
"
"	Please prepend [VIM] in the title when writing e-mail to me, or it will
"	be automatically treated as spam and removed. 
"
"	See the help document for all details, the help document will be
"	installed after the script has been sourced once, do not open the
"	script when you source it for the first time.
"

" Initializations: {{{1
"
function! s:init_option(var, value)
  if !exists("g:psc_".a:var)
    execute "let s:".a:var." = ".a:value
  else
    let s:{a:var} = g:psc_{a:var}
  endif
endfunction
command! -nargs=+ InitOpt call s:init_option(<f-args>)

function! s:multi_hi(setting, ...)
  let l:idx = a:0
  while l:idx > 0
    let l:hlgroup = a:{l:idx}
    execute "highlight ".l:hlgroup." ".a:setting
    let l:idx = l:idx - 1
  endwhile
endfunction
command! -nargs=+ MultiHi call s:multi_hi(<f-args>)

InitOpt style 'cool'
InitOpt inversed_todo 0
InitOpt use_default_for_cterm 0
InitOpt statement_different_from_type 0
if s:style == 'warm'
  InitOpt fontface 'mixed'
else
  InitOpt fontface 'plain'
endif

if !has("gui_running")
  call s:init_option("cterm_style", "'".s:style."'")

  " Forces 'cool' style when gui is not present Since the 'warm' style for
  " terminal isn't available now, and probably never will be.
  if s:cterm_style=='warm' | let s:cterm_style = 'cool'
  endif
  if s:use_default_for_cterm==1 | let s:cterm_style = 'default'
  elseif s:use_default_for_cterm==2 | let s:cterm_style = 'defdark'
  endif
endif


InitOpt other_style 0

if has("gui_running")
  if s:style=='warm' || s:style=='default'
    set background=light
  elseif s:style=='cool' || s:style=='defdark'
    set background=dark
  else | let s:other_style = 1
  endif
else
  if s:cterm_style=='cool' || s:cterm_style=='defdark'
    set background=dark
  elseif s:cterm_style=='default'
    set background=light
  else | let s:other_style = 1
  endif
endif


highlight clear

if exists("syntax_on")
  syntax reset
endif

let s:color_name = expand("<sfile>:t:r")

if s:other_style==0 | let g:colors_name = s:color_name
  " Go from console version to gui, the color scheme should be sourced again
  execute "autocmd TermChanged * if g:colors_name == '".s:color_name."' | "
	\."colo ".s:color_name." | endif"
else
  execute "runtime colors/".s:style.".vim"
endif

" Command to go different schemes easier.
execute "command! -nargs=1 Colo if '".s:color_name."'!=\"<args>\" | "
	\'let g:psc_style = "<args>"| endif | colo '.s:color_name

" Give control to 'reloaded' scheme if possible
if s:style == 'reloaded'
  finish
endif

" }}}1

" Relevant Help: 
" :h highlight-groups
" :h psc-cterm-color-table
" :ru syntax/hitest.vim
"
" Hardcoded Colors Comment:
" #aabbcc = Red aa, Green bb, Blue cc
" we must use hard-coded colours to get more 'tender' colours
"


" GUI:
"
" I don't want to abuse folding, but here folding is used to avoid confusion. 
if s:style=='warm' 
  " Warm style for gui here {{{2
  " LIGHT COLOR DEFINE START

  highlight Normal		guifg=#000000	guibg=#e0e0e0
  highlight Search		guifg=#902000	guibg=#f8f8f8
  highlight Visual		guifg=fg	guibg=#a6caf0
  highlight Cursor		guifg=#f0f0f0	guibg=#008000
  " The idea of CursorIM is pretty good, however, the feature is still buggy
  " in the current version (Vim 6.3). 
  " The following line will be kept commented until the bug fixed.
  "
  " highlight CursorIM		guifg=#f0f0f0	guibg=#800080
  highlight Special		guifg=#907000	guibg=bg
  highlight Comment		guifg=#505800	guibg=bg
  highlight Number		guifg=#907000	guibg=bg
  highlight Constant		guifg=#007068	guibg=bg
  highlight StatusLine		guifg=fg	guibg=#a6caf0
  highlight LineNr		guifg=#686868	guibg=bg
  highlight Question		guifg=fg	guibg=#d0d090
  highlight PreProc		guifg=#009030	guibg=bg
  if s:statement_different_from_type==1
    highlight Statement		guifg=#4020a0	guibg=bg
  else
    highlight Statement		guifg=#2060a8	guibg=bg
  endif
  highlight Type		guifg=#0850a0	guibg=bg
  if s:inversed_todo==1
    highlight Todo		guifg=#e0e090	guibg=#000080
  else
    highlight Todo		guifg=#800000	guibg=#e0e090
  endif
  " NOTE THIS IS IN THE WARM SECTION
  highlight Error		guifg=#c03000	guibg=bg
  highlight Identifier		guifg=#a030a0	guibg=bg
  highlight ModeMsg		guifg=fg	guibg=#b0b0e0
  highlight VisualNOS		guifg=fg	guibg=#b0b0e0
  highlight SpecialKey		guifg=#1050a0	guibg=bg
  highlight NonText		guifg=#002090	guibg=#d0d0d0
  highlight Directory		guifg=#a030a0	guibg=bg
  highlight ErrorMsg		guifg=fg	guibg=#f0b090
  highlight MoreMsg		guifg=#489000	guibg=bg
  highlight Title		guifg=#a030a0	guibg=bg
  highlight WarningMsg		guifg=#b02000	guibg=bg
  highlight WildMenu		guifg=fg	guibg=#d0d090
  highlight Folded		guifg=fg	guibg=#b0e0b0
  highlight FoldColumn		guifg=fg	guibg=#90e090
  highlight DiffAdd		guifg=fg	guibg=#b0b0e0
  highlight DiffChange		guifg=fg	guibg=#e0b0e0
  highlight DiffDelete		guifg=#002090	guibg=#d0d0d0
  highlight DiffText		guifg=fg	guibg=#c0e080
  highlight SignColumn		guifg=fg	guibg=#90e090
  highlight IncSearch		guifg=#f0f0f0	guibg=#806060
  highlight StatusLineNC	guifg=fg	guibg=#c0c0c0
  highlight VertSplit		guifg=fg	guibg=#c0c0c0
  highlight Underlined		guifg=#6a5acd	guibg=bg	gui=underline
  highlight Ignore		guifg=bg	guibg=bg

  " LIGHT COLOR DEFINE END
  " }}}2
elseif s:style=='cool' 
  " Cool style for gui here {{{2
  " DARK COLOR DEFINE START

  highlight Normal		guifg=#d0d0d0	guibg=#202020
  highlight Comment		guifg=#d0d090	guibg=bg
  highlight Constant		guifg=#80c0e0	guibg=bg
  highlight Number		guifg=#e0c060	guibg=bg
  highlight Identifier		guifg=#f0c0f0	guibg=bg
  if s:statement_different_from_type==1
    highlight Statement		guifg=#98a8f0	guibg=bg
  else
    highlight Statement		guifg=#c0d8f8	guibg=bg
  endif
  highlight PreProc		guifg=#60f080	guibg=bg
  highlight Type		guifg=#b0d0f0	guibg=bg
  highlight Special		guifg=#e0c060	guibg=bg
  highlight Error		guifg=#f08060	guibg=bg
  if s:inversed_todo==1
    highlight Todo		guifg=#d0d090	guibg=#000080
  else
    highlight Todo		guifg=#800000	guibg=#d0d090
  endif
  highlight Search		guifg=#e0e0e0	guibg=#800000
  highlight Visual		guifg=#000000	guibg=#a6caf0
  highlight Cursor		guifg=#000000	guibg=#00f000
  " NOTE THIS IS IN THE COOL SECTION
  " highlight CursorIM		guifg=#000000	guibg=#f000f0
  highlight StatusLine		guifg=#000000	guibg=#a6caf0
  highlight LineNr		guifg=#b0b0b0	guibg=bg
  highlight Question		guifg=#000000	guibg=#d0d090
  highlight ModeMsg		guifg=fg	guibg=#000080
  highlight VisualNOS		guifg=fg	guibg=#000080
  highlight SpecialKey		guifg=#b0d0f0	guibg=bg
  highlight NonText		guifg=#6080f0	guibg=#101010
  highlight Directory		guifg=#80c0e0	guibg=bg
  highlight ErrorMsg		guifg=#d0d090	guibg=#800000
  highlight MoreMsg		guifg=#c0e080	guibg=bg
  highlight Title		guifg=#f0c0f0	guibg=bg
  highlight WarningMsg		guifg=#f08060	guibg=bg
  highlight WildMenu		guifg=#000000	guibg=#d0d090
  highlight Folded		guifg=#d0d0d0	guibg=#004000
  highlight FoldColumn		guifg=#e0e0e0	guibg=#008000
  highlight DiffAdd		guifg=fg	guibg=#000080
  highlight DiffChange		guifg=fg	guibg=#800080
  highlight DiffDelete		guifg=#6080f0	guibg=#202020
  highlight DiffText		guifg=#000000	guibg=#c0e080
  highlight SignColumn		guifg=#e0e0e0	guibg=#008000
  highlight IncSearch		guifg=#000000	guibg=#d0d0d0
  highlight StatusLineNC	guifg=#000000	guibg=#c0c0c0
  highlight VertSplit		guifg=#000000	guibg=#c0c0c0
  highlight Underlined		guifg=#80a0ff	guibg=bg	gui=underline 
  highlight Ignore		guifg=#000000	guibg=bg

  " DARK COLOR DEFINE END
  " }}}2
elseif s:style=='defdark'
  highlight Normal		guifg=#f0f0f0	guibg=#000000
endif

" Take NT gui for example, If you want to use a console font such as
" Lucida_Console with font size larger than 14, the font looks already thick,
" and the bold font for that will be too thick, you may not want it be bolded.
" The following code does this.
"
" All of the bold font may be disabled, since continuously switching between
" bold and plain font hurts consistency and will inevitably fatigue your eye!

" Maximum 20 parameters for vim script function
"
MultiHi gui=NONE ModeMsg Search Cursor Special Comment Constant Number LineNr Question PreProc Statement Type Todo Error Identifier Normal

MultiHi gui=NONE VisualNOS SpecialKey NonText Directory ErrorMsg MoreMsg Title WarningMsg WildMenu Folded FoldColumn DiffAdd DiffChange DiffDelete DiffText SignColumn

" For reversed stuffs
MultiHi gui=NONE IncSearch StatusLine StatusLineNC VertSplit Visual

if s:style=="cool" || s:style=="warm"
  if s:fontface=="mixed"
    MultiHi gui=bold IncSearch StatusLine StatusLineNC VertSplit Visual
  endif
else
  if s:fontface=="mixed"
    hi StatusLine gui=bold,reverse
  else
    hi StatusLine gui=reverse
  endif
  MultiHi gui=reverse IncSearch StatusLineNC VertSplit Visual
endif

" Enable the bold style
if s:fontface=="mixed"
  MultiHi gui=bold Question DiffText Statement Type MoreMsg ModeMsg NonText Title VisualNOS DiffDelete
endif




" Color Term:

" It's not quite possible to support 'cool' and 'warm' simultaneously, since
" we cannot expect a terminal to have more than 16 color names. 
"

" I assume Vim will never go to cterm mode when has("gui_running") returns 1,
" Please enlighten me if I am wrong.
"
if !has('gui_running')
  " cterm settings {{{1
  if s:cterm_style=='cool'

    highlight Normal	 ctermfg=LightGrey  ctermbg=Black
    highlight Search	 ctermfg=White	    ctermbg=DarkRed
    highlight Visual	 ctermfg=Black	    ctermbg=DarkCyan
    highlight Cursor	 ctermfg=Black	    ctermbg=Green
    highlight Special	 ctermfg=Yellow	    ctermbg=Black
    highlight Comment	 ctermfg=DarkYellow ctermbg=Black
    highlight Constant	 ctermfg=Blue	    ctermbg=Black
    highlight Number	 ctermfg=Yellow	    ctermbg=Black
    highlight StatusLine ctermfg=Black	    ctermbg=DarkCyan
    highlight LineNr	 ctermfg=DarkGrey   ctermbg=Black
    highlight Question	 ctermfg=Black	    ctermbg=DarkYellow
    highlight PreProc	 ctermfg=Green	    ctermbg=Black
    highlight Statement	 ctermfg=Cyan	    ctermbg=Black
    highlight Type	 ctermfg=Cyan	    ctermbg=Black
    if s:inversed_todo==0
        highlight Todo	 ctermfg=DarkRed    ctermbg=DarkYellow
    else
        highlight Todo	 ctermfg=DarkYellow ctermbg=DarkBlue
    endif
    highlight Error	 ctermfg=Red	    ctermbg=Black
    highlight Identifier ctermfg=Magenta    ctermbg=Black
    highlight Folded	 ctermfg=White	    ctermbg=DarkGreen
    highlight ModeMsg	 ctermfg=Grey	    ctermbg=DarkBlue
    highlight VisualNOS	 ctermfg=Grey	    ctermbg=DarkBlue
    highlight SpecialKey ctermfg=Cyan	    ctermbg=Black
    highlight NonText	 ctermfg=Blue	    ctermbg=Black
    highlight Directory	 ctermfg=Blue	    ctermbg=Black
    highlight ErrorMsg	 ctermfg=DarkYellow ctermbg=DarkRed
    highlight MoreMsg	 ctermfg=Green	    ctermbg=Black
    highlight Title	 ctermfg=Magenta    ctermbg=Black
    highlight WarningMsg ctermfg=Red	    ctermbg=Black
    highlight WildMenu	 ctermfg=Black	    ctermbg=DarkYellow
    highlight FoldColumn ctermfg=White	    ctermbg=DarkGreen
    highlight SignColumn ctermfg=White	    ctermbg=DarkGreen
    highlight DiffText	 ctermfg=Black	    ctermbg=DarkYellow
    highlight DiffDelete ctermfg=Blue	    ctermbg=Black

    if &t_Co==8
      " 8 colour terminal support, this assumes 16 colour is available through
      " setting the 'bold' attribute, will get bright foreground colour.
      " However, the bright background color is not available for 8-color terms.
      "
      " You can manually set t_Co=16 in your .vimrc to see if your terminal
      " supports 16 colours, 
      MultiHi cterm=none DiffText Visual Cursor Comment Todo StatusLine Question DiffChange ModeMsg VisualNOS ErrorMsg WildMenu DiffAdd Folded DiffDelete Normal
      MultiHi cterm=bold Search Special Constant Number LineNr PreProc Statement Type Error Identifier SpecialKey NonText MoreMsg Title WarningMsg FoldColumn SignColumn Directory DiffDelete

    else
      " Background > 7 is only available with 16 or more colors

      " Only use the s:fontface option when there is 16-colour(or more)
      " terminal support

      MultiHi cterm=none WarningMsg Search Visual Cursor Special Comment Constant Number LineNr PreProc Todo Error Identifier Folded SpecialKey Directory ErrorMsg Normal
      MultiHi cterm=none WildMenu FoldColumn SignColumn DiffAdd DiffChange Question StatusLine DiffText
      MultiHi cterm=reverse IncSearch StatusLineNC VertSplit

      " Well, well, bold font with color 0-7 is not possible.
      " So, the Question, StatusLine, DiffText cannot act as expected.

      call s:multi_hi("cterm=".((s:fontface=="plain") ? "none" : "bold"), "Statement", "Type", "MoreMsg", "ModeMsg", "NonText", "Title", "VisualNOS", "DiffDelete")

    endif

  elseif s:cterm_style=='defdark'
    highlight Normal	 ctermfg=LightGrey  ctermbg=Black
  endif
  " }}}1
endif


" Term:
" For console with only 4 colours (term, not cterm), we'll use the default.
" ...
" The default colorscheme is good enough for terms with no more than 4 colours
"


" Links:
"
if (s:style=='cool') || (s:style == 'warm')
  " COLOR LINKS DEFINE START

  highlight link		String		Constant
  " Character must be different from strings because in many languages
  " (especially C, C++) a 'char' variable is scalar while 'string' is pointer,
  " mistaken a 'char' for a 'string' will cause disaster!
  highlight link		Character	Number
  highlight link		SpecialChar	LineNr
  highlight link		Tag		Identifier
  " The following are not standard hi links, 
  " these are used by DrChip
  highlight link		Warning		MoreMsg
  highlight link		Notice		Constant
  " these are used by Calendar
  highlight link		CalToday	PreProc
  " these are used by TagList
  highlight link		MyTagListTagName	IncSearch
  highlight link		MyTagListTagScope	Constant

  " COLOR LINKS DEFINE END
endif


" Clean:
"
delcommand InitOpt
delcommand MultiHi

" vim:et:nosta:sw=2:ts=8:
" vim600:fdm=marker:fdl=1:
" HelpExtractor:
" The document {{{2
set lz
let docdir = substitute(expand("<sfile>:r").".txt",'\<colors[/\\].*$','doc','')
if !isdirectory(docdir)
 if has("win32")
  echoerr 'Please make '.docdir.' directory first'
  unlet docdir
  finish
 elseif !has("mac")
  exe "!mkdir ".docdir
 endif
endif

let curfile = expand("<sfile>:t:r")
let docfile = substitute(expand("<sfile>:r").".txt",'\<colors\>','doc','')
exe "silent! 1new ".docfile
silent! %d
exe "silent! 0r ".expand("<sfile>:p")
silent! 1,/^" HelpExtractorDoc:$/d
norm! GVkkd
call append(line('$'), '')
call append(line('$'), 'v' . 'im:tw=78:ts=8:noet:ft=help:fo+=t:norl:noet:')
silent! wq!
exe "helptags ".substitute(docfile,'^\(.*doc.\).*$','\1','e')

exe "silent! 1new ".expand("<sfile>:p")
1
silent! /^" HelpExtractor:$/,$g/.*/d
silent! wq!

unlet docdir 
unlet curfile
"unlet docfile
noh
finish

" ---------------------------------------------------------------------
" Put the help after the HelpExtractorDoc label...
" HelpExtractorDoc:
*ps_color.txt*  PSC For Vim version 6.3         Last change:  17 November 2004


PERSONAL COLOUR SWITCHER                                *ps_colour* *pscolor*


Author:  Pan, Shizhu.  <dicpan> at <hotmail o com> >
	(prepend '[VIM]' in the title or your mail may be silently removed.)
<
==============================================================================
CONTENTS                                                 *psc* *psc-contents*

	1. Contents.....................|psc-contents|
	2. PSC Overview.................|psc-overview|
	3. PSC Installation.............|psc-usage|
	4. PSC Options..................|psc-options|
	5. PSC under color term ........|psc-cterm|
	6. PSC FAQ and Tips ............|psc-faq|
	7. PSC Release notes............|psc-release-notes|
	8. PSC Todo List................|psc-todo|

For release notes, please see the header of ps_color.vim

==============================================================================
PSC FEATURES OVERVIEW                           *psc-features* *psc-overview*

	Features ~

	. PSC is firstly a color scheme which have both dark and light
	  background styles. 
	. It can have the same appearance in [cterm] as in [gui]. 
	. It is designed with gentle color to minimize fatigue of eye.
	. It also works with other color schemes.
	. Default foreground and background can easily be changed, it is more
	  configurable than most other color schemes
	. Works with the optional tool reloaded.vim, can change the whole
	  color scheme in Hue,Saturation,Luminance color space.

	Design Concern ~

	At the first glance this color scheme may look pretty 'dull', don't be
	afraid, this is quite normal.  Bear in mind that a text editor is not
	a photo album, if a text editor looks exciting you may not be able to
	stare at it for a long time.

	Predefined Vim Syntax highlighting can be too colorful or contrasty so
	that many programmers prefer to switch off the syntax highlighting at
	work.  That is not a good idea because you will lost the advantages of
	syntax high-lighting.  It is often the case that we have to work for
	300+ minutes, then I decide to do-it-myself. 

	Many user-defined color schemes in vim.sf.net tend to achieve low
	contrast by having a strong color-cast, i.e. looks blueish or
	yellowish or reddish.  This does look comfortable at first, however,
	any type of color-cast will cause the eyes less sensitive for
	particular color after a long-time work session, and that's no good to
	health. 

	Efforts had been made to ensure no color-cast for this scheme, all
	elementary colors like RGB and CYMK are evenly used.  Like TeX,
	'consistency' is the principle this color scheme based on.  Default
	values which hurt consistency are amended according to the vim script
	syntax/hitest.vim

	There are 3 parameters to describe a color: Hue, Saturation and
	Brightness.  In this color scheme, the saturation is low and the
	brightness are designed to be very close to each other in order not to
	fatigue our eyes after a whole day's programming work.
	
	Portability ~

	Different monitor settings led to different look.  In this color
	scheme, it is assumed that the monitor adjust at 6500k color
	temperature with a good gamma curve.  If you have a 9300k monitor or
	if the gamma curve is not optimal, the appearance may be less
	comfortable, use adobe gamma loader or similar tools to adjust
	your monitor if your monitor do not have the option to change color
	temperature and/or gamma curve. 
	
	Needless to say, VI is an editor originally designed to do edit tasks
	in a text terminal, and VIM is an improved version of VI.  Its a shame
	that a color scheme cannot have a satisfactory appearance in cterm.
	The cterm compatibility should be considered high priority when
	designing ViM color scheme.

	I had made much attempt to make support for 8-color terminals,
	however, 8 colors is not enough to represent a color scheme.  Finally
	I end up making the cterm support for 16-color terminal.  Have to say
	sorry if the color scheme sucks in your 8-color terminal, I had tried
	my best. More details about cterm please see |psc-cterm|.

							*psc-about-background*
	About the Background ~

	We have talked about off-white backgrounds, any background which is
	not black, grey or white should be changed constantly in order not to
	make the eyes less sensitive to particular color.  i.e. you can use
	blue background on Monday, red background on Tuesday, green background
	on Wednesday, but if you use blue background everyday, that's no good
	to your health.

	Now we talk about the brightness of the background. Why dark
	background is preferred over others? There are many reasons, such as,
	the monitor emits lower radiation for black background. You may have
	lots of similar reasons... 
	
	But I'll talk about something you may not know:
>
	It is easier to distinguish foreground colors on a dark background
	than on a light background. 

	At the same time, it is easier to distinguish background colors on
	a light background than on a dark background. 

	We will mainly change foreground colors for syntax highlighting.
<
	Hence, we can reduce the contrast and saturation of the color in
	a dark-background scheme, while retain the readability. Schemes with
	white background usually comes with higher contrast and saturation.
	This is probably the most important reason that the color scheme is
	designed to be dark-background instead of light one.

	Now we came to know, that change the foreground color is enough to
	emphasis text in a dark background, while for a white background, we
	need to change the font shape (bold or italic, etc.), or change the
	background color to effectively emphasis the text.  This is probably
	the reason Vim default scheme has bold properties for highlighting
	groups, because the default scheme is a light background one.

	No one knows what color scheme is best for you, except yourself. Try!

==============================================================================
PSC INSTALLATION                                                  *psc-usage*

	Step 1, Enable the color scheme ~

	To use PSC is simple, just put ps_color.vim into your
	[runtimepath]/colors and append the line >

		colorscheme ps_color
<
	to your |.vimrc|.  The [runtimepath] can be any directory listed in
	|vimfiles|, normally your $HOME/.vim in Unix or $HOME/vimfiles in
	Windows.

	Step 2, Install the help document ~
        
	The help document will be automatically installed when the colorscheme
	be sourced the first time.  If it is not, type :colo ps_color now.
        
	After successfully installed the help document, you can use >

		:help psc-options
<
	to go to the following section.

==============================================================================
PSC OPTIONS                                                     *psc-options*

	You can let these options in your ~/.vimrc, most options works for
	both GUI and cterm, only some of them do not work for both.

	Options set using the 'let' command must present [BEFORE] the color
	scheme been sourced. 

								*psc_style*
	Style ~
>
	let psc_style='cool'
	let psc_style='warm'
	let psc_style='default'
	let psc_style='defdark'
<
	This selects between styles of colors, 
	The 'cool' is the default, dark background. 
	The 'warm' is the experimental, light background scheme.

	See |psc-about-background| for more knowledge about the background,
	and the differences of two style.

	The 'default' and 'defdark' refers to Vim system default color scheme.
	Which are provided only for reference.

	Let psc_style to any string other than the above 4 will switch to the
	specified color scheme. For example, let psc_style='desert' and then
	activate the ps_color, the color scheme will be chosen according to
	desert.vim color scheme.

							    *psc_cterm_style*
	Color Term Style ~
>
	let psc_cterm_style='cool'
<
	This is exactly the same to psc_style, except that it only affects the
	console version of vim in a color terminal, the 'warm' is not
	available for cterm.  
	By default, it will be set to the same value as 'psc_style'. You can
	change it if you want different style in cterm from gui.


							       *psc_fontface*
	Font face ~
>
	let psc_fontface='plain'
	let psc_fontface='mixed'
<
	The Vim default behavior is the 'mixed', however, the mixed font style
	in a dark colorscheme is not optimal. This color uses 'plain' for
	'cool' style, i.e. No texts are bolded font. For 'warm', the default
	is still 'mixed', If you want the mixed style in which the highlighted
	statements are bolded font, choose this. If you want all texts be
	bolded, choose 'plain' and specify a bolded guifont or terminal font.

	In GUI, this option also works for other color schemes. You can
	disable the bold font and use your favorite color scheme. See
	|psc-faq-ffothers| for detail.

							  *psc_inversed_todo*
	Inversed Todo ~
>
	let psc_inversed_todo=1
<
	When set to 1, the TODO group will be dark background with light font,
	Otherwise, the TODO group have light background with dark foreground.
	Default is 0.

						  *psc_use_default_for_cterm*
	Use default for cterm (obsoleted)~

	This option is Obsoleted, retained only for backward compatibility,
        see |psc_cterm_style| for alternative.

					  *psc_statement_different_from_type*
	Statement different from type ~
>
	let psc_statement_different_from_type=1
<
	The Statement-group and Type-group are easy to distinguish, different
	color for them are not necessary, I use similar color for S-group
	& T-group in order not to make the screen too 'colorful', also this
	saves a color name for cterm.  But if you do want the Statement & Type
	to be different color, try 'let statement_different_from_type=1' in
	your .vimrc file, which is available only for GUI.  Since the color
	names in cterm is limited to 16 we cannot have too many different
	colors in cterm.
	Default is 0, i.e. they have very similar color.

						*psc-change-background*
	Changing the Background color ~

	You may prefer a dark background over pure black one, and it is
	possible to change the background, this may make life more interesting.
	To do this is quite straight forward for GUI, just define the Normal
	highlight in your .gvimrc, [AFTER] the color scheme has been sourced.

	For example: 
>
	highlight Normal guibg=#103040
<
	The #103040 will give a taste similar to oceandeep, #152535 for
	hhazure, #303030 for desert, #404040 for zenburn...  Replace #103040
	with any color you like. You can do the same to guifg foreground if
	you are careful enough, remember this is only possible with ps_color
	version 2.7 or above, and only possible for GUI.

	You can do this to the NonText group also, for example.
>
	highlight NonText guibg=#202020
<
	will give you a taste similar to most color schemes on vim.sf.net, in
	which the NonText has a different background than Normal text.
	However, this is only useful in GUI, in cterm, there are only
	8 background colors, so it is wise not to have a different color.

        If you want more variations, please try the optional utility
        reloaded.vim, this optional utility provides an amazing level of
        customization.

	Quick switching between warm and cold styles ~

	Here is an example to define hot key of different style switching,
	note that I had only given this example without actually define it.
	You can choose to define it in .vimrc or anyway you prefer. 
>
	nnoremap <Leader>pc :let psc_style='cool'<CR>:colo ps_color<CR>
	nnoremap <Leader>pw :let psc_style='warm'<CR>:colo ps_color<CR>
<
	Alternatively, you can use the capitalized :Colo command, like 
	:Colo cool or :Colo warm

==============================================================================
PSC WITH CTERM                                                    *psc-cterm*

	Colour Term ~

	The cterm color is designed mainly in these terminals: 
>
	1. Cygwin bash shell in NT command prompt box
	2. XTERM and RXVT
	3. Other color terminals which have at least 16 colors
<
	*psc-cterm-nt*
	In Windows NT Prompt console you can change the exact value of each
	color, so you can have the same color with your GUI version of Vim,
	for 'cool' color style you just change the color according to the
	|psc-cterm-color-table|, for how to redefine the color of Windows NT
	prompt console please see Windows Help. 

	NT Cygwin bash shell console supports 16 foreground colors by add bold
	attribute to 8 color, the cterm=bold specifies which should be bright
	color, so totally the 16 color foreground is available, but color
	name DarkXXX and LightXXX are the same.
	
	The pre-configured Cygwin.lnk is available for download on my web page
	for Vim, but the site seems down, if my site would be on again, it
	should be at the following URL: >
		http://poet.tomud.com/pub/Cygwin.lnk.gz
<
	Cygwin is highly recommended for Vim user if you are using Windows NT
	based systems (e.g. NT 4.0, Win2k, WinXP, Win2003, etc). But Cygwin is
	not that versatile under Windows 95/98/ME. I'm not sure whether this
	works for DOS DJGPP or Windows 95 console version of Vim because
	I don't have the system, in case you encountered problem please
	contact me, if you like.

	*psc-cterm-xterm*
	XTERM is a much more feature-rich terminal than Windows Console so the
	support is much better, add the following recommend line into your
	.Xdefaults and you can achieve the same color as in GUI version.
 
	Add the following into your .Xdefaults:
	This works for XTERM and RXVT.
>
	XTerm*color0:           #000000
	XTerm*color1:           #800000
	XTerm*color2:           #008000
	XTerm*color3:           #d0d090
	XTerm*color4:           #000080
	XTerm*color5:           #800080
	XTerm*color6:           #a6caf0
	XTerm*color7:           #d0d0d0
	XTerm*color8:           #b0b0b0
	XTerm*color9:           #f08060
	XTerm*color10:          #60f080
	XTerm*color11:          #e0c060
	XTerm*color12:          #80c0e0
	XTerm*color13:          #f0c0f0
	XTerm*color14:          #c0d8f8
	XTerm*color15:          #e0e0e0
	XTerm*cursorColor:      #00f000

	! The following are recommended but optional
	XTerm*reverseVideo:     False
	XTerm*background:       #000000
	XTerm*foreground:       #d0d0d0
	XTerm*boldMode:         False
<
	There is an assumption that your RXVT or XTERM supports 16 colors,
	most RXVTs and XTERMs support this, if yours do not, get a source of
	RXVT and recompile it.

	Sometimes the color mode are not recognized well, or you do not want
	bright foreground be bolded. If this is the case, add the following in
	your .vimrc (before the color scheme been sourced)
>
	if &term=='xterm'     " Change 'xterm' to your term name if necessary
	    set t_Co=16
	endif
<
	If the t_Co=16 have problem, set t_Co=8 and :colo ps_color again.
	vice versa.
	
	My rxvt works well with t_Co=16: >
	Rxvt v2.7.10 - released: 26 MARCH 2003
	Options:
	XPM,transparent,utmp,menubar,frills,linespace,multichar_languages,
	scrollbars=rxvt+NeXT+xterm,.Xdefaults
<	But I've know that my rxvt v2.6.4 in another machine has problem with
	t_Co=16, if that is the case, set t_Co=8 instead.

	*psc-cterm-others*
	For other terminals, you can manually set the color according to the
	following table

	Hints for Manually set the color (for 'cool' style only):
	*psc-cterm-color-table*
	Color name	Hex value	Decimal value ~
	 0 Black	= #000000	0,0,0
	 4 DarkBlue	= #000080	0,0,128
	 2 DarkGreen	= #008000	0,128,0
	 6 DarkCyan	= #a6caf0	166,202,240
	 1 DarkRed	= #800000	128,0,0
	 5 DarkMagenta	= #800080	128,0,128
	 3 DarkYellow	= #d0d090	208,208,144
	 7 Grey		= #d0d0d0	208,208,208
	 8 DarkGrey	= #b0b0b0	176,176,176
	12 Blue		= #80c0e0	128,192,224
	10 Green	= #60f080	96,240,128
	14 Cyan		= #c0d8f8	192,216,248
	 9 Red		= #f08060	240,128,96
	13 LMag.	= #f0c0f0	240,192,240
	11 Yellow	= #e0c060	224,192,96
	15 White	= #e0e0e0	224,224,224

	*psc-cterm-incompatible*
	If your color terminal does only have 8 colors and cannot achieve 16
	colors with cterm=bold, you may want to switch to other color schemes
	to gain more readability. Anyway, you can specify in your .vimrc to
	use different color scheme under different consoles and GUI. 
	For example: 
>
	let psc_cterm_style = 'foobarcolor'
	let psc_style = 'cool'
	colo ps_color
<
	The 'foobarcolor' means the color scheme you want to choose, such as
	'desert', I recommend to try vim default schemes 'default' and
	'defdark' before experience others.

==============================================================================
PSC FAQ AND TIPS                                         *psc-faq* *psc-tips*
>
	Q: What is meant by `PS' ? 
<
	A: PS means: PostScript, PhotoShop, PerSonal, ... or anything you can
	   imagine and anything you want it do be.
>
	Q: How to obtain the same appreance as gui in color term?
<
	A: This need some work around, see |psc-cterm| for details.
	   Generally speaking, you should ensure your color term has support
	   for 16 foreground colors, and each color is customizable.

							  *psc-faq-ffothers*  >
	Q: How to use psc_fontface with other colorschemes?
<
	A: Make sure you had sourced :colo ps_color in your .vimrc, then you
	   can use the Capitalized :Colo instead of :colo
	   e.g. you want to use 'murphy', just type :Colo murphy after you
	   sourced the ps_color, the 'defdark', 'cool', 'warm' can also be
	   used here.
>
	Q: I updated from v2.0 to v2.3 or above, why the cterm color scheme
	   for Comment is different?
<
	A: The color map of DarkYellow and Yellow have been exchanged,
	   You need to reconfigure your terminal to meet the change,
	   see |psc-cterm-color-table| for guide, or if you are using xterm
	   compatible terminal, just update the .XDefaults according to
	   |psc-cterm-xterm|.
>
	Q: What do you mean by 'Vanilla Windows'?
<
	A: People often argue that Windows is not a REAL operating system.
	   Well, I agree this, but only for vanilla windows. i.e. with no
	   plug-ins installed.  Vanilla windows is a very limited platform,
	   since it is not POSIX compliant.

	   There are currently many working around to make Windows POSIX
	   Compliant, do you still mind which OS to use when it is POSIX
	   Compliant?  I don't. If you installed Cygwin kernel in your
	   NT-based Windows, the Windows will be more or less POSIX compliant
	   and you can use it in the same way as you use any Unix, BSD,
	   Solaris, Linux, XWindow, etc... What is more, Cygwin is not the
	   only kernel which makes Windows POSIX Compliant, make a google
	   search and you will find many alternatives.
>
	Q: How to change the Normal background color? Why don't you use
	   different background for NonText group?
<
	A: This is for compatibility, since we have to use only 8 colors as
	   background in a color terminal.  For GUI you can change this, see
	   |psc-change-background| for details.

==============================================================================
PSC RELEASE NOTES                                         *psc-release-notes*

	2.82 Release Note: ~

	Fixed bug with the reversed group for the Vim default, or other
	schemes.

        Fixed bug with the Diff mode fg mistaken as fg.

        Shrink the script a lot to improve load performance, moved the release
        notes into document.

        Change the default gui background color to #202020 (Dark Grey)


	2.81 Release Note: ~

	Provided a separate utility reloaded.vim to fine tune the GUI color
	scheme based on Hue, Saturation and Brightness(Luminance).  

	Added some groups to meet the need of reloaded.vim, no essential
	change.

	2.8 Release Note: ~

	Bugfix : when psc_style=='mixed', the visual got reversed wrong.

	'mixed' is now the default for 'warm' style.

	changed the function name to lower case.

	removed pre-2.0 compatibility, (the non-psc version of s-d-f-t).

	Added variable psc_cterm_style, see |psc_cterm_style|

	Added group Underline

	Tuned the function call.

	2.7 Release Note: ~

	Now it is possible to change the Background, 
	see :h psc-change-background for details.

	Linked the Tag group to Identifier.

	NonText as Notice is not good for 'warm', changed to Constant.

	Added links for the most popular plugins: taglist, calendar

	Tuned the 'Statement' color when different from Type (gui only).

	Re-adjusted cterm scheme according to syntax/hitest.vim

	The 'defdark' style for cterm is not functioning, fixed.

	Many 'cosmetic' changes, makes no difference for functionality.

	Use of DrChip's help extractor to auto-install help document.

	Added command define, :Colo

	2.6 Release Note: ~

	As stated in the v2.3, the only 'todo' thing seems to be the 'warm'
	style, now in this version I had been working on it. 

	There also are some minor fixes for the document, to be more friendly
	for new readers.

	The 'StatusLine' of 'cold' style is modified by mistake in the v2.3,
	this time the bug is fixed.

	The 'Directory' in GUI 'cold' style is different from 'cterm' one,
	now fixed.

	2.3 Release Note: ~

	This is an incompatible update, main changes are in 'cterm'.
	A new group 'SignColumn' had been added, new links added for engspchk
	v52, hundreds of typos fixed in the document, thanks to the engspchk.

	The support for 8-color cterm is slightly better now, but the mappings
	of the DarkYellow and Yellow are exchanged, you need to update the
	.Xdefaults or your terminal configuration before apply this update if
	you are using v2.0.  Guide for redefinition the color value is
	available in the document, make sure you had updated the ps_color.txt,
	then see |psc-cterm-color-table|

	2.0 Release Note: ~

	There've been great enhancement since this version, so I'd choose to
	bump the version number to 2. This version comes with Vim online help,
	if you had installed ps_color.txt, you can see for details in
	|pscolor|

	n/a Release: ~

	Initial upload, can be called as v1.8


==============================================================================
PSC TODO LIST                                                      *psc-todo*

	. Fix the remain bugs.
	. Follow the new Vim versions for new added highlighting group

==============================================================================
vim:et:nosta:sw=2:ts=8:
}}}2 vim600:fdm=marker:fdl=1:
