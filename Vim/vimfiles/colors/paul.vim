" Vim color file
" Con't hurt my eyes

" First remove all existing highlighting.
set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif

let colors_name = "paul"

"hi Normal ctermfg=Black ctermbg=LightGrey guifg=Black guibg=grey97
hi Normal ctermfg=white ctermbg=black guifg=Black guibg=grey92

"line numbers
hi LineNr guifg=grey55
hi CursorLineNr guifg=grey45
