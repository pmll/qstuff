" Vim syntax file for defs

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn keyword fieldType	number date string integer choice refString
syn match fieldName	"= .*:"ms=s+2,me=e-1
syn match fileComment	";.*$"

" ojects contained in links summary
syn match fileNameInLink contained "^[^ -]*"

" objects contained in the file definition
syn match fileNameInDef contained "^file [^ ]*"ms=s+5
syn match toSpecifier contained	" to [^ ]*"
syn match bySpecifier contained " by [^ ]*"

" file links summary region
syn region linksRegion start="^[^ ]" end="^----$" contains=fileNameInLink fold

" file definition region
syn region fileRegion start="^file " end="^end"
\ contains=fileDef,fieldType,fileComment,fileNameInDef,toSpecifier,
\bySpecifier,fieldName fold

" map to standard vim syntax types
hi def link fileNameInDef	Statement
hi def link fileNameInLink	Statement
hi def link fieldName		Identifier
hi def link fileComment		Ignore
hi def link toSpecifier		Comment
hi def link bySpecifier		PreProc
"hi def link fieldType		Type

let b:current_syntax = "defs"
