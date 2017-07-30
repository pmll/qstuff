" Syntax file for lib files

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn keyword nvpName Boot Version Library Database HelpFile ResourceFile
syn match nvpName	"^Volume[0-9]\{1,2\}="me=e-1
syn match nvpName	"^Show Splash="me=e-1
syn match libSection	"^\[.*\]"
syn match nvpValue	"=.*"ms=s+1
syn match commentValue	"=\.\..*"ms=s+1

" map to standard vim syntax types
hi def link libSection		Statement
hi def link nvpName		Identifier
hi def link nvpValue		Constant
hi def link commentValue	Comment

let b:current_syntax = "qlib"
