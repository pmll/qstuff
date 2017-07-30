
" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn match nvpName	"^ecs"
syn match nvpName	"^ems"
syn match nvpName	"^group"
syn match nvpName	"^application"
syn match nvpName	"^json"
syn match nvpName	"^quicklib"
syn match nvpName	"^modules-root"
syn match nvpName	"^source-lang"
syn match coComment	"#.*$"

hi def link nvpName	Statement
hi def link coComment	Comment

