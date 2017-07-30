" Vim syntax file for ReportWriter

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" all code contained in the following 2 regions, this gives us a clue as code
" typed in the wrong place will have string literal/comment highlighting
syn region rwCode	start="^.[^.]" end="$" contains=ALL
syn region rwBracketExpression contained start="{" end="}" contains=ALL

" rw statements
syn keyword rwStatement	contained ADD CARRY CHAIN CLEAR CONST COPY DEFINE
syn keyword rwStatement	contained FORWARD LOCAL OPEN PARALLEL PASCAL PROC RECALL
syn keyword rwStatement contained TYPE VERIFY WHILE WIDTH DO ONLY
syn keyword rwStatement contained COMPILER LET FIND IF THEN ELSE MENU COLOUR
syn keyword rwStatement contained BEGIN END BOX MESSAGE WRITE LOAD_TITLES
syn keyword rwStatement contained LINE FORMAT SWITCH CASE ENTER DATAPANEL
syn keyword rwStatement contained MERGE PROMPT WARNING REMEMBER RETRIEVE DISPLAY
syn keyword rwStatement contained PRINTER HEADER

" rw functions
syn keyword rwFunction contained addr_key char comma contains_word copy crdr
syn keyword rwFunction contained cur_key custom_date date date_key day days
syn keyword rwFunction contained depth drcr ends find full_date func hour int
syn keyword rwFunction contained int_key integer is_date is_integer is_number
syn keyword rwFunction contained length line_num long_date lower memory minute
syn keyword rwFunction contained money month months name_key num number num_key
syn keyword rwFunction contained page_num parallel period_count period_days
syn keyword rwFunction contained pm_trace random round screen_num starts str
syn keyword rwFunction contained str_binary string surname_key time time_key
syn keyword rwFunction contained time_str upper when width words wrap year
syn keyword rwFunction contained zcomma zcrdr zdrcrzero
syn match rwFunction contained "contains"

" rw operators
syn keyword rwOperator contained AND OR FROM TO MOD

" types
syn keyword rwType contained string number integer boolean char choice date
syn keyword rwType contained diskp long_string record

" comments
syn match rwComment	"^\.\..*"
syn match rwComment	";.*"
" syn region rwComment contained start=";" end="}"me=e-1 contains=rwStringLiteralContained

" literals
syn region rwStringLiteral	start="^[^.].*" end="$" contains=rwBracketExpression
" just in case a commented out stirng literal contains a '}'
syn region rwStringLiteralContained start='"' end='"'
syn region rwStringLiteralContained start="'" end="'"
syn region rwStringLiteral	start='"' end='"'
syn region rwStringLiteral	start="'" end="'"
"syn keyword ecBooleanLiteral	true false

" built in variables
"syn match ecVar		"result"	" this will need refining

" map to standard vim syntax types
hi def link rwStatement		Statement
hi def link rwFunction		Function
hi def link rwComment		Comment
hi def link rwStringLiteralContained	Comment
hi def link rwStringLiteral	Constant
hi def link rwtype		Type
"hi def link ecBooleanLiteral	Boolean
"hi def link ecComp		Preproc
"hi def link ecOperator		Operator
"hi def link ecVar		Special

let b:current_syntax = "reportwriter"
