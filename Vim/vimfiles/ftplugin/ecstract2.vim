" vim script - specific to ecstract2 files

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1
let b:custom_indent = 2
let b:help_version = "release"

source $VIM/vimfiles/qcommon/compile.vim
source $VIM/vimfiles/qcommon/ecstracttabularize.vim

" folding
"set foldmethod=syntax
setlocal foldexpr=ExprFoldLevel(v:lnum)
setlocal foldmethod=expr
setlocal foldlevel=10
"set foldcolumn=10
" set tab to 2 chars
"set tabstop=2
"set shiftwidth=2
let &tabstop = b:custom_indent
let &shiftwidth = b:custom_indent
setlocal expandtab
" use autoindent for now, until we get more 'intelligent' indenting
setlocal autoindent
" set 80 column guide
setlocal colorcolumn=80
" set wondow size
set lines=34
set columns=115

" abbreviations
" ite - full if then else block from current position
inoreabbrev ite if then<CR>end else<CR>end<up><up><left>
" sw - empty switch block, because it's so hard to remember the syntax
inoreabbrev sw switch<CR>end<up><esc>o  case<CR>case<CR>case else<up><up><esc>o  statements<down><esc>.<down>.<up><up><up><up><up><up>$a

" tips
call cuetip#add('a', 'if', 'the ite abbreviation will create an if then else skeleton.')
call cuetip#add('a', 'switch', 'the sw abbreviation will create a switch skeleton.')
"inoreabbrev if <ESC>:call ShowTip('The ite abbreviation will create an if then else skeleton.')<CR>aif
"inoreabbrev switch <ESC>:call ShowTip('The sw abbreviation will create a switch skeleton.')<CR>aswitch

" get the spaces at the beginning of the given string
function! IndentChars(lineNo)
	let codeLine = getline(a:lineNo)
	let pos = 0
	while codeLine[pos] == " "
		let pos = pos + 1
	endwhile

	if pos == 0
		return ""
	else
		return codeLine[0 : pos - 1]
	endif 
endfunction


" toggle a given char at the beginning (after spaces) of a line
function! ToggleChar(togChar)
	let lineText = getline(".")
	let pos = 0
	while lineText[pos] == " "
		let pos = pos + 1
	endwhile

	let startChar = lineText[pos]

	if pos == 0
		let indentChars = ""
	else
		let indentChars = lineText[0 : pos - 1]
	endif

	if startChar == a:togChar
		call setline(".", indentChars . lineText[pos + 1 : 999])
	elseif startChar != ""
		call setline(".", indentChars . a:togChar . lineText[pos : 999])
	endif
endfunction


" create a block using current line as start of block
function! CreateBlock()
	
	let curLine = line(".")
	let indent = IndentChars(curLine)
	call append(curLine, indent . "end")
	call append(curLine, indent . repeat(" ", b:custom_indent))
	call cursor(curLine + 1, 999)	" 999 just to get to end of line

endfunction

function! IndentedBy(lineNo)

	let codeLine = getline(a:lineNo)
	let indentedBy = 0

	while codeLine[indentedBy] == " "
		let indentedBy = indentedBy + 1
	endwhile

	return indentedBy

endfunction


function! IncreaseIndent(lineNo, byChars)

	let codeLine = getline(a:lineNo)
	call setline(a:lineNo, repeat(" ", a:byChars) . codeLine)

endfunction


function! DecreaseIndent(lineNo, byChars)

	let codeLine = getline(a:lineNo)
	let pos = 0

	while codeLine[pos] == ' ' && pos < a:byChars
		let pos = pos + 1
	endwhile
	call setline(a:lineNo, codeLine[pos :])

endfunction


function! ReIndent(fromIndent, toIndent)

	let indentDiff = 0
	let curLine = 0
	let lastLine = line("$")
	let continued = 0
	let indentedBy = 0

	while curLine < lastLine
		let curLine = curLine + 1
		if IsBlank(curLine)
			let continued = 0
			continue
		endif
		if ! continued
			" this is not a continuation, so recalc the indent diff
			let indentedBy = IndentedBy(curLine)
			let indentDiff = ((indentedBy * a:toIndent) / a:fromIndent) - indentedBy
		endif
		if indentDiff < 0
			call DecreaseIndent(curLine, -1 * indentDiff)
		else
			call IncreaseIndent(curLine, indentDiff)
		endif
		let continued = HasContinuation(curLine)
	endwhile

endfunction


function! ChangeIndentation(newIndent)

	call ReIndent(b:custom_indent, a:newIndent)
	let b:custom_indent = a:newIndent
	let &tabstop = b:custom_indent
	let &shiftwidth = b:custom_indent

	"re-evalutate folding
	execute "normal zx"

endfunction


function! PreSaveIndent()

	if b:custom_indent != 2
		let curModified = &modified
		call ReIndent(b:custom_indent, 2)
		if ! curModified
			set nomodified
		endif
	endif

endfunction


function! PostSaveIndent()

	if b:custom_indent != 2
		call ReIndent(2, b:custom_indent)
		set nomodified
	endif

endfunction


" attempt to open chm file with a given language word
function! LangHelp(reservedWord)
	
	" the embedded html files for ecstract 2 help have an underscore
	" in front of them for some reason
	call system('"' . $VIM . '\vimfiles\ftplugin\langhelp.bat" ' .
\		b:help_version . ' ecstract _' . a:reservedWord)

endfunction

" FOLDING

" calculate indent level for given line
function! IndentLevel(lineNum)

	let lineText = getline(a:lineNum)
	let lineLen = len(lineText)
	let indentLevel = 0

	while indentLevel < lineLen && lineText[indentLevel] == " "
		let indentLevel = indentLevel + b:custom_indent
		"let indentLevel = indentLevel + 2
	endwhile

	return indentLevel / b:custom_indent
	"return indentLevel / 2

endfunction


" Is given line blank?
function! IsBlank(lineNum)

	let lineText = getline(a:lineNum)
	return lineText =~ "^ *$"

endfunction


" test if given line contains an end statement and nothing else
function! IsEndStatement(lineNum)

	let lineText = getline(a:lineNum)
	return lineText =~ "^ *end *$" || lineText =~ "^ *end *;"

endfunction


" given line continues on to next (ie \ or , or &)
function! HasContinuation(lineNum)

	" fixme: should really check if is comment line
	let lineText = getline(a:lineNum)
	return lineText =~ "[\\,&\(+-] *$" || lineText =~ "choice *$"

endfunction


" calculate fold level for given line
function! FoldLevel(lineNum)

	let foldLevel = 0	

	if IsBlank(a:lineNum)
		" for a blank line, set the fold level to the indent level
		" of the previous non-blank line
		let prevLine = a:lineNum - 1
		while prevLine > 0 && IsBlank(prevLine)
			let prevLine = prevLine - 1
		endwhile
		while prevline > 1 && HasContinuation(prevLine - 1)
			let prevLine = prevLine - 1
		endwhile
		
		if prevLine > 0
			let foldLevel = FoldLevel(prevLine)
			if IsEndStatement(prevLine)
				let foldLevel = foldLevel - 1
			endif
		endif
	elseif IsEndStatement(a:lineNum)
		" for an end statement, set the fold level to the indent + 1
		let foldLevel = IndentLevel(a:lineNum) + 1
	elseif a:lineNum > 1 && HasContinuation(a:lineNum - 1)
		let foldLevel = FoldLevel(a:lineNum - 1)
	else
		let indentLevel = IndentLevel(a:lineNum)
		let foldLevel = indentLevel
		let lastLine = line("$")
		let nextLine = a:lineNum
		while nextLine < lastLine && HasContinuation(nextLine)
			let nextLine = nextLine + 1
		endwhile
		let nextLine = nextLine + 1
		while nextLine <= lastLine && IsBlank(nextLine)
			let nextLine = nextLine + 1
		endwhile
		
		if nextLine <= lastLine
			let nextIndent = IndentLevel(nextLine)

			if nextIndent > indentLevel ||
\			   (IsEndStatement(nextLine) &&
\			    nextIndent == indentLevel)
				" next non blank line is more indented or end
				let foldLevel = foldLevel + 1
			endif
		endif
	endif

	return foldLevel

endfunction


" Fold level function callable by expression folding mechanism
function! ExprFoldLevel(lineNum)

	" clumsy way to find out if we are in a help file (has "\doc\")
	" because applying this folding to a help file causes havoc...
	"if bufname("") =~ "\\\\doc\\\\"
"		return 0
"	else
		let foldLevel = FoldLevel(a:lineNum)

		if IsEndStatement(a:lineNum)
			return "<" . foldLevel
		else
			return foldLevel
		endif
"	endif
	
endfunction

" KEY MAPS

" ctrl b in insert mode to create a new block
inoremap <C-B> <C-\><C-O>:call CreateBlock()<CR>
" the b character simply holds the current indentation level, otherwise the
" blank line would be collapsed down after <cr>
"inoremap <C-B> <CR>b<CR>end<up><bs><space><space>

" F9 to compile
"inoremap <f9> <C-O>:call QCompile("ecstract2",function("PreSaveIndent"),function("PostSaveIndent"))<CR>
"noremap <f9> :call QCompile("ecstract2",function("PreSaveIndent"),function("PostSaveIndent"))<CR>

" F7 to compile using experimental config file driven compile
inoremap <f7> <C-O>:call ExpSelectCompile()<CR>
noremap <f7> :call ExpSelectCompile()<CR>

" F2 language help for word currently under cursor
noremap <F2> :call LangHelp("<C-R><C-W>")<CR>
"inoremap <F2> <C-O>:call LangHelp("<C-R><C-W>")<CR>
inoremap <F2> <ESC>:call LangHelp("<C-R><C-W>")<CR>li

" These toggles need to be more clever as if we are commenting out code with
" various different indentation levels, the comment indentation needs to be
" set according to the first line
" toggle ';' comment
noremap ;; :call ToggleChar(";")<CR>

" toggle '#' comment
noremap ## :call ToggleChar("#")<CR>

