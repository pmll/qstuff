" vim script - specific to ecstract3 files

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1
let b:custom_indent = 2
let b:help_version = "release"

source $VIM/vimfiles/qcommon/compile.vim
" the things you would want to line up in pascal are similar to ecstract,
" i think...
source $VIM/vimfiles/qcommon/ecstracttabularize.vim

" This folding definitely won't work for pascal
" folding
"setlocal foldexpr=ExprFoldLevel(v:lnum)
"setlocal foldmethod=expr
"setlocal foldlevel=10
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
set maxfuncdepth=200  " waste memory in the pursuit of immutability

" abbreviations
" ite - full if then else block from current position
"inoreabbrev ite if then<CR>end else<CR>end<up><up><left>
" sw - empty switch block, because it's so hard to remember the syntax
"inoreabbrev sw switch<CR>end<up><esc>o  case<CR>case<CR>case else<up><up><esc>o  statements<down><esc>.<down>.<up><up><up><up><up><up>$a

" tips


function! Spaces(qty)

	return repeat(" ", a:qty)

endfunction


" get the spaces at the beginning of the given buffer line
function! IndentChars(lineNo)

	return matchstr(getline(a:lineNo), "^ *")

endfunction


"function! IsCommentedOut(lineNum)
"
"	return getline(a:lineNum) =~ "^ *[#;]"
"
"endfunction


"function! CommentOut(lineNum, level, commentChar)
"
"	let indentedBy = IndentedBy(a:lineNum)
"	let commentAt = min([a:level, indentedBy])
"	let lineText = getline(a:lineNum)
"	call setline(a:lineNum, strpart(lineText, 0, commentAt) . a:commentChar . strpart(lineText, commentAt))
"
"endfunction


"function! UnComment(lineNum)
"
"	let indentedBy = IndentedBy(a:lineNum)
"	let lineText = getline(a:lineNum)
"
"	call setline(a:lineNum, strpart(lineText, 0, indentedBy) . strpart(lineText, indentedBy + 1))
"
"endfunction


" still not sure how this should work - perhaps we should not have a toggle
" but an explicit comment / remove comment. that way we can have comments
" within comments if we want to (e.g. when commenting out large swathes)
"function! ToggleComment(commentChar)
"
"	let lineNum = line(".")
"	let indentedBy = IndentedBy(lineNum)
"
"	if IsCommentedOut(lineNum)
"		" de-comment
"		call UnComment(lineNum)
"	elseif lineNum > 1 && IsCommentedOut(lineNum - 1)
"		" previous line has comment, comment at same level as that
"		let prevIndentedBy = IndentedBy(lineNum - 1)
"		call CommentOut(lineNum, prevIndentedBy, a:commentChar)
"	else
"		" comment at current line indent level
"		call CommentOut(lineNum, indentedBy, a:commentChar)
"	endif
"
"endfunction


" *might* work out ok for pascal
" create a block using current line as start of block
function! CreateBlock()
	
	let curLine = line(".")
	let indent = IndentChars(curLine)
	call append(curLine, indent . "end")
	call append(curLine, indent . Spaces(b:custom_indent))
	call cursor(curLine + 1, 999)	" 999 just to get to end of line

endfunction


function! IndentedBy(lineNum)

	return strlen(IndentChars(a:lineNum))

endfunction


function! IncreaseIndent(lineNo, byChars)

	let codeLine = getline(a:lineNo)
	call setline(a:lineNo, Spaces(a:byChars) . codeLine)

endfunction


function! DecreaseIndent(lineNum, byChars)

	let codeLine = getline(a:lineNum)
	let currentIndent = IndentedBy(a:lineNum)
	let decreaseIndentBy = min([a:byChars, currentIndent])

	call setline(a:lineNum, strpart(codeLine, decreaseIndentBy))

endfunction


" fixme: nasty
function! ReIndent(fromIndent, toIndent)

	let indentDiff = 0
	let curLine = 0
	let lastLine = line("$")
	let continued = 0
	let indentedBy = 0

	while curLine < lastLine
		let curLine = curLine + 1
		if IsBlankLine(curLine)
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
		call ReIndent(b:custom_indent, 2)
	endif

endfunction


function! PostSaveIndent()

	if b:custom_indent != 2
		call ReIndent(2, b:custom_indent)
		set nomodified
	endif

endfunction


" attempt to open chm file with a given language word
"function! LangHelp(reservedWord)
"	
"	call system('"' . $VIM . '\vimfiles\ftplugin\langhelp.bat" ' . 
"\		b:help_version . ' E3LG ' . a:reservedWord)
"
"endfunction


"function! ToggleHelpVersion()
"
"	if b:help_version == 'release'
"		let b:help_version = 'dev'
"	else
"		let b:help_version = 'release'
"	end
"	echo 'Help version changed to ' . b:help_version
"
"endfunction


" more functional version of map
" disadvantage here is we can't take variables from local context like
" map can, we have to convert them to literals in the str expression
function! Fmap(expr, str)

	return map(deepcopy(a:expr), a:str)

endfunction


" find the true start of the given line
" (blank lines are thought of as trailing whitespace)
function! StartOfLine(lineNum)

	if a:lineNum <= 1
		return 1
	elseif IsBlankLine(a:lineNum) || HasContinuation(a:lineNum - 1)
		return StartOfLine(a:lineNum - 1)
	else
		return a:lineNum
	endif

endfunction


function! NextLine(lineNum)

	if a:lineNum >= line("$") - 1
		return line("$")
	elseif HasContinuation(a:lineNum) || IsBlankLine(a:lineNum + 1)
		return NextLine(a:lineNum + 1)
	else
		return a:lineNum + 1
	endif

endfunction


function! PrevLine(lineNum)

	let thisLine = StartOfLine(a:lineNum)
	return StartOfLine(thisLine - 1)

endfunction


function! PreviousLineWithIndent(lineNum, indent)

	let previousLine = PrevLine(a:lineNum)
	if previousLine <= 1
		return 1
	elseif IndentedBy(previousLine) == a:indent
		return previousLine
	else
		return PreviousLineWithIndent(previousLine, a:indent)
	endif

endfunction


function! PreviousLineWithSmallerIndent(lineNum, indent)

	let previousLine = PrevLine(a:lineNum)
	if previousLine <= 1
		return 1
	elseif IndentedBy(previousLine) < a:indent
		return previousLine
	else
		return PreviousLineWithSmallerIndent(previousLine, a:indent)
	endif

endfunction


function! NextLineWithIndent(lineNum, indent)

	let nextLine = NextLine(a:lineNum)
	if IsLastLine(nextLine) || IndentedBy(nextLine) == a:indent
		return nextLine
	else
		return NextLineWithIndent(nextLine, a:indent)
	endif

endfunction


function! NextLineWithSmallerIndent(lineNum, indent)

	let nextLine = NextLine(a:lineNum)
	if IsLastLine(nextLine) || IndentedBy(nextLine) < a:indent
		return nextLine
	else
		return NextLineWithSmallerIndent(nextLine, a:indent)
	endif

endfunction


function! StartOfBlock(lineNum)

	let lineStart = StartOfLine(a:lineNum)
	let lineIndent = IndentedBy(lineStart)
	if lineStart == 1
		return 1
	elseif IsEndStatement(lineStart)
		" end statement - start of block is on same level
		return PreviousLineWithIndent(lineStart, lineIndent)
	elseif lineIndent < IndentedBy(NextLine(lineStart))
		" we are at the start of a block
		return lineStart
	else
		" we are in middle of block - start of block is on lower level
		return PreviousLineWithSmallerIndent(lineStart, lineIndent)
	endif

endfunction


function! IsLastLine(lineNum)

	if a:lineNum == line("$")
		return 1  "true
	elseif HasContinuation(a:lineNum) || IsBlankLine(a:lineNum + 1)
		return IsLastLine(a:lineNum + 1)
	else
		return 0  "false
	endif

endfunction


function! EndOfBlock(lineNum)

	let lineStart = StartOfLine(a:lineNum)
	let indent = IndentedBy(lineStart)

	if IsLastLine(lineStart)
		return lineStart
	elseif IsEndStatement(lineStart)
		" end of block
		return lineStart
	elseif indent < IndentedBy(NextLine(lineStart))
		" start of block
		return NextLineWithIndent(lineStart, indent)
	else
		" middle of block
		return NextLineWithSmallerIndent(lineStart, indent)
	endif
	
	return blockEnd

endfunction


"function! FirstWord(lineNum)
"
"	let lineWords = split(getline(a:lineNum))
"	
"	if len(lineWords) > 0 
"		return lineWords[0] 
"	else
"		return ""
"	endif
"
"endfunction


"function! BlockType(lineNum)
"
"	return FirstWord(StartOfBlock(a:lineNum))
"	
"endfunction


"function! BlockFormat(lineNum)
"
"	let type = BlockType(a:lineNum)
"
"	" we will hopefully end up with more than just table...
"	if type == "table"
"		call FormatTable(StartOfBlock(a:lineNum), EndOfBlock(a:lineNum))
"	elseif type == "var"
"		call FormatVar(StartOfBlock(a:lineNum), EndOfBlock(a:lineNum))
"	endif
"
"endfunction


function! MinAlignmentPos(lineStr, alignText)

	let pos = match(a:lineStr, " *" . a:alignText)

	" if the search string does not already contain a leading space, 
	" leave room for one
	if strpart(a:alignText, 0, 1) != " " && pos > -1
		return pos + 1
	else
		return pos
	endif

endfunction


function! AlignmentPos(lst, alignText)

	return max(Fmap(a:lst, 'MinAlignmentPos(v:val, "' . a:alignText . '")'))

endfunction


function! AlignLine(lineStr, alignText, toPos)

	let pos = stridx(a:lineStr, a:alignText)

	if pos == -1 || pos == a:toPos
		return a:lineStr
	elseif pos < a:toPos
		" push the alignment text out
		return strpart(a:lineStr, 0, pos) . Spaces(a:toPos - pos) . strpart(a:lineStr, pos)
	else
		" reel in the alignment text
		return strpart(a:lineStr, 0, a:toPos) . strpart(a:lineStr, pos)
	endif

endfunction


" possible incorporate AlignmentPos into this fn if there is no need
" for it otherwise
function! AlignList(lst, alignText)

	let pos = max(Fmap(a:lst, 'MinAlignmentPos(v:val, "' . a:alignText . '")'))
	return Fmap(a:lst, 'AlignLine(v:val, "' . a:alignText . '", ' . pos . ')')

endfunction


" global defs are at the beginning of the file except for proc and func
" all start with lc apart from proc and func
"function! FindDef(identifier)
"
"	if a:identifier =~ "^[a-z]"
"		return FindDataDef(a:identifier)
"	elseif a:identifier =~ "^[A-Z]"
"		return FindFuncDef(a:identifier)
"	else
"		return 0 " not found
"	endif
"
"endfunction


"function! FindFuncDef(identifier)
"
"	let lineNum = 1
"	let lineMatch = 0
"
"	while lineNum <= line("$")
"		let lineText = getline(lineNum)
"		" starts proc or func; followed by any amount of whitespace;
"		" followed by a possible identifier ending with literal ".";
"		" followed by our search identifier; followed by whitespace,
"		" "(" or eol
"		if lineText =~# '^\(proc\|func\) \+\([a-zA-Z0-9|_.]*\.\)\?' . a:identifier . '\([ (]\|$\)'
"			let lineMatch = lineNum
"			break
"		endif
"		let lineNum += 1
"	endwhile
"
"	return lineMatch
"
"endfunction

" types of data def: table, var, const, type - only search for global
" for var, const, type things get a bit more complicated as they can be
" defined in single line definitions or in blocks
"function! FindDataDef(identifier)
"
"	let lineNum = 1
"	let lineMatch = 0
"	let inDefBlock = 0
"
"	while lineNum <= line("$")
"		let lineText = getline(lineNum)
"		" block start followed by eol or comment
"		if lineText =~# '^\(var\|type\|const\|input\) *\(;\|$\)'
"			let inDefBlock = 1
"		" end followed by eol or comment
"		elseif lineText =~# '^end *\(;\|$\)'
"			let inDefBlock = 0
"		" definition command followed by whitespace followed by our
"		" identifier followed by whitespace, equals or eol
"		elseif (! inDefBlock) && lineText =~# '^\(var\|type\|const\|table\) *' . a:identifier . '\([ =]\|$\)'
"			let lineMatch = lineNum
"			break
"		" our indented identifier followed by whitespace, colon,
"		" equals or eol
"		elseif inDefBlock && lineText =~# '^  ' . a:identifier . '\([ =:]\|$\)'
"			let lineMatch = lineNum
"			break
"		endif
"		"if lineText =~# '^\(proc\|func\) \+\([a-zA-Z0-9|_.]*\.\)\?' . a:identifier . '\([ (]\|$\)'
"		let lineNum += 1
"	endwhile
"
"	return lineMatch
"
"endfunction


"function! GotoDef(identifier)
"	
"	let gotoLine = FindDef(a:identifier)
"	if gotoLine && gotoLine != line(".")
"		" add current position to jumplist first
"		execute "normal" "m'"
"		" now go there
"		execute ':' . gotoLine
"	endif
"
"endfunction


" FOLDING

" calculate indent level for given line
function! IndentLevel(lineNum)

	return IndentedBy(a:lineNum) / b:custom_indent

endfunction


" Is given line blank?
function! IsBlankLine(lineNum)

	return getline(a:lineNum) =~ "^ *$"

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


" this is slow...
function! NewFoldLevel(lineNum)

	return IndentLevel(StartOfBlock(a:lineNum)) + 1

endfunction


" this is ugly but fast...
" calculate fold level for given line
function! FoldLevel(lineNum)

	let foldLevel = 0	

	if IsBlankLine(a:lineNum)
		" for a blank line, set the fold level to the indent level
		" of the previous non-blank line
		let prevLine = a:lineNum - 1
		while prevLine > 0 && IsBlankLine(prevLine)
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
		while nextLine <= lastLine && IsBlankLine(nextLine)
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

	let foldLevel = FoldLevel(a:lineNum)

	if IsEndStatement(a:lineNum)
		return "<" . foldLevel
	else
		return foldLevel
	endif
	
endfunction


" KEY MAPS

" ctrl b in insert mode to create a new block
inoremap <C-B> <C-\><C-O>:call CreateBlock()<CR>
" the b character simply holds the current indentation level, otherwise the
" blank line would be collapsed down after <cr>
"inoremap <C-B> <CR>b<CR>end<up><bs><space><space>

" F9 to compile
"inoremap <f9> <C-O>:call QCompile("ecstract3",function("PreSaveIndent"),function("PostSaveIndent"))<CR>
"noremap <f9> :call QCompile("ecstract3",function("PreSaveIndent"),function("PostSaveIndent"))<CR>

" F8 to compile using experimental json compiler
"inoremap <f8> <C-O>:call JsonCompile("ecstract3",function("PreSaveIndent"),function("PostSaveIndent"))<CR>
"noremap <f8> :call JsonCompile("ecstract3",function("PreSaveIndent"),function("PostSaveIndent"))<CR>

" F7 to compile using experimental config file driven compile
inoremap <f7> <C-O>:call ExpSelectCompile()<CR>
noremap <f7> :call ExpSelectCompile()<CR>

" F2 language help for word currently under cursor
"noremap <F2> :call LangHelp("<C-R><C-W>")<CR>
"inoremap <F2> <C-O>:call LangHelp("<C-R><C-W>")<CR>
"inoremap <F2> <ESC>:call LangHelp("<C-R><C-W>")<CR>li

" These toggles need to be more clever as if we are commenting out code with
" various different indentation levels, the comment indentation needs to be
" set according to the first line
" toggle ';' comment
"noremap ;; :call ToggleComment(";")<CR>

" toggle '#' comment
"noremap ## :call ToggleComment("#")<CR>

" use language aware formatting tool
"nnoremap <Leader>f :call BlockFormat(line("."))<CR>

"nnoremap <leader>d : call GotoDef(expand('<cword>'))<CR>
