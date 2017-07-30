" compile functions

function! TruncatePath(pathName)

	let pathLen = strlen(a:pathName)
	let idx = strridx(a:pathName, '/', pathLen - 2)
	if idx == -1
		let idx = strridx(a:pathName, '\', pathLen - 2)
	endif
	if idx == -1
		return ''
	else
		return strpart(a:pathName, 0, idx + 1)
	end

endfunction


function! ExpSelectCompile()
	let fileName = expand('%:p')
	let optFiles = CompileOptionFiles(fileName)
	let numOpts  = len(optFiles)

	if numOpts != 0
		" if we only have the standard compile options file, go ahead
		" and compile using it
		if numOpts == 1
			call ExpCompile(optFiles[0])
		else
			call DisplayCompileOptions(optFiles, numOpts)
			for comp in GetCompileList(numOpts)
				call ExpCompile(optFiles[comp])
			endfor
		endif
	else
		echo 'Compile options file not found'
	endif
endfunction


function! DisplayCompileOptions(optFiles, numOpts)
	let i = 0
	while i < a:numOpts
		if (i == 0)
			echo 'S - ' . a:optFiles[0]
		else
			echo i . ' - ' . a:optFiles[i]
		endif
		let i = i + 1
	endwhile
	echo 'A - All but standard'
endfunction


function! GetCompileList(numOpts)
	let badCompileList = [-1]
	let toCompile = badCompileList

	while toCompile == badCompileList
		call inputsave()
		let compileChoice = input('Choose: ')
		call inputrestore()
		let toCompile = StringToCompileList(compileChoice, a:numOpts)
		echo " "
		if toCompile == badCompileList
			echo "Invalid input, choose S, A or 1 thru " . (a:numOpts - 1)
		endif
	endwhile

	return toCompile
endfunc


function! StringToCompileList(str, numOpts)
	let badCompileList = [-1]
	let toCompile = []

	if a:str == ''
		let toCompile = []
	elseif a:str == 'S' || a:str == 's'
		let toCompile = [0]
	elseif a:str == 'A' || a:str == 'a'
		let i = 1
		while i < a:numOpts
			call add(toCompile, i)
			let i = i + 1
		endwhile
	else
		for opt in split(a:str, ",")
			if opt =~# '^\d\+$' && str2nr(opt) > 0 && str2nr(opt) < a:numOpts
				call add(toCompile, str2nr(opt))
			else
				let toCompile = badCompileList
				break
			endif
		endfor
	endif

	return toCompile
endfunc


function! CompileOptionFiles(currentFile)
	" settings
	let stdOptionsFile = "compile_options"
	let stdOptionsExt = ".txt"

	let seachDir = ''
	let optFiles = []

	if a:currentFile != ""
		let searchDir = TruncatePath(a:currentFile)
		while searchDir != '' && ! filereadable(searchDir . stdOptionsFile . stdOptionsExt)
			let searchDir = TruncatePath(searchDir)
		endwhile
	endif
	if searchDir != ''
		" we are relying here on the standard options file being first
		" in the list (i.e. where * matches '')
		let optFiles = glob(searchDir . stdOptionsFile . '*' . stdOptionsExt, 0, 1)
	endif

	return optFiles
endfunction


function! ExpCompile(optionsFile)
	" settings
	let compileExe = 'c:\ecomp\exp_compile.exe'
	let tempFile = 'c:\vimtemp\compile.txt'
	let compileOutput = 'c:\ecomp\compiler_output.txt'

	let fileName = expand('%:p')
	"let saveCursor = getpos(".")

	if fileName != ""
		" save the file
		if &modified
			"call a:preSaveFn()
			echo ""
			w
			"call a:postSaveFn()
		endif
		let compileCmd = compileExe . ' ' . a:optionsFile . ' "' .
\			fileName . '" ' . compileOutput

		" we don't simply collect results from stdout because of
		" troublesome intermittant vim problem whereby capturing the
		" output of a system command does not always work
		call writefile([''], compileOutput)
		echo 'Running: ' . compileCmd
		execute 'silent ! ' . compileCmd . ' > ' . tempFile . ' 2>&1'

		" display resulting output
		echohl WarningMsg
		for line in readfile(tempFile)
			echo line
		endfor
		echohl Comment
		for line in readfile(compileOutput)
			echo line
		endfor
		"echo substitute(messages, "\n\n", "\n", "g")
		echohl None
	else
		echo "File has not been saved yet"
	endif

	" put cursor back where we found it
	" call setpos(".", saveCursor)
endfunction
