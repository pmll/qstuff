" Vim support file to detect file types
"

function! LooksLikeEcstract3()

	let saveCursor = getpos(".")
	let matchLine = search("^; \\[Ecstract 3\\] *$") > 0
	call setpos(".", saveCursor)

	return matchLine > 0

endfunction


"augroup customfiletypes
"	autocmd!

" defs file
autocmd BufNewFile,BufRead *.defs setfiletype defs

" ecstract 3
autocmd BufNewFile,BufRead *.qe,*.qei setfiletype ecstract3
autocmd BufRead *.text if LooksLikeEcstract3() | setf ecstract3 | endif

" ecstract 2
autocmd BufNewFile,BufRead *.qe2,*.qe2i setfiletype ecstract2

" ems/lib files
autocmd BufNewFile,BufRead *.ems setfiletype ems
autocmd BufNewFile,BufRead *.lib setfiletype qlib

" report writer
autocmd BufNewFile,BufRead *.qr,*.qri setfiletype reportwriter

" qpascal
autocmd BufNewFile,BufRead *.qp,*.qpp setfiletype qpascal

" vim ecstract3 compiler helper file
autocmd BufNewFile,BufRead vime3compile.txt setfiletype vime3compile

" experimental compile options file
autocmd BufNewFile,BufRead compile_options*.txt setfiletype compile_options

"augroup END
