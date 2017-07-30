" tabularize shortcut keys for use with ecstract
"
" ,te (=) ,ta (:=) ,tt (:) ,tq (')
" Visual mode uses GTabularize - ignores lines in range that do not
" contain pattern but aligns the rest to the same column
if exists(":Tabularize")
  nnoremap <Leader>te :Tabularize /=<CR>
  vnoremap <Leader>te :GTabularize /=<CR>
  nnoremap <Leader>ta :Tabularize /:=<CR>
  vnoremap <Leader>ta :GTabularize /:=<CR>
  nnoremap <Leader>tt :Tabularize /:[^=].*<CR>
  vnoremap <Leader>tt :GTabularize /:[^=].*<CR>
  nnoremap <Leader>tq :Tabularize /'.*<CR>
  vnoremap <Leader>tq :GTabularize /'.*<CR>
endif
