" vim script - specific to report writer files

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

source $VIM/vimfiles/qcommon/compile.vim
set lines=34
set columns=115

" KEY MAPS
" F7 to compile using experimental config file driven compile
inoremap <f7> <C-O>:call ExpSelectCompile()<CR>
noremap <f7> :call ExpSelectCompile()<CR>
