" Vim plugin that shows tip text in response to keystrokes 
" Maintainer: Paul Marshall <getpaulhere@gmail.com>

if exists("g:loaded_cuetip")
    finish
endif
let g:loaded_cuetip = 1

let s:save_cpo = &cpo
set cpo&vim


" unsupported key sequences
" Fixme: the rest of these need to be discovered...
" Fixme: do we need to exclude leader key? will that mess things up?
let s:unsupported = {'n' : ['o', 'O', 'gi', 'gI', 'i', 'I', 'a', 'A', 's', 'S'],
                  \  'i' : [],
                  \  'v' : []}


function s:ShowTip(tip)
    if exists("s:enable_cuetip")
        redraw
        echohl Title
        echon "Tip: " . a:tip
        echohl None
    endif
endfunction


function s:DQuoteEscape(str)
    return substitute(a:str, '"', '\\"', 'g')
endfunction


function s:SQuoteEscape(str)
    return substitute(a:str, "'", "''", 'g')
endfunction


" in normal mode, we have to cater for count prefixes
function s:ShowNormalTip(tip, keySeq)
    call s:ShowTip(a:tip)
    if v:count
        execute 'execute "normal! ' . v:count . s:DQuoteEscape(a:keySeq) . '"'
    else
        " on version 7.4, this works as expected:
        " execute "normal! \<left>"
        " whereas this doesn't:
        " let foo='\<left>'
        " execute "normal! " . foo
        " hence we pass a fully formed execute string to execute
        execute 'execute "normal! ' . s:DQuoteEscape(a:keySeq) . '"'
    endif
endfunction


" in visual mode, we have to cater for count prefixes and maintain the
" selection
function s:ShowVisualTip(tip, keySeq)
    call s:ShowTip(a:tip)
    if v:count
        execute 'execute "normal! gv' . v:count . s:DQuoteEscape(a:keySeq) . '"'
    else
        execute 'execute "normal! gv' . s:DQuoteEscape(a:keySeq) . '"'
    endif
endfunction


function s:Clear()
    if exists("s:enable_cuetip")
        echo ""
    endif
endfunction


function s:EnableTips()
    let s:enable_cuetip = 1
endfunction


function s:DisableTips()
    unlet s:enable_cuetip
endfunction


function s:ShowError(msg)
    echohl Error
    echomsg a:msg
    echohl None
endfunction


function s:ModeName(mode)
    if a:mode == "i"
        return "insert"
    elseif a:mode == "n"
        return "normal"
    elseif a:mode == "v"
        return "visual"
    else
        return "unknown"
    endif
endfunction


function s:Supported(modes, keySeq)
    for md in ["n", "i", "v"]
        if a:modes =~? md && index(s:unsupported[md], a:keySeq) > -1
            call s:ShowError("cuetip#add: key sequence '" . a:keySeq .
              \ "' is not currently supported in " . s:ModeName(md) . " mode")
            return 0  " unsuppoted
        endif
    endfor

    return 1  " supported
endfunction


function s:NewMaps(modes, keySeq)
    " Fixme: how to check abberviations?
    for md in ["n", "i", "v"]
        if a:modes =~? md && mapcheck(a:keySeq, md) != ""
            call s:ShowError("cuetip#add: key sequence '" . a:keySeq .
              \ "' is already mapped for " . s:ModeName(md) . " mode")
            return 0  " map(s) already exist
        endif
    endfor

    return 1  " proposed maps are new
endfunction


function s:ValidTip(modes, keySeq)
    " we allow blank tip text
    if a:modes =~? '\m[^niva]'
        call s:ShowError("cuetip#add: Invalid mode specification: " . a:modes)
    elseif a:keySeq == ""
        call s:ShowError("cuetip#add: No key sequence specified")
    elseif s:Supported(a:modes, a:keySeq) && s:NewMaps(a:modes, a:keySeq)
        return 1  " valid 
    endif

    return 0  " invalid
endfunction


function s:ResultsInInsert(keySeq)
    " normal mode key sequences that result in a switch to insert mode
    " Fixme: the rest of these need to be discovered...
    return a:keySeq ==? 'i' || a:keySeq ==? 'a' || a:keySeq ==# 'gi' ||
      \ a:keySeq ==# 'gI' || a:keySeq[0] ==? 'c' || a:keySeq ==? 's' ||
      \ a:keySeq ==? 'o'
endfunction


" if we try to generate a map with a function call with say, '\<left>'
" embedded as an argument, it will look ok in the map listing but will
" execute as though that key had been pressed at that point in the command
" hence this rather clumsy escaping mechanism: '\<' . 'left>'
" there is probably a much better way to do it...
function s:KeyArgEscape(keySeq)
    let ks = s:SQuoteEscape(a:keySeq)
    " Fixme: there are other chars that can go into a <..> sequence that
    "        have not been allowed for here
    return "'" .
      \ substitute(ks, '<\(\a[a-zA-Z0-9-]*>\)', '\\<'' . ''\1', 'g') .
      \ "'"
endfunction


function! cuetip#add(modes, keySeq, tip)
    if s:ValidTip(a:modes, a:keySeq)
        let tipArg = s:SQuoteEscape(a:tip)
        if a:modes =~? "n"
            " normal mode tip
            if s:ResultsInInsert(a:keySeq)
                " normal mode keys that result in insert mode being entered
                " need to be dealt with differently, we do not cater for
                " counts here
                execute "nnoremap <silent>" a:keySeq
                  \ ":call <SID>ShowTip('" . tipArg . "')<CR>" . a:keySeq
            else
                execute "nnoremap <silent>" a:keySeq
                  \ ":<C-U>call <SID>ShowNormalTip('" . tipArg . "', " .
                  \ s:KeyArgEscape(a:keySeq) . ")<CR>"
            endif
        endif
        if a:modes =~? "i"
            " insert mode tip
            execute "inoremap <silent>" a:keySeq
              \ "<C-O>:call <SID>ShowTip('" . tipArg . "')<CR>" . a:keySeq
        endif
        if a:modes =~? "v"
            " visual mode tip
            execute "vnoremap <silent>" a:keySeq
              \ ":<C-U>call <SID>ShowVisualTip('" . tipArg . "', " .
              \ s:KeyArgEscape(a:keySeq) . ")<CR>"
        endif
        if a:modes =~? "a"
            " inserted abbreviation tip
            execute "inoreabbrev <silent>" a:keySeq
              \   "<C-O>:call <SID>ShowTip('" . tipArg . "')<CR>" . a:keySeq
        endif
    endif
endfunction


function cuetip#enable()
    call s:EnableTips()
endfunction


" Expose other script functions
if !exists(":EnableTips")
    command -nargs=0 EnableTips :call s:EnableTips()
endif
if !exists(":DisableTips")
    command -nargs=0 DisableTips :call s:DisableTips()
endif
noremap <unique> <Plug>TipClear :call <SID>Clear()<CR>
if !hasmapto("<Plug>TipClear")
    nmap <unique> <Leader>c <Plug>TipClear
endif

" restore user's cpoptions
let &cpo = s:save_cpo
unlet s:save_cpo
