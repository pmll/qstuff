" cuetip examples

" general tips that might be placed in vimrc
"call cuetip#add('ni', '<left>', 'h in normal mode can also cursor left')
"call cuetip#add('ni', '<right>', 'l in normal mode can also cursor right')
"call cuetip#add('ni', '<up>', 'k in normal mode can also cursor up')
"call cuetip#add('ni', '<down>', 'j in normal mode can also cursor down')

"call cuetip#add('nv', 'h', 'b will move to the beginning of the previous word')
"call cuetip#add('nv', 'l', 'w will move to the next word')
call cuetip#add('nv', 'w', 'e will move to the end of the next word')
"call cuetip#add('n', 'cw', 'ciw will replace the whole word under the cursor')
"call cuetip#add('i', '#', 'never press that!')
"call cuetip#add('n', '<c-d>', 'hey ctrl d')
"call cuetip#add('i', '<BS>', 'normal mode J will join 2 lines')

" language specific tips that might belong in a ftplugin file
call cuetip#add('a', 'if', 'use ite to produce a full if-then-else skeleton')
call cuetip#add('a', 'switch', 'use sw to produce a full switch skeleton')

