" tests
" passes
call cuetip#add('nv', 'b', 'b')
call cuetip#add('nv', 'c', 'c')

" fails
"call cuetip#add('nv', 'd', 'd')
"call cuetip#add('nv', 'r', 'r')
"call cuetip#add('nv', 'y', 'y')



call cuetip#add('niv', "'", 'single quote')
call cuetip#add('a', "'", 'abbrev single quote')
call cuetip#add('niv', '"', 'double quote')
call cuetip#add('a', '"', 'abbrev double quote')

call cuetip#add('niv', '<left>', 'h in normal mode can also cursor left')
call cuetip#add('niv', '<right><right>', 'l in normal mode can also cursor right')
call cuetip#add('niv', '<up>', 'k in normal mode can also cursor up')
call cuetip#add('niv', '<down>', 'j in normal mode can also cursor down')
call cuetip#add('nv', 'e', 'e')
call cuetip#add('nv', 'f', 'f')   " could be dodgy
call cuetip#add('nv', 'g', 'g')   " could be dodgy
" g is bound, refer to vim - prob needs own section
call cuetip#add('nv', 'h', 'h')
call cuetip#add('nv', 'j', 'j')
call cuetip#add('nv', 'k', 'k')
call cuetip#add('nv', 'l', 'l')
call cuetip#add('nv', 'm', 'm')   " could be dodgy
call cuetip#add('nv', 'n', 'n')
call cuetip#add('n', 'p', 'p')
call cuetip#add('nv', 'q', 'q')   " could be dodgy
call cuetip#add('nv', 't', 't')   " could be dodgy
call cuetip#add('nv', 'u', 'u')   " could be dodgy
call cuetip#add('n', 'v', 'v')    " could be dodgy
call cuetip#add('nv', 'w', 'w')
call cuetip#add('nv', 'x', 'x')
call cuetip#add('nv', 'z', 'z')   " could be dodgy

call cuetip#add('nv', 'B', 'B')
call cuetip#add('nv', 'C', 'C')   " could be dodgy
call cuetip#add('nv', 'D', 'D')
call cuetip#add('nv', 'E', 'E')
call cuetip#add('nv', 'F', 'F')   " could be dodgy
call cuetip#add('nv', 'G', 'G')   " almost certainly doesn't work
call cuetip#add('nv', 'H', 'H')   " almost certainly doesn't work
call cuetip#add('nv', 'J', 'J')
call cuetip#add('nv', 'K', 'K')
call cuetip#add('nv', 'L', 'L')   " almost certainly doesn't work
call cuetip#add('nv', 'M', 'M')
call cuetip#add('nv', 'N', 'N')
call cuetip#add('nv', 'P', 'P')
call cuetip#add('nv', 'R', 'R')   " almost certainly doesn't work
call cuetip#add('nv', 'T', 'T')   " could be dodgy
call cuetip#add('nv', 'U', 'U')   " could be dodgy
call cuetip#add('nv', 'V', 'V')   " could be dodgy
call cuetip#add('nv', 'W', 'W')
call cuetip#add('nv', 'X', 'X')
call cuetip#add('nv', 'Y', 'Y')
call cuetip#add('nv', 'Z', 'Z')   " almost certainly doesn't work

call cuetip#add('nv', '0', '0')

call cuetip#add('nv', '<SPACE>', 'space')
call cuetip#add('nv', '~', '~')   " could be dodgy
call cuetip#add('nv', '!', '!')   " could be dodgy
call cuetip#add('nv', '@', '@')   " almost certainly doesn't work
call cuetip#add('nv', '*', '*')   " could be dodgy
call cuetip#add('nv', '#', '#')   " could be dodgy
call cuetip#add('nv', '$', '$')   " could be dodgy
call cuetip#add('nv', '%', '%')   " could be dodgy
call cuetip#add('nv', '^', '^')   " could be dodgy
call cuetip#add('nv', '&', '&')   " could be dodgy
call cuetip#add('nv', '(', '(')   " could be dodgy
call cuetip#add('nv', ')', ')')   " could be dodgy
"call cuetip#add('nv', '|', '|')   " could be dodgy
call cuetip#add('nv', '-', '-')   " could be dodgy
call cuetip#add('nv', '_', '_')   " could be dodgy
call cuetip#add('nv', '=', '=')   " almost certainly doesn't work
call cuetip#add('nv', '+', '+')
call cuetip#add('nv', '[', '[')
call cuetip#add('nv', ']', ']')
call cuetip#add('nv', '{', '{')
call cuetip#add('nv', '}', '}')
call cuetip#add('nv', ';', ';')
"call cuetip#add('nv', "'", "'")   " almost certainly doesn't work
call cuetip#add('nv', "`", "`")   " almost certainly doesn't work
"call cuetip#add('nv', ",", ",")
call cuetip#add('nv', ".", ".")   " almost certainly doesn't work
call cuetip#add('nv', ">", ">")   " almost certainly doesn't work
call cuetip#add('nv', "<", "<")   " almost certainly doesn't work
call cuetip#add('nv', '?', '?')

call cuetip#add('n', '<C-B>', 'C-B')
call cuetip#add('nv', '<C-D>', 'C-D')
call cuetip#add('n', '<C-E>', 'C-E')
call cuetip#add('n', '<C-F>', 'C-F')
call cuetip#add('n', '<C-G>', 'C-G')
call cuetip#add('nv', '<C-H>', 'C-H')
call cuetip#add('nv', '<C-J>', 'C-J')
call cuetip#add('nv', '<C-L>', 'C-L')
call cuetip#add('nv', '<C-M>', 'C-M')
call cuetip#add('n', '<C-N>', 'C-N')
call cuetip#add('n', '<C-P>', 'C-P')
call cuetip#add('n', '<C-T>', 'C-T')   " dodgy
call cuetip#add('nv', '<C-U>', 'C-U')
call cuetip#add('n', '<C-\>', 'C-\')
call cuetip#add('n', '<C-]>', 'C-]')
call cuetip#add('n', '<C-^>', 'C-^')

" change commands here
" delete commands here
" z fold commands
" y yank commands
