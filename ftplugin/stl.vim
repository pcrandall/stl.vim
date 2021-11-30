if (exists('b:did_ftplugin'))
  finish
endif
let b:did_ftplugin = 1

setlocal formatoptions-=t
setlocal foldmethod=indent

" Define comments string
setlocal comments=
setlocal commentstring=(*%s*)

" Enable automatic comment insertion
setlocal formatoptions+=cro

setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
