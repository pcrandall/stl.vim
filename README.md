# stl.vim (DONT USE THIS)

Vim mode for editing siemens stl source code files

# Installation

To install with vim/neovim [plug-vim](https://github.com/junegunn/vim-plug) add following line in your configuration file:

```
" Vim-Plug
call plug#begin()
  Plug 'pcrandall/stl.vim', { 'for': 'stl' }
call plug#end()

...

autocmd BufNewFile,BufRead *.AWL setf stl
```


