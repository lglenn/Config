" Line numbers in gutter
set number

"
" Tabbing
set tabstop=2
" Insert tabstop spaces instead of a tab (ctrl-V-tab for tab)
set expandtab
" Indent by this many spaces when doing autoindent
set shiftwidth=4

" Solarized theme
syntax enable
set background=dark
colorscheme solarized

" Soft wrap
set wrap linebreak textwidth=0

" Map window movement keys to ctrl-cursor
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Don't complain when moving away from a dirty buffer, just create 
" a hidden one
set hidden
