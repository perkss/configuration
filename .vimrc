" turn on syntax color
syntax on

" Use spaces for indenting
set expandtab

" turn off line wraping
set nowrap

" tab size
set tabstop=4

" size of an indent
set shiftwidth=4

" auto indenting
set smartindent

" case insensitive searching by default
set ignorecase smartcase

set number              " show line numbers

set showcmd             " show command in bottom bar

set cursorline          " highlight current line

set wildmenu            " visual autocomplete for command menu

set incsearch           " search as characters are entered
set hlsearch            " highlight matches

" move to beginning/end of line

let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'


