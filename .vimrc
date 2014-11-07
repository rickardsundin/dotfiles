" Required for this vimrc to work
"  - pathogen (plugin management)
"  - solarized (color scheme)
"  - fugitive (git wrapper)
"
" mkdir -p ~/.vim/autoload ~/.vim/bundle
" curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
" cd ~/.vim/bundle
" git clone git://github.com/altercation/vim-colors-solarized.git
" git clone git://github.com/tpope/vim-fugitive.git

" Enable Pathogene
execute pathogen#infect()
syntax on
filetype plugin indent on

" Colors
syntax enable
set background=dark
colorscheme solarized

" Enable "hybrid" line numbers
set relativenumber
set number

" Indentation
set expandtab               " turns <TAB>s into spaces
set shiftwidth=2
set softtabstop=2           " number of spaces in tab when editing

" Statusline
set statusline=%c           " column number
set statusline+=\ %.100f    " filename, left aligned, 100 characters max
set statusline+=%m          " display modified flag
set statusline+=%=          " right align below here
set statusline+=%{fugitive#statusline()}  "Indicate the current git branch

set laststatus=2            "Always display statusline
