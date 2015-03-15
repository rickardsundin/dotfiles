" Start with configuration for Vundle (plugin manager)
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" Keep Plugin commands between vundle#begin/end.
Plugin 'altercation/vim-colors-solarized' " color scheme
Plugin 'tpope/vim-fugitive'               " git wrapper

" plugins for clojure development
Plugin 'tpope/vim-classpath'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-fireplace'
Plugin 'paredit.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

" Enable Pathogene
syntax on

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

"Navigate easier between split windows
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" More natural split opening
set splitbelow
set splitright

