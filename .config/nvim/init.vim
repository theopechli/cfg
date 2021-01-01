set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum
set termguicolors
filetype plugin on
filetype indent on
syntax enable
set encoding=utf8
" set relativenumber
set number
set linebreak
set cpo+=n
set wrap
set showbreak=↳\ \ \
set textwidth=80
set formatoptions+=cqt
set showmatch
set visualbell
" set cursorline
set hlsearch
set smartcase
set ignorecase
set incsearch
set autoindent
set expandtab
set shiftwidth=4
set smartindent
set smarttab
set softtabstop=4
set showcmd
set ruler
set undolevels=1000
set backspace=indent,eol,start
set list listchars=tab:»·,trail:·
set mouse=a
set colorcolumn=80
set background=dark
colorscheme koehler

set path+=**
set wildmenu
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
" let g:netrw_list_hide=netrw_gitignore#Hide()
" let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

let mapleader=","

nnoremap <Leader>b :buffers<CR>:buffer<Space>
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") .
\       '> trans<' . synIDattr(synID(line("."),col("."),0),"name") .
\       "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Commenting blocks of code.
augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
  autocmd FileType sh,ruby,python   let b:comment_leader = '# '
  autocmd FileType conf,fstab       let b:comment_leader = '# '
  autocmd FileType tex              let b:comment_leader = '% '
  autocmd FileType mail             let b:comment_leader = '> '
  autocmd FileType vim              let b:comment_leader = '" '
augroup END
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>
