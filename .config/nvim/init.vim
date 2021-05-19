set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum
set t_Co=256
set termguicolors
set background=dark
colorscheme gruvbox
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
" set textwidth=80
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
" set colorcolumn=80
au ColorScheme * hi Normal ctermbg=none guibg=none

set path+=**
set wildmenu
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3
" let g:netrw_list_hide=netrw_gitignore#Hide()
" let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'

let mapleader=","

" Switch buffers
nnoremap <Leader>b :buffers<CR>:buffer<Space>

" Commenting blocks of code.
augroup commenting_blocks_of_code
  autocmd!
  autocmd FileType c,cpp,java,scala let b:comment_leader = '// '
  autocmd FileType sh,ruby,python   let b:comment_leader = '# '
  autocmd FileType conf,fstab       let b:comment_leader = '# '
  autocmd FileType tex              let b:comment_leader = '% '
  autocmd FileType mail             let b:comment_leader = '> '
  autocmd FileType vim              let b:comment_leader = '" '
  autocmd FileType lua              let b:comment_leader = '-- '
augroup END
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

autocmd FileType html setlocal shiftwidth=2 softtabstop=2 expandtab

" By default, use spaced tabs.
set expandtab

" Display tabs as 4 spaces wide. When expandtab is set, use 4 spaces.
set shiftwidth=4
set tabstop=4

function TabsOrSpaces()
    " Determines whether to use spaces or tabs on the current buffer.
    if getfsize(bufname("%")) > 256000
        " File is very large, just use the default.
        return
    endif

    let numTabs=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^\\t"'))
    let numSpaces=len(filter(getbufline(bufname("%"), 1, 250), 'v:val =~ "^ "'))

    if numTabs > numSpaces
        setlocal noexpandtab
    endif
endfunction

" Call the function after opening a buffer
autocmd BufReadPost * call TabsOrSpaces()

function! Preserve(command)
  " Preparation: save window state
  let l:saved_winview = winsaveview()
  " Run the command:
  execute a:command
  " Clean up: restore previous window position
  call winrestview(l:saved_winview)
endfunction

nnoremap <Leader>cw :call Preserve("%s/\\s\\+$//e")<CR>
xnoremap <Leader>cw :call Preserve("'<,'>s/\\s\\+$//e")<CR>


if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif
