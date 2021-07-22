:imap fd <Esc>
:let mapleader=","

" Integrate with whichkey for spacemacs-style space key
nnoremap <space> :call VSCodeNotify('vspacecode.space')<CR>
xmap gc  <Plug>VSCodeCommentary
nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
"nmap gcc <Plug>VSCodeCommentaryLine

xmap <M-;>  <Plug>VSCodeCommentary
nmap <M-;>  <Plug>VSCodeCommentary
omap <M-;>  <Plug>VSCodeCommentary
" nmap gcc <Plug>VSCodeCommentaryLine



map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
map <silent> ge <Plug>CamelCaseMotion_ge
sunmap w
sunmap b
sunmap e
sunmap ge
 
call plug#begin('~/.vim/plugged')

Plug 'neomake/neomake'


" editing
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
Plug 'bkad/CamelCaseMotion'

" ui
Plug 'rbong/vim-crystalline'
function! StatusLine(...)
  return crystalline#mode() . crystalline#right_mode_sep('')
        \ . ' %f%h%w%m%r ' . crystalline#right_sep('', 'Fill') . '%='
        \ . crystalline#left_sep('', 'Fill') . ' %{&ft}[%{&fenc!=#""?&fenc:&enc}][%{&ff}] %l/%L %c%V %P '
endfunction
let g:crystalline_enable_sep = 1
let g:crystalline_statusline_fn = 'StatusLine'
let g:crystalline_theme = 'default'
set laststatus=2

Plug 'airblade/vim-gitgutter'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
set termguicolors
Plug 'norcalli/nvim-colorizer.lua'

"langs
Plug 'vim-scripts/haproxy'
au BufRead,BufNewFile haproxy* set ft=haproxy
Plug 'JuliaEditorSupport/julia-vim'

call plug#end()

set shortmess+=A
