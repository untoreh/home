:imap fd <Esc>
:let mapleader=","


" Integrate with whichkey for spacemacs-style space key
nnoremap <space> :call VSCodeNotify('vspacecode.space')<CR>
xmap gc  <Plug>VSCodeCommentary

nmap gc  <Plug>VSCodeCommentary
omap gc  <Plug>VSCodeCommentary
nmap gcc <Plug>VSCodeCommentaryLine


if exists('g:vscode')
	xmap <M-;>  <Plug>VSCodeCommentary
	nmap <M-;>  <Plug>VSCodeCommentary
	omap <M-;>  <Plug>VSCodeCommentary
	" wait for vscode-neovim extension v > 0.0.82
 	autocmd CursorMoved * :call VSCodeNotifyVisual('editor.action.selectFromAnchorToCursor', 1)
else
	xmap <M-;>  <Plug>NERDCommenterToggle
	nmap <M-;>  <Plug>NERDCommenterToggle
	omap <M-;>  <Plug>NERDCommenterToggle
endif

let g:wordmotion_nomap = 1
nmap w          <Plug>WordMotion_w
nmap b          <Plug>WordMotion_b
nmap gE         <Plug>WordMotion_gE
omap aW         <Plug>WordMotion_aW
cmap <C-R><C-W> <Plug>WordMotion_<C-R><C-W>

call plug#begin('~/.vim/plugged')

Plug 'neomake/neomake'

" editing
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdcommenter'
Plug 'easymotion/vim-easymotion'
Plug 'chaoren/vim-wordmotion'

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
