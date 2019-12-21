call plug#begin('~/.config/nvim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-github-dashboard'
Plug '/usr/local/opt/fzf'
Plug 'neoclide/coc.nvim', {'do': './install.sh nightly'}
Plug 'liuchengxu/eleline.vim'
Plug 'let-def/vimbufsync'
Plug 'whonore/coqtail'
call plug#end()

nnoremap <leader>f :FZF<cr>
nnoremap <leader>cd :lcd %:h<cr>
nnoremap gev :e $MYVIMRC<cr>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
