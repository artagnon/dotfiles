call plug#begin('~/.config/nvim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/vim-github-dashboard'
Plug '/usr/local/opt/fzf'
Plug 'neoclide/coc.nvim', {'branch': 'master', 'do': 'yarn install --frozen-lockfile'}
Plug 'liuchengxu/eleline.vim'
Plug 'let-def/vimbufsync'
Plug 'whonore/coqtail'
Plug 'RishabhRD/popfix'
Plug 'RishabhRD/nvim-finder'
Plug 'EdenEast/nightfox.nvim', {'branch': 'main'}
Plug 'ibhagwan/fzf-lua', {'branch': 'main'}
Plug 'nvim-tree/nvim-web-devicons'
Plug 'nvim-lualine/lualine.nvim'
call plug#end()

set termguicolors
lua require('lualine').setup()

nnoremap <leader>f :FzfLua files<cr>
nnoremap gev :e $MYVIMRC<cr>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
colorscheme carbonfox
