call plug#begin('~/.config/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ibhagwan/fzf-lua', {'branch': 'main'}
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'folke/tokyonight.nvim', {'branch': 'main'}
call plug#end()

set tabstop=2
set shiftwidth=2
set expandtab
set nosmartindent
set nocindent
set noautoindent

nnoremap <leader>f :FzfLua files<cr>
nnoremap gev :e $MYVIMRC<cr>

inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

:lua << EOF
require 'nvim-treesitter.configs'.setup {
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false
  },
  indent = {
    enable = true
  }
}
EOF

colorscheme tokyonight-night
