vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = false
vim.opt.cindent = false
vim.opt.autoindent = false
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.updatetime = 300
vim.opt.signcolumn = "yes"

-- plug
local Plug = vim.fn['plug#']
vim.call('plug#begin')
Plug('neoclide/coc.nvim', { ['branch'] = 'release' })
Plug('ibhagwan/fzf-lua', { ['branch'] = 'main' })
Plug('nvim-treesitter/nvim-treesitter', { ['do'] = function()
  local tsupdate = require('nvim-treesitter.install').update({ with_sync = true })
  tsupdate()
end })
Plug('rebelot/kanagawa.nvim')
vim.call('plug#end')

-- coc
function _G.check_back_space()
  local col = vim.fn.col('.') - 1
  return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

local keyset = vim.keymap.set
local opts = {silent = true, noremap = true, expr = true, replace_keycodes = false}
keyset("i", "<CR>", [[coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"]], opts)
keyset("n", "gd", "<Plug>(coc-definition)", {silent = true})
keyset("n", "gy", "<Plug>(coc-type-definition)", {silent = true})
keyset("n", "gi", "<Plug>(coc-implementation)", {silent = true})
keyset("n", "gr", "<Plug>(coc-references)", {silent = true})
keyset("n", "<leader>2", "<Plug>(coc-rename)", {silent = true})

function _G.show_docs()
  local cw = vim.fn.expand('<cword>')
  if vim.fn.index({'vim', 'help'}, vim.bo.filetype) >= 0 then
    vim.api.nvim_command('h ' .. cw)
  elseif vim.api.nvim_eval('coc#rpc#ready()') then
    vim.fn.CocActionAsync('doHover')
  else
    vim.api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
  end
end
keyset("n", "K", '<CMD>lua _G.show_docs()<CR>', {silent = true})

-- fzf
keyset('n', '<leader>f', "<CMD>lua require('fzf-lua').files()<CR>", {silent = true})

-- nvim-treesitter
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

vim.cmd('silent! colorscheme kanagawa-wave')
