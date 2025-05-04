local vim = vim

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true

-- lazy setup
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- plug
require 'lazy'.setup({
  'neovim/nvim-lspconfig',
  'hrsh7th/nvim-cmp',
  'hrsh7th/cmp-buffer',
  'hrsh7th/cmp-nvim-lsp',
  'hrsh7th/cmp-path',
  'nvim-lua/plenary.nvim',
  'williamboman/mason.nvim',
  'williamboman/mason-lspconfig.nvim',
  { 'ibhagwan/fzf-lua', branch = 'main' },
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  'catppuccin/nvim'})

  -- fzf
  local keyset = vim.keymap.set
  keyset('n', '<leader>f', "<CMD>lua require('fzf-lua').files()<CR>", {silent = true})

  -- nvim-treesitter
  require 'nvim-treesitter.configs'.setup {
    auto_install = true,
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = false
    }
  }

  -- nvim-cmp
  local cmp = require 'cmp'
  cmp.setup({
    snippet = {
      expand = { function(args) vim.snippet.expand(args.body) end }
    },
    mapping = cmp.mapping.preset.insert({
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'buffer' },
    }),
  })
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' },
    }),
    matching = { disallow_symbol_nonprefix_matching = false }
  })
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'buffer' },
    }),
  })

  local capabilities = require 'cmp_nvim_lsp'.default_capabilities()
  require 'lspconfig'['clangd'].setup {
    capabilities = capabilities
  }
  require 'lspconfig'['lua_ls'].setup {
    capabilities = capabilities
  }

  -- mason and mason-lspconfig
  require 'mason'.setup()
  require 'mason-lspconfig'.setup()

  -- colorscheme
  vim.cmd('silent! colorscheme catppuccin-mocha')
