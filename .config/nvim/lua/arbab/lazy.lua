-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
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
-- Load Plugins
require("lazy").setup({
      { import = "arbab.plugins" },
   },
   -- Telescope
   {
      'nvim-telescope/telescope.nvim',
      dependencies = {
         'nvim-lua/plenary.nvim',
         "BurntSushi/ripgrep",
         "sharkdp/fd",
         "nvim-tree/nvim-web-devicons",
      },
      -- Telescope Extensions
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
   },
   {
      install = {
         colorscheme = { "nordic" },
      },
      checker = {
         enabled = true,
         notify = false,
      },
      change_detection = {
         enabled = true,
         notify = false,
      }
   }
)
