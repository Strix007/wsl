return {
   "NeogitOrg/neogit",
   dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
      "ibhagwan/fzf-lua",
   },
   config = function()
      require("neogit").setup()
      vim.api.nvim_set_keymap('n', '<C-x>g', '<cmd>Neogit<CR>', {noremap = true})
   end,
}
