return {
   'nvim-telescope/telescope.nvim',
   dependencies = {
      'nvim-lua/plenary.nvim',
      "BurntSushi/ripgrep",
      "nvim-telescope/telescope-fzf-native.nvim",
      "sharkdp/fd",
      "nvim-tree/nvim-web-devicons",
   },
   config = function()
      require('telescope').setup {
         extensions = {
            fzf = {
               fuzzy = true,          -- false will only do exact matching
               override_generic_sorter = true, -- override the generic sorter
               override_file_sorter = true, -- override the file sorter
               case_mode = "smart_case", -- or "ignore_case" or "respect_case"
               -- the default case_mode is "smart_case"
            },
         } }
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
      vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
   end,
}
