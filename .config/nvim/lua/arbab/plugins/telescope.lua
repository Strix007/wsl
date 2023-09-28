return {
   'nvim-telescope/telescope.nvim',
   dependencies = {
      'nvim-lua/plenary.nvim',
      {
         'nvim-telescope/telescope-fzf-native.nvim',
         build = 'make',
         cond = function()
            return vim.fn.executable 'make' == 1
         end,
      },
      config = function()
         require('telescope').setup {
            extensions = {
               fzf = {
                  fuzzy = true,                   -- false will only do exact matching
                  override_generic_sorter = true, -- override the generic sorter
                  override_file_sorter = true,    -- override the file sorter
                  case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
                  -- the default case_mode is "smart_case"
               },
               require 'telescope'.setup {
                  extensions = {
                     media_files = {
                        filetypes = { "png", "webp", "jpg", "jpeg", "jpg", "mp4", "mpv", "webm", "pdf" },
                        find_cmd = "rg"
                     }
                  },
               }
            },
            defaults = {
               vimgrep_arguments = {
                  'rg',
                  '--hidden',
                  '--color=never',
                  '--no-heading',
                  '--with-filename',
                  '--line-number',
                  '--column',
                  '--smart-case',
               },
            },
         }
         local builtin = require('telescope.builtin')
         vim.api.nvim_set_keymap('n', '<Leader>ff', ':lua require"telescope.builtin".find_files({ hidden = true })<CR>',
            { noremap = true, silent = true })
         vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
         vim.keymap.set('n', '<leader>xb', builtin.buffers, {})
         vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
         -- Extensions
         require('telescope').load_extension('fzf')
      end
   },
}
