require('telescope').setup {
   extensions = {
      fzf = {
         fuzzy = true,
         override_generic_sorter = true,
         override_file_sorter = true,
         case_mode = "smart_case",
      },
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
