local map = vim.keymap.set
-- Cycle BufferS
map('n', 'gt', ':bnext<CR>', { noremap = true, silent = true, nowait = true })
map('n', 'gT', ':bprev<CR>', { noremap = true, silent = true, nowait = true })

-- Open link under cursor
map("n", "gx", [[:silent execute '!$BROWSER ' . shellescape(expand('<cfile>'), 1)<CR>]], opts)

-- Kill Buffer
map('n', '<Leader>xq', ':bd<CR>', { noremap = true, silent = true, nowait = true })

-- Telescope
map('n', '<Leader>ff', ":lua require'telescope.builtin'.find_files({ hidden = true })<CR>")
map('n', '<leader>fg', ":lua require'telescope.builtin'.live_grep{}<CR>")
map('n', '<leader>xb', ":lua require'telescope.builtin'.buffers{}<CR>")
map('n', '<leader>fh', ":lua require'telescope.builtin'.help_tags{}<CR>")
