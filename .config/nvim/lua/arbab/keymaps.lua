local map = vim.keymap.set
-- Cycle BufferS
map('n', 'gt', ':bnext<CR>')
map('n', 'gT', ':bprev<CR>')
map('v', 'gt', ':bnext<CR>')
map('v', 'gT', ':bprev<CR>')

-- Kill Buffer
map('n', '<Leader>xq', ':bd<CR>')
