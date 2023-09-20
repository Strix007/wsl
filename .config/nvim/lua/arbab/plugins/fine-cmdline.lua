return {
   'VonHeikemen/fine-cmdline.nvim',
   dependencies = {
      { 'MunifTanjim/nui.nvim' }
   },
   config = function()
      vim.api.nvim_set_keymap('n', '<A-x>', '<cmd>FineCmdline<CR>', {noremap = true})
      vim.api.nvim_set_keymap('n', ':', '<cmd>FineCmdline<CR>', {noremap = true})
   end,
}
