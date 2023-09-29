return {
   'stevearc/oil.nvim',
   opts = {
      delete_to_trash = true,
      view_options = {
         show_hidden = true,
      },
   },
   dependencies = { "nvim-tree/nvim-web-devicons" },
   config = function ()
      vim.keymap.set("n", "<C-x><C-f>", "<CMD>Oil<CR>")
   end,
}
