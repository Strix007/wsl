return {
   "mbbill/undotree",
   config = function ()
      vim.keymap.set("n", "<C-x>u", vim.cmd.UndotreeToggle)
   end,
}
