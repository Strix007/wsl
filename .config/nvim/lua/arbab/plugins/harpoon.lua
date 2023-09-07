return {
   'ThePrimeagen/harpoon',
   config = function ()
      local mark = require("harpoon.mark")
      local ui = require("harpoon.ui")
      local vk = vim.keymap
      vk.set("n", "<leader>ha", mark.add_file)
      vk.set("n", "<leader>hf", ui.toggle_quick_menu)
      vk.set("n", "<leader>h1", function () ui.nav_file(1) end)
      vk.set("n", "<leader>h2", function () ui.nav_file(2) end)
      vk.set("n", "<leader>h3", function () ui.nav_file(3) end)
      vk.set("n", "<leader>h4", function () ui.nav_file(4) end)
   end,
}
