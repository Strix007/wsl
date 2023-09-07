return {
  "nvim-lualine/lualine.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    local lualine = require("lualine")
    lualine.setup {
       options = {
          theme = "nord",
          icons_enabled = true,
       },
    }
  end,
}
