return {
   "lukas-reineke/indent-blankline.nvim",
   main = "ibl",
   event = { "BufReadPre", "BufNewFile" },
   config = function()
      local highlight = {
         "RainbowRed",
         "RainbowYellow",
         "RainbowBlue",
         "RainbowOrange",
         "RainbowGreen",
         "RainbowViolet",
         "RainbowCyan",
      }

      local hooks = require "ibl.hooks"
      hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
         vim.api.nvim_set_hl(0, "RainbowRed", { fg = "#bf616a" })
         vim.api.nvim_set_hl(0, "RainbowYellow", { fg = "#ebcb8b" })
         vim.api.nvim_set_hl(0, "RainbowBlue", { fg = "#81a1c1" })
         vim.api.nvim_set_hl(0, "RainbowOrange", { fg = "#d08770" })
         vim.api.nvim_set_hl(0, "RainbowGreen", { fg = "#a3be8c" })
         vim.api.nvim_set_hl(0, "RainbowViolet", { fg = "#b48ead" })
         vim.api.nvim_set_hl(0, "RainbowCyan", { fg = "#88c0d0" })
      end)

      require("ibl").setup {
         indent = { highlight = highlight },
      }
   end,
}
