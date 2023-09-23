return {
   "nvim-lualine/lualine.nvim",
   dependencies = { "nvim-tree/nvim-web-devicons" },
   config = function()
      local lualine = require("lualine")
      lualine.setup {
         options = {
            icons_enabled = true,
            component_separators = '|',
            section_separators = '',
            theme = "nordic",
         },
         sections = {
            lualine_x = {
               {
                  require("noice").api.statusline.mode.get,
                  cond = require("noice").api.statusline.mode.has,
                  color = { fg = "#ff9e64" },
               }
            },
            lualine_a = {
               {
                  'buffers',
               }
            }
         }
      }
   end,
}
