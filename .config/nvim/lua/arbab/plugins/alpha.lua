return {
   'goolord/alpha-nvim',
   dependencies = { 'nvim-tree/nvim-web-devicons' },
   config = function()
      local if_nil = vim.F.if_nil

      local default_header = {
         type = "text",
         val = {
            "                                            ",
            "▄█▄    ████▄ ██▄   ▄███▄       ████▄    ▄   ",
            "█▀ ▀▄  █   █ █  █  █▀   ▀      █   █     █  ",
            "█   ▀  █   █ █   █ ██▄▄        █   █ ██   █ ",
            "█▄  ▄▀ ▀████ █  █  █▄   ▄▀     ▀████ █ █  █ ",
            "▀███▀        ███▀  ▀███▀             █  █ █ ",
            "                                     █   ██ ",
            "                                            ",
         },
         opts = {
            position = "center",
            hl = "Namespace",
            -- wrap = "overflow";
         },
      }

      local footer = {
         type = "text",
         val = " Personal configuration of Arbab Khan",
         opts = {
            position = "center",
            hl = "Number",
         },
      }

      local leader = "SPC"

      --- @param sc string
      --- @param txt string
      --- @param keybind string optional
      --- @param keybind_opts table optional
      local function button(sc, txt, keybind, keybind_opts)
         local sc_ = sc:gsub("%s", ""):gsub(leader, "<leader>")

         local opts = {
            position = "center",
            shortcut = sc,
            cursor = 5,
            width = 50,
            align_shortcut = "right",
            hl_shortcut = "Keyword",
         }
         if keybind then
            keybind_opts = if_nil(keybind_opts, { noremap = true, silent = true, nowait = true })
            opts.keymap = { "n", sc_, keybind, keybind_opts }
         end

         local function on_press()
            -- local key = vim.api.nvim_replace_termcodes(keybind .. "<Ignore>", true, false, true)
            local key = vim.api.nvim_replace_termcodes(sc_ .. "<Ignore>", true, false, true)
            vim.api.nvim_feedkeys(key, "t", false)
         end

         return {
            type = "button",
            val = txt,
            on_press = on_press,
            opts = opts,
         }
      end

      local buttons = {
         type = "group",
         val = {
            button("e", "  New File", ":ene <BAR> startinsert <CR>"),
            button("f", "󰭎  Find File", ":lua require'telescope.builtin'.find_files({ hidden = true })<CR>"),
            button("g", "  Live Grep", ":Telescope live_grep<CR>"),
            button("r", "  Recent Files", ":Telescope oldfiles<CR>"),
            button("c", "  Configuration", ":Oil ~/.config/nvim<CR>"),
            button("s", "󰦛  Restore Session", ":SessionManager load_last_session<CR>"),
            button("q", "  Quit", ":qa<CR>"),
         },
         opts = {
            spacing = 1,
         },
      }

      local section = {
         header = default_header,
         buttons = buttons,
         footer = footer,
      }

      local config = {
         layout = {
            { type = "padding", val = 2 },
            section.header,
            { type = "padding", val = 2 },
            section.buttons,
            section.footer,
         },
         opts = {
            margin = 5,
            setup = function()
               vim.api.nvim_create_autocmd("User", {
                  pattern = "AlphaReady",
                  desc = "disable status and tabline for alpha",
                  callback = function()
                     vim.opt.showtabline = 0
                  end,
               })
               vim.api.nvim_create_autocmd("BufUnload", {
                  buffer = 0,
                  desc = "enable status and tabline after alpha",
                  callback = function()
                     vim.opt.showtabline = 2
                  end,
               })
            end,
         },
      }
      require("alpha").setup(config)
   end
};
