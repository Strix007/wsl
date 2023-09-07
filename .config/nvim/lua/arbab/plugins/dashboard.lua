return {
   'glepnir/dashboard-nvim',
   event = 'VimEnter',
   config = function()
      local conf = {}
      conf.header = {
         "                                                       ",
         "                                                       ",
         "                                                       ",
         " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
         " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
         " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
         " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
         " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
         " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
         "                                                       ",
         "                                                       ",
         "                                                       ",
         "                                                       ",
      }
      conf.center = {
             {
                icon = "󰈞  ",
                desc = "Find  File                              ",
                action = "Leaderf file --popup",
                key = "<Leader> f f",
             },
             {
                icon = "󰈢  ",
                desc = "Recently opened files                   ",
                action = "Leaderf mru --popup",
                key = "<Leader> f r",
             },
             {
                icon = "  ",
                desc = "Open Nvim config                        ",
                action = "tabnew $MYVIMRC | tcd %:p:h",
                key = "<Leader> e v",
             },
             {
                icon = "  ",
                desc = "New file                                ",
                action = "enew",
                key = "e",
             },
             {
                icon = "󰗼  ",
                desc = "Quit Nvim                               ",
                -- desc = "Quit Nvim                               ",
                action = "qa",
                key = "q",
             },
          }
      require('dashboard').setup {
         theme = 'doom',
         shortcut_type = 'number',
         -- config
         config = conf,
      }
   end,
   dependencies = { { 'nvim-tree/nvim-web-devicons' } }
}
