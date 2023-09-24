return {
   'Shatur/neovim-session-manager',
   dependencies = { "nvim-lua/plenary.nvim" },
   config = function()
      local Path = require('plenary.path')
      local config = require('session_manager.config')
      require('session_manager').setup({
         sessions_dir = Path:new(vim.fn.stdpath('data'), 'sessions'), -- The directory where the session files will be saved.
         session_filename_to_dir = session_filename_to_dir,
         dir_to_session_filename = dir_to_session_filename,
         autoload_mode = config.AutoloadMode.Disabled,
         autosave_last_session = true,
         autosave_ignore_not_normal = true,
         autosave_ignore_dirs = {},
         autosave_ignore_filetypes = {
            'gitcommit',
            'gitrebase',
         },
         autosave_ignore_buftypes = {}, -- All buffers of these bufer types will be closed before the session is saved.
         autosave_only_in_session = false, -- Always autosaves session. If true, only autosaves after a session is active.
         max_path_length = 80,      -- Shorten the display path if length exceeds this threshold. Use 0 if don't want to shorten the path at all.
      })
   end,
}
