return {
   'nvim-treesitter/nvim-treesitter',
   dependencies = {
      'nvim-treesitter/nvim-treesitter-refactor'
   },
   config = function()
      vim.cmd([[TSUpdate]])
      require 'nvim-treesitter.configs'.setup {
         ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "rust", "javascript", "typescript" },

         sync_install = false,
         auto_install = true,

         highlight = {
            enable = true,
            additional_vim_regex_highlighting = false,
         },
      }
      require 'nvim-treesitter.configs'.setup {
         refactor = {
            highlight_current_scope = { enable = false },
           smart_rename = { enable = true },
         },
      }
   end,
}
