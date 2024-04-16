return {
  "xiyaowong/transparent.nvim",
  config = function ()
    require("transparent").setup({
      groups = { 
        'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier', 'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function', 'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText', 'SignColumn', 'CursorLine', 'CursorLineNr', 'StatusLine', 'StatusLineNC', 'EndOfBuffer',
      },
      extra_groups = {'NormalFloat', 'NvimTreeNormal'}, 
      exclude_groups = {},
      require('transparent').clear_prefix('BufferLine', 'NeoTree', 'lualine'),
      -- vim.cmd "TransparentEnable",
      vim.cmd "TransparentDisable",
    })
  end,
}
