return {
   'VonHeikemen/lsp-zero.nvim',
   dependencies = {
      -- LSP Support
      { 'neovim/nvim-lspconfig' },
      { 'williamboman/mason.nvim' },
      { 'williamboman/mason-lspconfig.nvim' },

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'hrsh7th/cmp-buffer'},
      {'hrsh7th/cmp-path'},
      {'hrsh7th/cmp-nvim-lua'},
      {'saadparwaiz1/cmp_luasnip'},
      {'L3MON4D3/LuaSnip'},
      {
         "SmiteshP/nvim-navbuddy",
         dependencies = {
            "SmiteshP/nvim-navic",
            "MunifTanjim/nui.nvim"
         },
      },
   },
   config = function()
      local lsp = require("lsp-zero")
      local navbuddy = require("nvim-navbuddy")
      local navic = require("nvim-navic")
      lsp.preset("recommended")
      require('mason').setup()
      require('mason-lspconfig').setup({
        ensure_installed = { 
          "lua_ls", 
          "pyright", 
          "ts_ls" 
        },
      })

      -- Fix Undefined global 'vim'
      local lspconfig = require('lspconfig')
      lspconfig.lua_ls.setup {
        settings = {
          Lua = {
            diagnostics = {
              -- Get the language server to recognize the `vim` global
              globals = { 'vim' },
            },
            workspace = {
              -- Make the server aware of Neovim runtime files
              library = vim.api.nvim_get_runtime_file("", true),
              checkThirdParty = false,  -- Prevent prompts about third-party libraries
            },
          },
        },
      }

      local cmp = require('cmp')
      local luasnip = require('luasnip')
      local cmp_select = { behavior = cmp.SelectBehavior.Select }
      local cmp_mappings = lsp.defaults.cmp_mappings({
         ['<A-j>'] = cmp.mapping.select_prev_item(cmp_select),
         ['<A-k>'] = cmp.mapping.select_next_item(cmp_select),
         ['<C-y>'] = cmp.mapping.confirm({ select = true }),
         ["<C-Space>"] = cmp.mapping.complete(),
      })

      cmp_mappings['<Tab>'] = nil
      cmp_mappings['<S-Tab>'] = nil


      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp_mappings,
        sources = cmp.config.sources({
          { name = 'nvim_lsp' }, 
          { name = 'luasnip' }, 
        }, {
          { name = 'buffer' }, 
        })
      })

      cmp.setup.filetype('gitcommit', {
        sources = cmp.config.sources({
          { name = 'cmp_git' },  -- Git completion
        }, {
          { name = 'buffer' },
        })
      })

      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })

      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
          { name = 'cmdline' }
        })
      })

      lsp.on_attach(function(client, bufnr)
         local opts = { buffer = bufnr, remap = false }
         vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
         vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
         vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
         vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
         vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
         vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
         vim.keymap.set("n", "<leader>vca", function() vim.lsp.buf.code_action() end, opts)
         vim.keymap.set("n", "<leader>vrr", function() vim.lsp.buf.references() end, opts)
         vim.keymap.set("n", "<leader>vrn", function() vim.lsp.buf.rename() end, opts)
         vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
         navbuddy.attach(client, bufnr)
      end)
      lsp.setup()

      vim.diagnostic.config({
         virtual_text = true
      })
   end,
}
