-- :help options
local options = {
   backup = false,
   clipboard = "unnamedplus",
   cmdheight = 2,
   completeopt = { "menuone", "noselect" },
   conceallevel = 0,
   fileencoding = "utf-8",
   hlsearch = true,
   incsearch = true,
   ignorecase = true,
   mouse = "a",
   showmode = true,
   pumheight = 10,
   showtabline = 2,
   smartcase = true,
   smartindent = true,
   splitbelow = true,
   splitright = true,
   swapfile = false,
   termguicolors = true,
   timeoutlen = 1000,
   undofile = true,
   updatetime = 300,
   writebackup = false,
   expandtab = true,
   shiftwidth = 2,
   tabstop = 2,
   cursorline = true,
   number = true,
   relativenumber = false,
   numberwidth = 4,
   signcolumn = "yes",
   wrap = true,
   scrolloff = 8,
   sidescrolloff = 8,
   guifont = "monospace:h17",

}

for option, value in pairs(options) do
  vim.opt[option] = value
end

vim.g.mapleader = " " -- Set leader to SPACE

vim.opt.shortmess:append "c"

vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]
vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work
