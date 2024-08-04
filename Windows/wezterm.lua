-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- Configuration table
local config = {}

-- Cleaner error messages
config = wezterm.config_builder()

-- Colorscheme
config.color_scheme = 'nord'

-- Spawn a arch wsl shell on login
config.default_prog = { 'arch' }

-- Transparency
config.window_background_opacity = 0

-- Blur On Windows
config.win32_system_backdrop = 'Acrylic'

-- Tab Bar
config.hide_tab_bar_if_only_one_tab = true
config.window_decorations = "TITLE | RESIZE"

-- Font
config.font = wezterm.font('JetBrainsMono Nerd Font Mono', { weight = 'Regular', italic = false })

-- Cursor
config.default_cursor_style = 'BlinkingBar'

-- Return config table
return config
