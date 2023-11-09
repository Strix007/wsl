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
config.window_background_opacity = 0.95

-- Font
config.font = wezterm.font('JetBrainsMono Nerd Font Mono', { weight = 'Regular', italic = false })

-- Cursor
config.default_cursor_style = 'BlinkingBar'

-- Return config table
return config
