local wezterm = require 'wezterm'
local config = wezterm.config_builder()
config.default_prog = { '/opt/homebrew/bin/fish', '-l' }
config.color_scheme = 'Sonokai (Gogh)'
config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentTab { confirm = false },
  },
}
config.window_close_confirmation = 'NeverPrompt'
return config
