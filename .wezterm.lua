local wezterm = require 'wezterm'
local config = wezterm.config_builder()
config.default_prog = { '/opt/homebrew/bin/fish', '-l' }
config.color_scheme = 'iTerm2 Default'
config.unix_domains = {
  {
    name = 'unix',
  },
}
config.default_gui_startup_args = { 'connect', 'unix' }
config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentTab { confirm = false },
  },
}
config.window_close_confirmation = 'NeverPrompt'
return config
