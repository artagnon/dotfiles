local wezterm = require 'wezterm'
local mux = wezterm.mux
local config = wezterm.config_builder()

local cache_dir = os.getenv('HOME') .. '/.cache/wezterm/'
local window_size_cache_path = cache_dir .. 'window_size_cache.txt'

config.set_environment_variables = {
  PATH = '/bin:/usr/bin:/usr/local/bin:/opt/homebrew/bin',
}

wezterm.on('gui-startup', function()
  os.execute('mkdir ' .. cache_dir)

  local window_size_cache_file = io.open(window_size_cache_path, 'r')
  if window_size_cache_file ~= nil then
    _, _, width, height = string.find(window_size_cache_file:read(), '(%d+),(%d+)')
    mux.spawn_window{ width = tonumber(width), height = tonumber(height) }
    window_size_cache_file:close()
  else
    local tab, pane, window = mux.spawn_window{}
    window:gui_window():maximize()
  end
end)

wezterm.on('window-resized', function(window, pane)
  local window_size_cache_file = io.open(window_size_cache_path, 'r')
  local tab_size = pane:tab():get_size()
  cols = tab_size['cols']
  rows = tab_size['rows'] + 2 -- Without adding the 2 here, the window doesn't maximize
  contents = string.format('%d,%d', cols, rows)
  window_size_cache_file:write(contents)
  window_size_cache_file:close()
end)

config.color_scheme = 'Catppuccin Mocha'
config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentTab { confirm = false },
  },
}
config.default_prog = { 'fish', '-l' }
config.color_scheme = 'Catppuccin Mocha'
config.window_close_confirmation = 'NeverPrompt'
config.font = wezterm.font 'Source Code Pro'
config.font_size = 12
config.use_ime = false
return config
