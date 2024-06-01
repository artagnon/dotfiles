local wezterm = require 'wezterm'
local mux = wezterm.mux
local config = wezterm.config_builder()

local cache_dir = os.getenv('HOME') .. '/.cache/wezterm/'
local window_size_cache_path = cache_dir .. 'window_size_cache.txt'

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
