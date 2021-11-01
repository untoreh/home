local wezterm = require 'wezterm';
local launch_menu = {}
local default_prog = nil

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
	table.insert(launch_menu, {
                 label = "PowerShell",
                 args = {"powershell.exe", "-NoLogo"},
	})
  default_prog = {"wsl.exe", "-d", "Arch", "-e", "bash", "-c",
                  "{ [ -f /tmp/.mounted ] || /etc/wsl-mount.sh; } && SHELL=fish exec fish -li"}
else
  default_prog = {"fish", "-li"}
end

return {
	unix_domains = {
    {
      name = "init",
    },
    },
	ssh_domains = {
    {
      name = "remote",
      remote_address = "mbx:22",
      username = "fra",
    },
    {
      name = "mbx",
      remote_address = "mbx:22",
      username = "fra",
	}},
	tls_servers = {
		{bind_address = "0.0.0.0:2233"}
	},
	audible_bell = "Disabled",
  -- don't linger terminated process unconditionally
	exit_behavior = "Close",
  window_decorations = "TITLE",
  font = wezterm.font("Hack"),
  color_scheme = "Dracula",
  scrollback_lines = 9000,
  enable_scroll_bar = true,
  launch_menu = launch_menu,
  default_prog = default_prog,
  default_prog = {"fish", "-li"},
  keys = {
    {key="|", mods="CTRL|SHIFT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    {key="\\", mods="CTRL|ALT", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    {key="q", mods="CTRL|SHIFT",
     action=wezterm.action{CloseCurrentPane={confirm=true}}},
    {key="_", mods="CTRL|SHIFT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    {key="-", mods="CTRL|ALT", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    {key="h", mods="CTRL|SHIFT", action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="j", mods="CTRL|SHIFT", action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="k", mods="CTRL|SHIFT", action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="l", mods="CTRL|SHIFT", action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="n", mods="CTRL|SHIFT", action="ToggleFullScreen"},
    {key="m", mods="CTRL|SHIFT|ALT", action=wezterm.action{SendString="/etc/local.d/crt.start &>/dev/null &"}},
    {key="h", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Left", 1}}},
    {key="j", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Down", 1}}},
    {key="k", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Up", 1}}},
    {key="l", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Right", 1}}} ,
    {key="{", mods="SHIFT|ALT", action=wezterm.action{ActivateTabRelative=-1}},
    {key="n", mods="CTRL|ALT", action=wezterm.action{ActivateTabRelative=1}},
    {key="}", mods="SHIFT|ALT", action=wezterm.action{ActivateTabRelative=1}},
    {key="p", mods="CTRL|ALT", action=wezterm.action{ActivateTabRelative=-1}},
    {key="p", mods="SHIFT|ALT", action=wezterm.action{ScrollToPrompt=-1}},
    {key="n", mods="SHIFT|ALT", action=wezterm.action{ScrollToPrompt=1}},
    {key="u", mods="CTRL|SHIFT", action=wezterm.action{ScrollByPage=-1}},
    {key="d", mods="CTRL|SHIFT", action=wezterm.action{ScrollByPage=1}},
    {key="j", mods="SHIFT|ALT", action=wezterm.action{ScrollByLine=1}},
    {key="k", mods="SHIFT|ALT", action=wezterm.action{ScrollByLine=-1}},
    {key="l", mods="SHIFT|ALT", action="ShowLauncher"},
    {key="a", mods="CTRL|SHIFT", action=wezterm.action{
       SpawnCommandInNewTab={
         args={"wsl.exe", "-d", "Arch", "-e", "bash", "-c",
               "{ [ -f /tmp/.mounted ] || /etc/wsl-mount.sh; } && SHELL=fish exec fish -li"}}}},
    {key="p", mods="CTRL|SHIFT", action=wezterm.action{
       SpawnCommandInNewTab={
         args={"powershell.exe", "/NoLogo"}}}},
    {key="g", mods="CTRL|SHIFT", action=wezterm.action{
       SpawnCommandInNewTab={
         args={"nu.exe"}}}}

  },
  --	leader = { key="a", mods="CTRL", timeout_milliseconds=1000 },
  -- enable_wayland = true,
}
