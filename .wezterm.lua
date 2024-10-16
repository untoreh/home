local wezterm = require 'wezterm';
local launch_menu = {}
local default_prog = nil

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
  local distro = "arch"
	table.insert(launch_menu, {
                 label = "PowerShell",
                 args = {"powershell.exe", "-NoLogo"},
	})
  default_prog = {"wsl.exe", "-d", distro, "-e", "bash", "-c",
                  "{ [ -f /tmp/.mounted ] || /etc/wsl-mount.sh; } && SHELL=fish exec fish -li -C ~/"}
else
  local distro = "fedora"
  default_prog = {"/usr/bin/toolbox", "run", "fish", "-li"}
end

wezterm.on("toggle-autoscroll", function(window, pane)
  local overrides = window:get_config_overrides() or {}
  if not overrides.scroll_to_bottom_on_input then
    -- If we haven't overridden it yet, then override with ligatures disabled
    overrides.scroll_to_bottom_on_input =  true
  else
    -- else we did already, and we should disable out override now
    overrides.scroll_to_bottom_on_input = false
  end
  window:set_config_overrides(overrides)
end)

local ssh_domains = {}

for host, config in pairs(wezterm.enumerate_ssh_hosts()) do
  table.insert(ssh_domains, {
    name = host,
    remote_address = host,
    local_echo_threshold_ms = 500,
    -- if you know that the remote host has a posix/unix environment,
    -- setting assume_shell = "Posix" will result in new panes respecting
    -- the remote current directory when multiplexing = "None".
    assume_shell = 'Posix',
  })
end

return {
	unix_domains = {
    {
      name = "init",
    },
    },
	ssh_domains = ssh_domains,
	tls_servers = {
		{bind_address = "0.0.0.0:2233"}
	},
	audible_bell = "Disabled",
  -- don't linger terminated process unconditionally
	exit_behavior = "Close",
	window_decorations = "NONE",
  font = wezterm.font("Hack"),
  color_scheme = "Dracula",
  scrollback_lines = 9000000,
  enable_scroll_bar = true,
  launch_menu = launch_menu,
  default_prog = default_prog,
  canonicalize_pasted_newlines = "None", -- prevent double newlines on windows
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
    {key="f", mods="CTRL|SHIFT|ALT", action="ToggleFullScreen"},
    {key="h", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Left", 1}}},
    {key="j", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Down", 1}}},
    {key="k", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Up", 1}}},
    {key="l", mods="CTRL|SHIFT|ALT", action=wezterm.action{AdjustPaneSize={"Right", 1}}} ,
    {key="{", mods="SHIFT|ALT", action=wezterm.action{ActivateTabRelative=-1}},
    {key="n", mods="CTRL|ALT", action=wezterm.action{ActivateTabRelative=1}},
    {key="}", mods="SHIFT|ALT", action=wezterm.action{ActivateTabRelative=1}},

    {key="p", mods="CTRL|ALT", action=wezterm.action{ActivateTabRelative=-1}},
    {key="h", mods="CTRL|ALT", action=wezterm.action{ScrollToPrompt=-1}},
    {key="l", mods="CTRL|ALT", action=wezterm.action{ScrollToPrompt=1}},
    {key="u", mods="CTRL|ALT", action=wezterm.action{ScrollByPage=-1}},
    {key="d", mods="CTRL|ALT", action=wezterm.action{ScrollByPage=1}},
    {key="j", mods="CTRL|ALT", action=wezterm.action{ScrollByLine=1}},
    {key="k", mods="CTRL|ALT", action=wezterm.action{ScrollByLine=-1}},

    {key="l", mods="SHIFT|ALT", action="ShowLauncher"},
    {key="s", mods="CTRL|ALT", action=wezterm.action{EmitEvent="toggle-autoscroll"}},
    {key="a", mods="CTRL|SHIFT", action=wezterm.action{
       SpawnCommandInNewTab={
         args={"/usr/bin/toolbox", "run", "/usr/bin/fish", "-li"}}}},
    -- host shell
    {key="t", mods="CTRL|SHIFT", action=wezterm.action{
       SpawnCommandInNewTab={
         args={"/usr/bin/fish"}}}},
	 {key="x", mods="ALT", action=wezterm.action.ActivateCommandPalette,}


  },
  --	leader = { key="a", mods="CTRL", timeout_milliseconds=1000 },
  -- enable_wayland = true,
}
