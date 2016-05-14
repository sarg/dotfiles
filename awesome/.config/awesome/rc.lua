-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
require("awful.remote")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")

local scratch = require("scratch")


-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
config_dir = awful.util.getdir("config")
beautiful.init(config_dir .. "/themes/brown/theme.lua")
beautiful.border_width = 3
beautiful.border_focus = '#fa3321'

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
browser = "chromium-browser" or os.getenv("BROWSER")
editor = "vim" or os.getenv("EDITOR")
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.max,
    awful.layout.suit.floating,
--    awful.layout.suit.tile,
    awful.layout.suit.tile.right,
--    awful.layout.suit.tile.bottom,
--    awful.layout.suit.tile.top,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 'code', 'term', 'web', 'mail', 'b' }, s, 
    	{
		layouts[3],
		layouts[3],
		layouts[1],
		layouts[1],
		layouts[1]
	}
    )

    awful.tag.setnmaster(3, tags[s][2])
end
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Keyboard layout widget
kbdwidget = wibox.widget.textbox()
kbdwidget.border_width = 0
kbdwidget.border_color = beautiful.fg_normal
kbdwidget:set_markup(" EN ")
dbus.request_name("session", "ru.gentoo.kbdd")
dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
dbus.connect_signal("ru.gentoo.kbdd", function(...)
local data = {...}
local layout = data[2]
lts = {[0] = " EN", [1] = " RU"}
kbdwidget:set_markup (lts[layout].." ")
end
)

-- Create a wibox for each screen and add it
mywibox = {}
mylayoutbox = {}
mypromptbox = awful.widget.prompt()
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end)
					  )

for s = 1, screen.count() do
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, function(c, screen)
	return awful.widget.tasklist.filter.minimizedcurrenttags(c, screen) or awful.widget.tasklist.filter.focused(c, screen)
    end, 
    mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylayoutbox[s])
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox)

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(kbdwidget)
    right_layout:add(mytextclock)

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

local function move(dir)
   return function()
      local oldclient = client.focus
      awful.client.focus.bydirection(dir)
      if client.focus == oldclient then
         awful.client.focus.byidx(-1)
      end
      if client.focus then client.focus:raise() end
  end
end

local vol_notify_id
function set_volume(val)
  return function()
    awful.util.spawn("pulseaudio-ctl " .. val)
    local fd = io.popen("pulseaudio-ctl full-status")
    local status = fd:read("*all")
    fd:close()

    local volume = tonumber(string.match(status, "^%d+")) or 0

    vol_notify_id = naughty.notify({ text = "[" .. volume .. "%]", timeout = 5, replaces_id = vol_notify_id, font = "fixed"  }).id
  end
end

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),
    awful.key({ modkey            }, "`", function () scratch.drop("urxvt", "bottom") end),
    awful.key({ modkey            }, "i", function () scratch.drop("Telegram", "center", "center", 0.6, 0.5, true) end),

    awful.key({ modkey,           }, "w", move("up")),
    awful.key({ modkey,           }, "d", move("right")),
    awful.key({ modkey,           }, "a", move("left")),
    awful.key({ modkey,           }, "s", move("down")),

    -- Layout manipulation
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Volume
    awful.key({ modkey, "Control" }, "Up",   set_volume("up")) ,
    awful.key({ modkey, "Control" }, "Down",   set_volume("down")),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({                   }, "Print", function () awful.util.spawn("gnome-screenshot -a") end),
    awful.key({                   }, "KP_Subtract",      function () awful.util.spawn("xdg-screensaver lock") end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox:run() end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Control" }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
-- load the 'run or raise' function
local ror = require("aweror")

-- generate and add the 'run or raise' key bindings to the globalkeys table
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(modkey))
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
		     size_hints_honor = false,
                     focus = awful.client.focus.filter,
		     callback = awful.client.setslave,
		     --raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
--    { rule = { class = "Chromium-browser" },
--      properties = { skip_taskbar = true, tag = tags[1][4] } },
    { rule = { class = "Chromium-browser", role = "pop-up" },
      properties = { floating = true },
      callback = awful.placement.centered
    },
    { rule = { role = "bubble" },
      properties = { floating = true } },
    { rule = { class = "Evolution" },
      properties = { border_width = 0, tag = tags[1][4] } },
    { rule = { class = "Pavucontrol" },
      properties = { floating = true, callback = awful.placement.centered } },
      { rule = { class = "jetbrains-idea" },
      callback = awful.client.setmaster },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
--    c:connect_signal("mouse::enter", function(c)
--        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
--            and awful.client.focus.filter(c) then
--            client.focus = c
--        end
--    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        --awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)

client.connect_signal("focus", function(c) 
	c.border_color = beautiful.border_focus
	--c.opacity = 1
end)
client.connect_signal("unfocus", function(c) 
	c.border_color = beautiful.border_normal 
	--c.opacity = 0.3
end)

function run_once(cmd)
    findme = cmd
    firstspace = cmd:find(" ")
    if firstspace then
        findme = cmd:sub(0, firstspace-1)
    end
    awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

run_once('setxkbmap us,ru -option ctrl:nocaps -option grp:alt_shift_toggle')
--run_once('compton -i 0.3 -f -D 10 -I 0.07 -O 0.07 -b')
run_once('hsetroot -solid \'#000000\'')
run_once('kbdd &')
run_once('unclutter &')
-- }}}

local redshift = require("redshift")
-- 1 for dim, 0 for not dimmed
redshift.init(1)
