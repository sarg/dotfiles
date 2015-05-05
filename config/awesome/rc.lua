---- vim: set fdm=marker:
-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

require("vicious")
--require("scratch")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/theme/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
terminal_tabbed = "urxvtc -pe tabbed"
editor = os.getenv("EDITOR") or "nano"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.tile,
--    awful.layout.suit.tile.left,
--    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
--    awful.layout.suit.fair,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral,
--    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
tags[1] = awful.tag({ "vim", "web", "im" }, s, awful.layout.suit.max)
for s = 2, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ "main" }, s, awful.layout.suit.tile)
end
-- }}}

-- {{{ Moosic menu
require("moosic")
function moosic_menu_run()
    local moosic_menu = awful.menu.new({items = playlist_menu()})
    moosic_menu.w = 1000
    if #moosic_menu.items then
        mouse.coords({ x = 0, y = 0 })
        moosic_menu:toggle(true)
    end
    return true
end
-- }}}

-- {{{ Wibox
-- Create a textclock widget
batwidget = widget({ type = 'textbox' })
vicious.register(batwidget, vicious.widgets.bat, 'B: $2% ', 15, 'BAT1')

volwidget = widget({ type = 'textbox' })
vicious.register(volwidget, vicious.widgets.volume, '| $2 $1% |', 15, 'Master')

--wifiwidget = widget({ type = 'textbox' })
--vicious.register(wifiwidget, vicious.widgets.wifi, '${rate} |', 15, 'wlan0')

mytextclock = awful.widget.textclock({ align = "right" })

-- Create a systray
mysystray = widget({ type = "systray" })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, awful.tag.viewnext),
                    awful.button({ }, 5, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        volwidget,
        batwidget,
        wifiwidget,
        s == 1 and mysystray or nil,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "d",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "a",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),

    -- Layout manipulation awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Custom program
    awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn('amixer set Master 5-') vicious.force({volwidget}) end),
    awful.key({}, "XF86AudioRaiseVolume", function () awful.util.spawn('amixer set Master 5+') vicious.force({volwidget}) end),
    awful.key({}, "XF86AudioMute", function () awful.util.spawn('amixer set Master toggle') vicious.force({volwidget}) end),
    awful.key({}, "XF86MonBrightnessUp", function () awful.util.spawn('xbacklight + 20') end),
    awful.key({}, "XF86MonBrightnessDown", function () awful.util.spawn('xbacklight - 10') end),
    awful.key({}, "XF86RotateWindows", function () awful.util.spawn('xrandr --output VGA1 --auto --rotate normal --above LVDS1 --output LVDS1 --primary --auto') end),

    -- Moosic
    awful.key({ modkey,           }, "x", function () awful.util.spawn('quodlibet --play-pause') end),
    awful.key({ modkey,           }, "c", function () awful.util.spawn('quodlibet --stop') end),
    awful.key({ modkey,           }, "z", function () awful.util.spawn('quodlibet --prev') end),
    awful.key({ modkey,           }, "v", function () awful.util.spawn('quodlibet --next') end),
    awful.key({ modkey,           }, "/", function () awful.util.spawn('quodlibet --toggle-window') end),


    awful.key({ modkey,           }, "p", function () focus_or_hide('Pidgin Buddy List', 'pidgin') end),

    -- Standard program
    awful.key({ modkey,           }, "F1", function () awful.util.spawn(terminal) end),
    awful.key({ modkey,           }, "F2", function () awful.util.spawn(terminal .. ' -e mc -x') end),
    awful.key({ modkey,           }, "F3", function () awful.util.spawn(terminal .. ' -title MUSIC -e mc ~/music ~/newmus') end),

    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "[",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end)
)

clientkeys = awful.util.table.join(
    --awful.key({ modkey,           }, "t",      function (c) scratch.pad.set(c, 150,200,true) end),
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Control" }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end))
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    { rule = { class = "MPlayer" }, properties = { floating = true } },

    { rule = { class = "Skype", name = "MainWindow" }, 
        properties = { 
            --width=150, 
            --height=screen[1].workarea.height, 
            --x=200,
            --y=screen[1].workarea.y, 
            floating = true, sticky = true, skip_taskbar = true 
        } 
    },

    --{rule = {class = "Pidgin", role = "buddy_list"}, properties = { tag = tags[1][3], target = "master", split = 250}},
    {rule = {class = "Pidgin"}, properties = { floating = true }},
    {rule = {class = "Pidgin", role = "buddy_list"}, 
        properties = { 
            floating = true, 
            sticky = true, 
            skip_taskbar = true, 
            width = 200,
            maximized_vertical = true,
            x = 700,
        },
    },
    {rule = {class = "Pidgin", role = "conversation"}, properties = { tag = tags[1][3], floating = false, target = "slave",  split = 250}},
    {rule = {class = "Chromium"}, properties = { tag = tags[1][2], switchtotag = true }}, 
    {rule = {name = "SHUTDOWN"}, properties = { floating = true }}, 
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    -- awful.titlebar.add(c, { modkey = modkey })

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

end)

-- }}}

-- {{{ focus_or_hide
function focus_or_hide(str, cmd)
    local clients = client.get()
    for i, c in pairs(clients) do
--        naughty.notify({ text = c.class .. " " .. c.name .. " " .. c.instance })
        if string.find(string.lower(c.class .. " " .. c.name .. " " .. c.instance), string.lower(str)) then
            awful.client.movetotag(awful.tag.selected(), c)
            if not c.minimized then
                c.minimized = true
            else
                client.focus = c
                c:raise()
            end
            return
        end
    end
    awful.util.spawn(cmd)
end
-- }}}
