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
local bashets = require("bashets")

require("collision") {
  up    = { "k" },
  down  = { "j" },
  left  = { "h" },
  right = { "l" },
                     }
local tyrannical = require("tyrannical")

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
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
config_dir = awful.util.getdir("config")
-- beautiful.init(config_dir .. "/themes/brown/theme.lua")
beautiful.init(awful.util.get_themes_dir() .. "default/theme.lua")
beautiful.border_width = 3
beautiful.border_focus = '#fa3321'

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

browser = "qutebrowser --backend webengine"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.max,
    awful.layout.suit.floating,
    awful.layout.suit.tile.right,
}
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- deprecated task widget
-- local taskwidget = wibox.widget.textbox()
-- bashets.register("/home/sarg/.local/bin/task.py current", {
--                    widget = taskwidget,
--                    separator = '\0',
--                    format = "$1",
--                    update_time = 1
-- })


-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
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

bat = awful.widget.watch('cat /sys/class/power_supply/BAT1/capacity', 15, function(w, s)
                           w:set_text("[bat: " .. s:gsub("\n", "") .. "]")
end)

mykeyboardlayout = awful.widget.keyboardlayout()
awful.screen.connect_for_each_screen(function(s)
    -- awful.tag({ 'code', 'term', 'mail', 'b' }, s, 
    --   {
    --     layouts[3],
    --     layouts[3],
    --     layouts[1],
    --     layouts[1]
    --   }
    -- )

    -- awful.tag.setnmaster(3, tags[s][2]) FIXME

    s.mylayoutbox = awful.widget.layoutbox(s)

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s,
                                         function(c, screen)
                                           return awful.widget.tasklist.filter.minimizedcurrenttags(c, screen)
                                             or awful.widget.tasklist.filter.focused(c, screen)
                                         end, 
                                         mytasklist.buttons)


    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Add widgets to the wibox
    s.mywibox:setup {
      layout = wibox.layout.align.horizontal,
      { -- Left widgets
        layout = wibox.layout.fixed.horizontal,
        s.mytaglist,
        s.mypromptbox,
      },
      s.mytasklist, -- Middle widget
      { -- Right widgets
        layout = wibox.layout.fixed.horizontal,
        bat,
        mykeyboardlayout,
        wibox.widget.systray(),
        mytextclock,
        s.mylayoutbox,
      },
    }
end)

-- }}}

-- {{{ Tyrannical
tyrannical.tags = {
    {
        name        = "Term",                 -- Call the tag "Term"
        init        = true,                   -- Load the tag on startup
        exclusive   = true,                   -- Refuse any other type of clients (by classes)
        screen      = {1,2},                  -- Create this tag on screen 1 and screen 2
        layout      = awful.layout.suit.tile, -- Use the tile layout
        instance    = {"dev", "ops"},         -- Accept the following instances. This takes precedence over 'class'
        class       = { --Accept the following classes, refuse everything else (because of "exclusive=true")
            "xterm" , "urxvt" , "aterm","URxvt","XTerm","konsole","terminator","gnome-terminal"
        }
    } ,
    {
        name        = "Internet",
        init        = true,
        exclusive   = true,
      --icon        = "~net.png",                 -- Use this icon for the tag (uncomment with a real path)
        screen      = screen.count()>1 and 2 or 1,-- Setup on screen 2 if there is more than 1 screen, else on screen 1
        layout      = awful.layout.suit.max,      -- Use the max layout
        class = {
            "Opera"         , "Firefox"        , "Rekonq"    , "Dillo"        , "Arora",
            "Chromium"      , "nightly"        , "minefield" , "qutebrowser"     }
    } ,
    {
        name        = "Files",
        init        = true,
        exclusive   = true,
        screen      = 1,
        layout      = awful.layout.suit.tile,
        exec_once   = {"dolphin"}, --When the tag is accessed for the first time, execute this command
        class  = {
            "Thunar", "Konqueror", "Dolphin", "ark", "Nautilus","emelfm"
        }
    } ,
    {
        name        = "Develop",
        init        = true,
        exclusive   = true,
        screen      = 1,
        layout      = awful.layout.suit.max                          ,
        class ={ 
            "Kate", "KDevelop", "Codeblocks", "Code::Blocks" , "DDD", "kate4"}
    } ,
    {
        name        = "Doc",
        init        = false, -- This tag wont be created at startup, but will be when one of the
                             -- client in the "class" section will start. It will be created on
                             -- the client startup screen
        exclusive   = true,
        layout      = awful.layout.suit.max,
        class       = {
            "Assistant"     , "Okular"         , "Evince"    , "EPDFviewer"   , "xpdf",
            "Xpdf"          ,                                        }
    } ,
}

-- Ignore the tag "exclusive" property for the following clients (matched by classes)
tyrannical.properties.intrusive = {
    "ksnapshot"     , "pinentry"       , "gtksu"     , "kcalc"        , "xcalc"               ,
    "feh"           , "Gradient editor", "About KDE" , "Paste Special", "Background color"    ,
    "kcolorchooser" , "plasmoidviewer" , "Xephyr"    , "kruler"       , "plasmaengineexplorer",
}

-- Ignore the tiled layout for the matching clients
tyrannical.properties.floating = {
    "MPlayer"      , "pinentry"        , "ksnapshot"  , "pinentry"     , "gtksu"          ,
    "xine"         , "feh"             , "kmix"       , "kcalc"        , "xcalc"          ,
    "yakuake"      , "Select Color$"   , "kruler"     , "kcolorchooser", "Paste Special"  ,
    "New Form"     , "Insert Picture"  , "kcharselect", "mythfrontend" , "plasmoidviewer" 
}

-- Make the matching clients (by classes) on top of the default layout
tyrannical.properties.ontop = {
    "Xephyr"       , "ksnapshot"       , "kruler"
}

-- Force the matching clients (by classes) to be centered on the screen on init
tyrannical.properties.placement = {
    kcalc = awful.placement.centered
}

tyrannical.settings.block_children_focus_stealing = true --Block popups ()
tyrannical.settings.group_children = true --Force popups/dialogs to have the same tags as the parent client
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
    awful.spawn("pulseaudio-ctl " .. val)

    awful.spawn.easy_async("pulseaudio-ctl full-status",
                           function(stdout, stderr, reason, exit_code)
                             local volume = tonumber(string.match(stdout, "^%d+")) or 0
                             vol_notify_id = naughty.notify({ text = "[" .. volume .. "%]", timeout = 5, replaces_id = vol_notify_id, font = "fixed"  }).id
                           end
    )
  end
end

local firefoxPrev

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    -- scratches
    awful.key({ modkey,           }, "space", function () scratch.drop("urxvt -name drop -e zsh -7", "bottom", "center", 1, 0.5, true, mouse.screen, "drop") end),
    awful.key({ modkey            }, "i", function () scratch.drop("Telegram", "center", "center", 0.6, 0.5, true, mouse.screen, "Telegram") end),

    -- awful.key({ modkey,           }, "k", move("up")),
    -- awful.key({ modkey,           }, "l", move("right")),
    -- awful.key({ modkey,           }, "h", move("left")),
    -- awful.key({ modkey,           }, "j", move("down")),

    awful.key({ modkey,           }, "F8", function() awful.spawn('emacs-capture --eval \'(org-capture nil)\'') end),

    -- awful.key({ modkey, }, "F9", function() awful.spawn.with_shell('task.py pause') end),
    -- awful.key({ modkey, }, "F8", function() awful.spawn.with_shell('task.py select') end),
    -- awful.key({ modkey, }, "F7", function() awful.spawn.with_shell('task.py stop') end),
    -- awful.key({ modkey, "Shift"}, "F7", function() awful.spawn.with_shell('task.py done') end),
    -- awful.key({ modkey, }, "F6", function() awful.spawn.with_shell('task.py todo') end),
    -- awful.key({                   }, "KP_Multiply", function() awful.spawn("timew gant today") end),

    -- Layout manipulation
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
           awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    -- rarely used layout change keys
    -- TODO: remove
    awful.key({ modkey,           }, "\\", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "\\", function () awful.layout.inc(layouts, -1) end),

    -- Volume
    awful.key({ modkey, "Control" }, "Up",   set_volume("up")) ,
    awful.key({ modkey, "Control" }, "Down",   set_volume("down")),
    awful.key({ modkey,           }, "F1", function() awful.spawn('clementine -t') end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn("urxvt") end),
    awful.key({                   }, "Print", function () awful.spawn("screenshot.sh") end),
    awful.key({                   }, "KP_Subtract", function () awful.spawn("xautolock -locknow") end),
    awful.key({ modkey,           }, "F12", awesome.restart),
    awful.key({ modkey,           }, "o", function()
        awful.client.run_or_raise('emacsclient -nc -a ""', function(c) return awful.rules.match(c, { class = "Emacs" }) end)
    end),
    awful.key({ modkey,           }, "e", function()
	    if client.focus and client.focus.class == 'qutebrowser' then
        if firefoxPrev and firefoxPrev.valid then
          awful.client.jumpto(firefoxPrev)
        end

        firefoxPrev = nil
	    else
        firefoxPrev = client.focus
		    awful.client.run_or_raise(browser, function (c)
			    return awful.rules.match(c, {class = 'qutebrowser'})
		    end)
	    end
    end),
    
    -- Prompt
    awful.key({ modkey },            "r",     function () awful.spawn("rofi -show run") end,
      {description = "run prompt", group = "launcher"})
    -- awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
    --   {description = "run prompt", group = "launcher"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen            end),
    awful.key({ modkey,           }, "m",
      function (c)
        c.maximized = not c.maximized
        c:raise()
      end,
      {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

root.keys(globalkeys)

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  { rule = { },
    properties = { border_width = beautiful.border_width,
                   border_color = beautiful.border_normal,
                   focus = awful.client.focus.filter,
                   -- raise = true,
                   keys = clientkeys,
                   buttons = clientbuttons,
                   screen = awful.screen.preferred,
                   placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }
  },

  -- Add titlebars to normal clients and dialogs
  { rule_any = {type = { "dialog" }}, properties = { titlebars_enabled = true }},

  -- { rule = { instance = "urxvt" }, properties = { tag = tags[1][2] }, callback = awful.client.jumpto }, FIXME
  -- { rule = { name = "GANT TIMEWARRIOR" }, properties = { floating = true, width = 2000 }, callback = awful.placement.centered },
  { rule = { instance = "emacs-capture" }, properties = { floating = true, height = 300 }, callback = awful.placement.centered },
  { rule = { class = "Pavucontrol" }, properties = { floating = true }, callback = awful.placement.centered },
  { rule = { class = "jetbrains-idea" }, callback = awful.client.setmaster },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
                        -- Set the windows at the slave,
                        -- i.e. put it at the end of others instead of setting it master.
                        if not awesome.startup then awful.client.setslave(c) end

                        if awesome.startup and
                          not c.size_hints.user_position
                        and not c.size_hints.program_position then
                          -- Prevent clients from being unreachable after screen count changes.
                          awful.placement.no_offscreen(c)
                        end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

client.connect_signal("property::urgent", function (c)
                         if c.class == "qutebrowser" then
                            awful.client.urgent.jumpto(c)
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

function run_once(cmd)
    findme = cmd
    firstspace = cmd:find(" ")
    if firstspace then
        findme = cmd:sub(0, firstspace-1)
    end
    awful.spawn.with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. cmd .. ")")
end

awful.spawn.with_shell('keymap.sh')
--run_once('compton -i 0.3 -f -D 10 -I 0.07 -O 0.07 -b')
awful.spawn('hsetroot -solid \'#000000\'')
run_once('xautolock -locker lock.sh &')
run_once('unclutter &')
-- }}}

local redshift = require("redshift")
-- 1 for dim, 0 for not dimmed
redshift.init(1)
bashets.start()
