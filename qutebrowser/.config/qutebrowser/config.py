import urllog

#c.colors.webpage.darkmode.enabled = True

c.auto_save.session = True
c.backend = 'webengine'
c.statusbar.show = 'in-mode'

c.fileselect.handler = 'external'
c.fileselect.single_file.command = ['emacsclient', '{}']
c.fileselect.folder.command = ['emacsclient', '{}']
c.fileselect.multiple_files.command = ['emacsclient', '{}']
c.content.cookies.accept = 'no-3rdparty'

c.editor.command = ['emacsclient', '{}']
c.content.default_encoding = 'utf-8'
c.scrolling.smooth = True
c.downloads.location.prompt = False
c.downloads.remove_finished = 15000
c.window.title_format = '[{host}] {current_title}{private}'
c.input.partial_timeout = 0
c.input.links_included_in_focus_chain = False

c.tabs.background = True
c.tabs.last_close = 'close'
c.tabs.tabs_are_windows = True
c.tabs.show = 'switching'
c.tabs.position = 'left'
c.tabs.favicons.show = 'never'
c.tabs.width = '30%'

# c.hints.next_regexes = [ '\bnext\b','\bmore\b','\bnewer\b','\b[>→≫]\b','\b(>>|»)\b','\bcontinue\b','\bследующая\b' ]
# c.hints.prev_regexes = ['\bprev(ious)?\b','\bback\b','\bolder\b','\b[<←≪]\b','\b(<<|«)\b','\bпредыдущая\b' ]

c.url.default_page = 'about:blank'
c.url.start_pages = 'about:blank'

c.url.searchengines = {
    '!yt': 'https://www.youtube.com/results?search_query={}',
    'DEFAULT': 'https://duckduckgo.com/lite?q={}',
    '!g': 'https://www.google.ru/search?hl=en&q={}',
    '!gh': 'https://github.com/search?utf8=✓&type=Code&q={}',
    '!gm': 'https://www.google.com/maps?hl=en&q={}',
    '!gi': 'https://www.google.com/search?q={}&tbs=imgo:1&udm=2'
}

c.colors.completion.fg = '#333333'
c.colors.completion.item.selected.bg = 'white'
c.colors.completion.odd.bg = 'white'
c.colors.completion.even.bg = 'white'
c.colors.completion.category.fg = '#444444'
c.colors.completion.category.bg = 'yellow'
c.colors.statusbar.normal.fg = 'black'
c.colors.statusbar.normal.bg = '#f0f0f0'
c.colors.statusbar.progress.bg = 'magenta'
c.colors.statusbar.url.success.http.fg = 'black'
c.colors.statusbar.url.success.https.fg = 'darkgreen'
c.colors.statusbar.url.hover.fg = 'blue'
c.colors.tabs.odd.fg = 'black'
c.colors.tabs.odd.bg = '#b0b0b0'
c.colors.tabs.even.fg = 'black'
c.colors.tabs.even.bg = '#a0a0a0'
c.colors.tabs.selected.odd.fg = 'blue'
c.colors.tabs.selected.odd.bg = 'white'
c.colors.tabs.bar.bg = '#f0f0f0'
c.fonts.default_family = ["Fira Code", "Terminus", "Fixed"]
c.fonts.completion.entry = '12pt default_family'
c.fonts.statusbar = '13pt monospace'
c.fonts.prompts = '12pt sans-serif---'

# bindings
config.unbind('q', mode='normal')
config.unbind('<Ctrl-p>', mode='command')
config.unbind('<Ctrl-n>', mode='command')
#config.unbind('xx', mode='normal')

config.bind('O', 'cmd-set-text :open {url:pretty}')
config.bind('t', 'cmd-set-text -s :open -t')
config.bind('T', 'cmd-set-text :open -t -i {url:pretty}')
config.bind('xx', 'spawn -u orgprotocol')
config.bind('xp', 'spawn -u password_fill')
config.bind('xm', 'spawn --detach mpv --force-window=immediate {url}')
config.bind(';m', 'hint links spawn --detach mpv --force-window=immediate {hint-url}')

config.bind('<Ctrl-w>', 'fake-key <Ctrl-Backspace>', mode='insert')
config.bind('<Ctrl-h>', 'fake-key <Backspace>', mode='insert')
config.bind('<Ctrl-i>', 'fake-key <Tab>', mode='insert')
config.bind('<Ctrl-n>', 'fake-key <Down>', mode='insert')
config.bind('<Ctrl-p>', 'fake-key <Up>', mode='insert')
config.bind('<Ctrl-a>', 'fake-key <Home>', mode='insert')
config.bind('<Ctrl-e>', 'fake-key <End>', mode='insert')

config.bind('<Ctrl-n>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-p>', 'completion-item-focus prev', mode='command')
config.bind('<Ctrl-w>', 'rl-backward-kill-word', mode='command')

config.load_autoconfig(False)
