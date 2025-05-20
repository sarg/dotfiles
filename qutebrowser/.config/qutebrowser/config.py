import urllog

c.auto_save.session = True
c.backend = 'webengine'
c.statusbar.show = 'never'

c.fileselect.handler = 'external'
c.fileselect.single_file.command = ['emacsclient', '{}']
c.fileselect.folder.command = ['emacsclient', '{}']
c.fileselect.multiple_files.command = ['emacsclient', '{}']
c.content.cookies.accept = 'no-3rdparty'

c.new_instance_open_target = 'tab-silent'
c.editor.command = ['emacsclient', '{}']
c.content.default_encoding = 'utf-8'
c.content.javascript.clipboard = 'access'
c.scrolling.smooth = True
c.downloads.location.prompt = False
c.downloads.remove_finished = 15000
c.window.title_format = '{audio}{private}{current_title}{title_sep}{current_url}'
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
    '!imdb': 'https://www.imdb.com/find/?s=all&q={}',
    '!g': 'https://www.google.ru/search?hl=en&q={}',
    '!gh': 'https://github.com/search?utf8=✓&type=Code&q={}',
    '!gm': 'https://www.google.com/maps?hl=en&q={}',
    '!gi': 'https://www.google.com/search?q={}&tbs=imgo:1&udm=2'
}

c.fonts.default_family = ["Hack"]
c.fonts.default_size = '14pt'

# allow video calls
for site in ("meet.google.com", ):
    for perm in ("content.notifications.enabled", "content.media.audio_video_capture",
                 "content.media.audio_capture", "content.media.video_capture"):
        config.set(perm, True, site)

# bindings
config.unbind('q', mode='normal')
config.unbind('<Ctrl-p>', mode='command')
config.unbind('<Ctrl-n>', mode='command')
#config.unbind('xx', mode='normal')


config.bind("ym", "yank inline [[{url:yank}][{title}]]")
config.bind("t", "emacs '(qutebrowser-launcher-window)'")
config.bind("o", "emacs '(qutebrowser-launcher)'")
config.bind("O", "emacs '(qutebrowser-launcher \"{url:pretty}\")'")
config.bind('xx', 'spawn -u orgprotocol')
config.bind('xp', "emacs '(qutebrowser-pass \"{url}\")'")
config.bind('xm', 'spawn --detach mpv --force-window=immediate {url}')
config.bind(';m', 'hint links spawn --detach mpv --force-window=immediate {hint-url}')
config.bind(';a', 'hint links run open -t https://web.archive.org/web/{hint-url}')

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

## theme loader
config.source("emacs_theme.py")
