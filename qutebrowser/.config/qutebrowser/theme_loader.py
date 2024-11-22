import os
from urllib.request import urlopen
from qutebrowser.api import cmdutils

if not os.path.exists(config.configdir / "theme.py"):
    theme = "https://raw.githubusercontent.com/catppuccin/qutebrowser/main/setup.py"
    with urlopen(theme) as themehtml:
        with open(config.configdir / "theme.py", "a") as file:
            file.writelines(themehtml.read().decode("utf-8"))

if os.path.exists(config.configdir / "theme.py"):
    import theme
    theme.setup(c, 'latte', True)

@cmdutils.register()
def theme_update(flavor: str, plain: bool = False) -> None:
    """Set new catppuccin flavor.

    Args:
        flavor: Theme flavor to set.
        plain: use plain colors.
    """
    theme.setup(c, flavor, plain)
