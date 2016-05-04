import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo 
import XMonad.Util.EZConfig
import XMonad.Config.Desktop
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Accordion
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LimitWindows
import XMonad.Layout.Simplest
import XMonad.Layout.Combo
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import           XMonad.Hooks.EwmhDesktops        (ewmh)

import           System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad.Util.NamedScratchpad

 

baseConfig = desktopConfig

myLayout = avoidStruts $ twoPane ||| Full 
  where
    myTabbed = tabbed shrinkText tabbedTheme
    tabbedTheme = defaultTheme { fontName="xft:DejaVu Sans:size=10" }
    twoPane = smartBorders $ combineTwo (TwoPane (3/100) (1/2)) Full myTabbed

keepMaster :: String -> ManageHook
keepMaster c = assertSlave <+> assertMaster
 where
   assertSlave = fmap (/= c) className --> doF W.swapDown
   assertMaster = className =? c --> doF W.swapMaster

scratchpads = [ NS "telegram" "Telegram" (className =? "Telegram") defaultFloating ]

myManageHook = composeAll
  [ keepMaster "Firefox" 
  , doF W.swapDown
  , manageDocks
  , namedScratchpadManageHook scratchpads
  ]

myKeys = 
  [ ("M-i", namedScratchpadAction scratchpads "telegram")
  , ("M-w", runOrRaise "chromium-browser" (className =? "chromium-browser"))
  ]

main = xmonad $ ewmh $ pagerHints $ baseConfig
    { terminal    = "urxvt"
    -- , layoutHook  = myLayout
    --, layoutHook = avoidStruts $ myTabbed ||| layoutHook defaultConfig
    , startupHook = setWMName "LG3D"
    , manageHook = myManageHook <+> manageHook baseConfig
    , logHook = updatePointer (0.5, 0.5) (1, 1)
    , layoutHook = myLayout
    , borderWidth = 3
    } `additionalKeysP` myKeys

