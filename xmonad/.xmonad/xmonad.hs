import XMonad
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
 

baseConfig = desktopConfig

myLayout =  twoPane ||| Full 
  where
    myTabbed = tabbed shrinkText tabbedTheme
    tabbedTheme = defaultTheme { fontName="xft:DejaVu Sans:size=10" }
    twoPane = smartBorders $ combineTwo (TwoPane (3/100) (1/2)) Full myTabbed

keepMaster :: String -> ManageHook
keepMaster c = assertSlave <+> assertMaster
 where
   assertSlave = fmap (/= c) className --> doF W.swapDown
   assertMaster = className =? c --> doF W.swapMaster

myManageHook = composeAll
  [ keepMaster "Firefox" 
  , doF W.swapDown
  ]

main = xmonad baseConfig
    { terminal    = "urxvt"
    -- , layoutHook  = myLayout
    --, layoutHook = avoidStruts $ myTabbed ||| layoutHook defaultConfig
    , manageHook = myManageHook <+> manageHook baseConfig
    , layoutHook = myLayout
    , borderWidth = 3
    }
