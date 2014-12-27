import qualified Data.Map as M
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.StackSet(greedyView)

  
myKeys x = [ ((modMask x,               xK_Right), nextWS)
           , ((modMask x,               xK_Left),  prevWS)
           , ((modMask x .|. shiftMask, xK_Right), shiftToNext)
           , ((modMask x .|. shiftMask, xK_Left),  shiftToPrev)
           , ((modMask x,               xK_t),     toggleWS)
           , ((modMask x,               xK_s),     shellPrompt defaultXPConfig)
           , ((mod1Mask .|. controlMask, xK_l),    spawn "xtrlock -b")
           , ((0                , 0x1008ff11),     spawn "amixer -c 1 sset Master 4-")
           , ((0                , 0x1008ff13),     spawn "amixer -c 1 sset Master 4+")
           , ((0                , 0x1008ff03),     spawn "xbacklight -inc -10%")
           , ((0                , 0x1008ff02),     spawn "xbacklight -inc +10%")

           -- , ((0                , 0x1008ff12),     spawn "amixer sset Master toggle")
           ]

newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , resource  =? "stalonetray" --> doIgnore
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    , className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , manageDocks
    ])

myStartupHook = do
  spawnOnce "xmodmap ~/.Xmodmap"
  spawnOnce "stalonetray"
  spawnOnce "unity-settings-daemon"
  spawnOnce "feh --bg-scale ~/Pictures/wallpaper-2782461.jpg"
  spawnOnce "nm-applet"
  spawnOnce "indicator-sound-switcher"

-- myLayoutHook = noBorders Full ||| noBorders (tabbed shrinkText defaultTheme) ||| Accordion

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/callen/.xmobarrc"
    xmonad $ gnomeConfig {
         manageHook = myManageHook
       , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig
       , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
       , startupHook = myStartupHook
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "gnome-terminal"
       }
