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
           -- , ((modMask x,               xK_s),     shellPrompt defaultXPConfig)
           , ((modMask x,               xK_s),     spawn "dmenu_run -b -fn 'Droid Sans Mono-14'")
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
    , className =? "Unity-2d-launcher" --> doIgnore
    , className =? "dota2" --> doIgnore
    -- , className =? "DyingLightGame" --> doFloat doesn't work either way.
    , className =? "Verdun.x86_64" --> doIgnore
    , className =? "PillarsOfEternity" --> doIgnore
    , className =? "hl2_linux" --> doIgnore
    , className =? "eu4" --> doIgnore
    , className =? "csgo_linux" --> doIgnore
    , className =? "RocketLeague" --> doIgnore
    , className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , className =? "Steam" --> doFloat
    , className =? "steam" --> doIgnore -- bigpicture-mode
    , className =? "MainThrd" --> doFloat
    , manageDocks
    ])

myStartupHook = do
  spawn "/usr/bin/synclient TouchpadOff=1"
  spawn "xmodmap ~/.Xmodmap"
  spawn "stalonetray --dockapp-mode simple"
  -- spawnOnce "unity-settings-daemon"
  spawn "gnome-settings-daemon"
  spawn "nm-applet"
  spawn "pasystray"
  spawn "fdpowermon"

-- myLayoutHook = noBorders Full ||| noBorders (tabbed shrinkText defaultTheme) ||| Accordion

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/callen/.xmobarrc"
    xmonad $ gnomeConfig {
         manageHook = myManageHook
       , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
       , logHook = dynamicLogWithPP xmobarPP
                   { ppOutput = hPutStrLn xmproc
                   , ppTitle = xmobarColor "green" "" . shorten 50
                   }
       , startupHook = myStartupHook
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "gnome-terminal"
       -- , terminal = "alacritty"
       , handleEventHook =
         mconcat [ docksEventHook
                 , handleEventHook defaultConfig ]
       -- , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
       }
