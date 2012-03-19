import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import qualified Data.Map as M

myKeys x =
             [ ((modMask x,               xK_Right), nextWS)
             , ((modMask x,               xK_Left),  prevWS)
             , ((modMask x .|. shiftMask, xK_Right), shiftToNext)
             , ((modMask x .|. shiftMask, xK_Left),  shiftToPrev)
             , ((modMask x,               xK_t),     toggleWS)
             ]
newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-launcher" --> doFloat
    ])

main = xmonad gnomeConfig {
         manageHook = myManageHook
       , layoutHook = smartBorders $ layoutHook defaultConfig
       , modMask = mod4Mask
       , keys = newKeys
       , terminal = "gnome-terminal"
       }
