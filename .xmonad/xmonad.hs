import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import qualified Data.Map as M

-- import qualified Data.Map as M
-- import qualified XMonad.StackSet as W
-- import System.Exit

myKeys x =
             [ ((modMask x,               xK_Right), nextWS)
             , ((modMask x,               xK_Left),  prevWS)
             , ((modMask x .|. shiftMask, xK_Right), shiftToNext)
             , ((modMask x .|. shiftMask, xK_Left),  shiftToPrev)
             , ((modMask x,               xK_t),     toggleWS)
             ]
newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

main = xmonad $ defaultConfig {
  modMask = mod4Mask
  , keys = newKeys
  }

-- main = xmonad $ gnomeConfig {
--   modMask = mod4Mask
--   }

-- myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
--   [ 
--     ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
--     , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
--     , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
--     , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

--     , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
--     , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

--     , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

--     -- move focus up or down the window stack
--     , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
--     , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
--     , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
--     , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
--     , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

--     -- modifying the window order
--     , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
--     , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
--     , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

--     -- resizing the master/slave ratio
--     , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
--     , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

--     -- floating layer support
--     , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

--     -- increase or decrease number of windows in the master area
--     , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
--     , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

--     -- toggle the status bar gap
--     --, ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

--     -- quit, or restart
--     , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
--     , ((modMask              , xK_q     ), spawn "xmonad --recompile && xmonad --restart") -- %! Restart xmonad
--     ]
--     ++
--     -- mod-[1..9] %! Switch to workspace N
--     -- mod-shift-[1..9] %! Move client to workspace N
--     [((m .|. modMask, k), windows $ f i)
--         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--     ++
--     -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
--     -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
--     [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--         | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- main = xmonad $ gnomeConfig {
--   modMask = mod4Mask
--   , keys = myKeys
--   }