import System.Directory

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myHome = "/home/b52"
myFont = "ProggyTiny-7"
myBgColor = "#1b1d1e"
myFgColor = "#bbbbbb"
myBorderColor = "#292c2d"
myFocusedColor = "#57666a"
myCurrentColor = "#cd5c5c"
myEmptyColor = "#4c4c4c"
myHiddenColor = "#dddddd"
myIcon name = myHome ++ "/.xmonad/icons/" ++ name ++ ".xbm"
myLayout name = myIcon "layout-" ++ name

myWorkspaces = ["main", "web", "dev", "misc"]
myTerminal = "urxvt"
myBorderWidth = 1
myModMask = mod4Mask

myStartupHook = setWMName "LG3D"

myLayoutHook = smartBorders . layoutHints . avoidStruts . gaps [(U, 3), (D, 3), (R, 3), (L, 3)] . spacing 3 $
                 named "grid" Grid ||| named "tall" (Tall 1 (3/100) (1/2)) ||| named "full" Full

myManageHook = composeAll $
                [ className =? "MPlayer" --> doFloat <+> doF copyToAll
                , className =? "Firefox" --> doShift "web"
                , resource =? "wicd-client.py" --> doFloat
                , resource =? "DTA" --> doFloat <+> doShift "misc"
                , className =? "org-igoweb-cgoban-CGoban" --> doFloat
                ]
                ++
                [ className =? n --> doFloat | n <- ["Dialog", "Download", "DTA", "Pinentry-gtk-2"]]

myKeys conf = mkKeymap conf $
               [ ("M-q", kill)
               , ("M-e", spawn "dmenu_run -l 8 -x 50 -y 50 -w 150 -fn \"xft:ProggyTiny-7\" -nb \"#1B1D1E\" -nf \"#a0a0a0\" -sb \"#333\" -sf \"#fff\" -p Run")
               , ("M-<Return>", spawn $ XMonad.terminal conf)
               , ("M-S-q", spawn "exec killall dzen2" >> restart "xmonad" True)
               , ("M-m", windows W.shiftMaster)
               , ("M-t", withFocused $ windows . W.sink)
               , ("M-,", sendMessage Shrink)
               , ("M-.", sendMessage Expand)
               , ("M-l", goToSelected defaultGSConfig)
               , ("M-<Space>", sendMessage NextLayout)
               ]
               ++
               [ (m ++ k, f i)
                  | (i, k) <- zip ((\ws -> last ws : ws) . workspaces $ conf)
                                   ("^" : map show ([1..9] ++ [0]))
                  , (m, f) <- [ ("M-"    , windows . W.greedyView)
                              , ("M-S-"  , windows . W.shift)
                              ]
               ]


myStatusBar = "dzen2 -x 0 -y 0 -h 16 -w 640 -ta l -fn " ++ myFont ++ " -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""
myResourceBar = "conky -c ~/.xmonad/conky.conf | dzen2 -x 640 -y 0 -h 16 -w 640 -ta r -fn " ++ myFont ++ " -bg \"" ++ myBgColor ++ "\" -fg \"" ++ myFgColor ++ "\""
myLogHook h = dynamicLogWithPP $ defaultPP
                                  { ppOutput = hPutStrLn h
                                  , ppCurrent = wrap ("^bg(" ++ myBgColor ++ ")^i(" ++ myIcon "corner" ++ ")^fg(" ++ myCurrentColor ++ ")") "^fg()"
                                  , ppHidden = wrap ("^i(" ++ myIcon "corner" ++ ")^fg(" ++ myHiddenColor ++ ")") "^fg()"
                                  , ppHiddenNoWindows = wrap ("^i(" ++ myIcon "corner" ++ ")^fg(" ++ myEmptyColor ++ ")") "^fg()"
                                  , ppUrgent = wrap ("^i(" ++ myIcon "corner" ++ ")^fg(" ++ myHiddenColor ++ ")") "^fg()"
                                  , ppLayout = wrap "^i(" ")" . myLayout
                                  , ppWsSep = "  "
                                  , ppSep = "   "
                                  , ppTitle = shorten 80
                                  }

main = do
        status <- spawnPipe myStatusBar
        resource <- spawnPipe myResourceBar
        xmonad $ defaultConfig
                  { workspaces = myWorkspaces
                  , terminal = myTerminal
                  , borderWidth = myBorderWidth
                  , modMask = myModMask
                  , normalBorderColor = myBorderColor
                  , focusedBorderColor = myFocusedColor
                  , keys = myKeys
                  , layoutHook = myLayoutHook
                  , logHook = myLogHook status
                  , startupHook = myStartupHook
                  , manageHook = myManageHook
                  }
