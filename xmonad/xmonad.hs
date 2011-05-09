import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces as DW
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
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
import XMonad.Layout.WorkspaceDir as WD
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Graphics.X11.Xlib.Extras

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myHome = "/home/b52"
myFont = "ProggyTiny-7"
myBgColor = "#1b1d1e"
myFgColor = "#bbbbbb"
myBorderColor = "#292c2d"
myFocusedColor = "#839cad"-- "#505050"
myCurrentColor = "#cd5c5c"
myEmptyColor = "#4c4c4c"
myHiddenColor = "#dddddd"
myLayoutColor = "#839cad"
myUrgentColor = "#2b9ac8"
myTitleColor = "#ffffff"
mySepColor = "#58504c"
myIcon name = myHome ++ "/.xmonad/icons/" ++ name ++ ".xbm"

myWorkspaces = ["main", "web", "dev", "misc"]
myTerminal = "urxvt"
myBorderWidth = 2
myModMask = mod4Mask

myStartupHook = setWMName "LG3D"

myLayoutHook = lessBorders OnlyFloat $ grid ||| tall ||| full
             where myNamed n l = named n $ {- layoutHints . -}avoidStruts {- . gaps [(U, 2), (D, 2), (R, 2), (L, 2)] . spacing 1 -} $ l
                   grid = myNamed "grid" Grid
                   tall = myNamed "tall" (Tall 1 (3/100) (1/2))
                   full = myNamed "full" Full

myManageHook = composeAll $
                [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
                , isDialog     --> doFloat
                , transience'
                , className =? "MPlayer" --> doFloat <+> doF copyToAll
                , className =? "Firefox" --> doShift "web"
                , ((className =? "Firefox") <&&> (resource =? "Download")) --> doFloat <+> doShift "misc"
                , ((className =? "Firefox") <&&> (resource =? "DAT")) --> doFloat <+> doShift "misc"
                , resource =? "wicd-client.py" --> doFloat
                , resource =? "DTA" --> doFloat <+> doShift "misc"
                , className =? "org-igoweb-cgoban-CGoban" --> doFloat
                ]
                ++
                [ className =? n --> doFloat | n <- ["Dialog", "Download", "DTA", "Pinentry-gtk-2"]]

myKeys conf = mkKeymap conf $
               [ ("M-q", kill)
               , ("M-e", spawn "dmenu_run -fn \"xft:ProggyTiny-7\" -nb \"#1B1D1E\" -nf \"#a0a0a0\" -sb \"#333\" -sf \"#fff\" -p Run")
               , ("M-<Return>", spawn $ XMonad.terminal conf)
               , ("M-S-q", spawn "exec killall dzen2" >> restart "xmonad" True)
               , ("M-m", windows W.shiftMaster)
               , ("M-t", withFocused $ windows . W.sink)
               , ("M-,", sendMessage Shrink)
               , ("M-.", sendMessage Expand)
               , ("M-l", goToSelected defaultGSConfig)
               , ("M-<Space>", sendMessage NextLayout)
               , ("<XF86MonBrightnessDown>", spawn "~/test.sh brightness")
               , ("<XF86MonBrightnessUp>", spawn "~/test.sh brightness")
               , ("<XF86AudioMute>", spawn "amixer set Master toggle; ~/test.sh audio")
               , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-; ~/test.sh audio")
               , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+; ~/test.sh audio")
               ]
               ++
               [ (m ++ k, f i)
                  | (i, k) <- zip ((\ws -> last ws : ws) . workspaces $ conf)
                                   ("^" : map show ([1..9] ++ [0]))
                  , (m, f) <- [ ("M-"    , windows . W.greedyView)
                              , ("M-S-"  , windows . W.shift)
                              ]
               ]


--myStatusBar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
myStatusBar = statusBar "xmobar" pp toggleStrutsKey
  where
    pp = defaultPP
             { ppCurrent = xmobarColor myCurrentColor ""
             , ppHidden = xmobarColor myHiddenColor ""
             , ppHiddenNoWindows = xmobarColor myEmptyColor ""
             , ppUrgent = xmobarColor myUrgentColor ""
             , ppLayout = xmobarColor myLayoutColor ""
             , ppWsSep = "  "
             , ppSep = xmobarColor mySepColor "" "   |   "
             , ppTitle = xmobarColor myTitleColor "" . shorten 120 . trim
             }
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)
    

{-
                                  { ppOutput = hPutStrLn h
                                  , ppCurrent = corner . fg myCurrentColor
                                  , ppHidden = corner . fg myHiddenColor
                                  , ppHiddenNoWindows = corner . fg myEmptyColor
                                  , ppUrgent = corner . fg myUrgentColor . dzenStrip
                                  , ppLayout = fg myLayoutColor . layout
                                  , ppWsSep = "  "
                                  , ppSep = "     "
                                  , ppTitle = fg myFgColor . dzenEscape . shorten 100 . trim
                                  }
            where fg c = dzenColor c ""
                  icon n = "^i(" ++ (myIcon n) ++ ")"
                  corner = (++) (icon "corner")
                  layout n = icon ("layout-" ++ n)
            -}

--xmonad $ withUrgencyHook NoUrgencyHook
main = do xmonad =<< myStatusBar defaultConfig
                  { workspaces = myWorkspaces
                  , terminal = myTerminal
                  , borderWidth = myBorderWidth
                  , modMask = myModMask
                  , normalBorderColor = myBorderColor
                  , focusedBorderColor = myFocusedColor
                  , keys = myKeys
                  , layoutHook = myLayoutHook
                  , startupHook = myStartupHook
                  , manageHook = myManageHook
                  }
