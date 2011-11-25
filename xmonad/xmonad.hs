import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Graphics.X11.Xlib.Extras

import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad.Layout.WorkspaceDir as WD
import qualified XMonad.StackSet as W


-- Visual settings used by e.g. dmenu
myFont         = "xft:ProggyTiny:pixelsize=9"
myBgColor      = "#1b1d1e"
myFgColor      = "#bbbbbb"
mySelFgColor   = "#ffffff"
mySelBgColor   = "#333333"
myBorderColor  = "#40464b"
myFocusedColor = "#839cad"
myCurrentColor = "#cd5c5c"
myEmptyColor   = "#4c4c4c"
myHiddenColor  = "#dddddd"
myLayoutColor  = "#839cad"
myUrgentColor  = "#2b9ac8"
myTitleColor   = "#ffffff"
mySepColor     = "#58504c"


-- Some basic defaults
myWorkspaces  = ["main", "web", "dev", "misc"]
myTerminal    = "urxvt"
myBorderWidth = 2
myModMask     = mod4Mask
myStartupHook = setWMName "LG3D"


-- All types of layouts
myLayoutHook = lessBorders OnlyFloat $ grid ||| tall ||| full
  where myNamed n = named n . avoidStruts
        grid      = myNamed "grid" Grid
        tall      = myNamed "tall" $ Tall 1 (3/100) (1/2)
        full      = myNamed "full" Full


-- Rules to handle the windows
myManageHook = composeAll $
    [ isFullscreen --> (doF W.focusDown <+> doFullFloat)
    , isDialog     --> doFloat
    , transience'
    , className =? "MPlayer" --> doFloat <+> doF copyToAll
    , className =? "Smplayer" --> doFloat <+> doF copyToAll
    , className =? "stalonetray" --> doIgnore
    ] ++
    [className =? n --> doFloat | n <- dialogCFs] ++
    [title =? n --> doFloat | n <- dialogNFs]
  where
    dialogCFs = ["Pinentry-gtk-2"]
    dialogNFs = ["Ordner wählen"]


-- All keyboard shortcuts
myKeys conf = mkKeymap conf $
    [ ("M-q", kill)
    , ("M-e", spawn dmenu)
    , ("M-<Return>", spawn $ terminal conf)
    , ("M-S-q", spawn "exec killall stalonetray" >> restart "xmonad" True)
    , ("M-m", windows W.shiftMaster)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-,", sendMessage Shrink)
    , ("M-.", sendMessage Expand)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-<Tab>", windows W.focusDown)
    , ("M-S-<Tab>", windows W.focusUp)
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    ]
    ++
    [ (m ++ [i], f w) | (i, w) <- zip ['1'..] $ workspaces conf
                      , (m, f) <- [ ("M-", windows . W.greedyView)
                                  , ("M-S-", windows . W.shift)
                                  ]
    ]
  where
    dmenu = "dmenu_run -fn \"" ++ myFont ++ "\" -nb \"" ++ myBgColor ++
            "\" -nf \"" ++ myFgColor ++ "\" -sb \"" ++ mySelBgColor ++
            "\" -sf \"" ++ mySelFgColor ++ "\" -p Run"


-- A simple xmobar statusbar controlled by XMonad
myStatusBar = statusBar "xmobar" myPP toggleStrutsKey
  where
    myPP = defaultPP
         { ppCurrent = xmobarColor myCurrentColor ""
         , ppHidden = xmobarColor myHiddenColor ""
         , ppHiddenNoWindows = xmobarColor myEmptyColor ""
         , ppUrgent = xmobarColor myUrgentColor ""
         , ppLayout = xmobarColor myLayoutColor ""
         , ppWsSep = "  "
         , ppSep = xmobarColor mySepColor "" "   |   "
         , ppTitle = xmobarColor myTitleColor "" . shorten 120 . trim
         }
    toggleStrutsKey XConfig {modMask = modm} = (modm, xK_b)


main = do
    stalonetray <- spawnPipe "stalonetray"
    xmonad . withUrgencyHook NoUrgencyHook =<< myStatusBar defaultConfig
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

