{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}


-- XMonad configuration file by Thomas ten Cate <ttencate@gmail.com>
-- Edited and extended by Robert Massaioli <robertmassaioli@gmail.com>
--
-- Works on whatever XMonad that I happen to be running, usually the latest one.
-- You will need xmonad-contrib and maybe more.
--
-- This is designed to play nice with a standard Ubuntu installation.
--
-- WARNING: On my computers I swap CapsLock and BackSpace around so if you don't then you will need
-- to swap those around in this config.

import XMonad
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as S
import XMonad.Actions.CycleWS
import XMonad.Config.Xfce
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Util.WindowProperties
import Control.Monad
import Data.Ratio
import qualified Data.Map as M
import XMonad.Util.Run
import XMonad.Util.Dmenu
import XMonad.Util.Dzen
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Layout.IndependentScreens
import XMonad.Actions.Warp
import XMonad.Hooks.UrgencyHook
import System.Exit

import XMonad.Prompt
import XMonad.Actions.TagWindows

import XMonad.Actions.OnScreen (greedyViewOnScreen, viewOnScreen)
import Data.Maybe (fromMaybe)

import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- display
-- replace the bright red border with a more stylish colour
myBorderWidth = 2
myNormalBorderColor = "white"
myFocusedBorderColor = "purple"

-- workspaces
myWorkspaces = map show [1..9]

-- layouts
basicLayout = Tall nmaster delta ratio
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 2

threeCol = ThreeCol nmaster delta ratio
  where
    nmaster = 1
    delta   = 3 / 100
    ratio   = 1 / 3

tallLayout    = named "tall" . avoidStruts $ basicLayout
threeLayout   = named "three" . avoidStruts $ threeCol
wideLayout    = named "wide" . avoidStruts $ Mirror basicLayout
singleLayout  = named "single" . avoidStruts $ noBorders Full

myLayoutHook  = tallLayout ||| threeLayout ||| wideLayout ||| singleLayout

-- Mod4 is the Super / Windows key
-- alt, is well...alt
myModMask = mod4Mask
altMask   = mod1Mask

{-
 - Default Spawn Commands
 - Update these using update alternatives as in:
 -    $ update-alternatives --config x-www-browser
 -}
myBrowser = "x-www-browser"
myTerminal = "x-terminal-emulator"

-- choose which menu runs
runMenu = spawn "dmenu_run"
-- runMenu = gnomeRun

-- better keybindings for dvorak
myKeys conf = M.fromList $
    [ ((myModMask              , xK_Return    ), spawn myTerminal)
    , ((myModMask              , xK_x         ), spawn myBrowser)
    , ((myModMask              , xK_r         ), runMenu)
    , ((myModMask              , xK_c         ), kill)
    -- Workspace Movement
    , ((altMask                , xK_space     ), viewEmptyWorkspace)
    , ((altMask .|. shiftMask  , xK_space     ), tagToEmptyWorkspace)
    , ((myModMask              , xK_Tab       ), nextWS)
    , ((myModMask .|. shiftMask, xK_Tab       ), prevWS)
    -- Layout Commands
    , ((myModMask              , xK_space     ), sendMessage NextLayout)
    , ((altMask .|. shiftMask  , xK_Return    ), sendMessage FirstLayout)
    , ((myModMask              , xK_n         ), refresh)
    , ((myModMask              , xK_m         ), windows S.swapMaster)

    -- controlling window movement, position and location
    , ((altMask                , xK_Tab       ), windows S.focusDown >> windowCenter)
    , ((altMask .|. shiftMask  , xK_Tab       ), windows S.focusUp >> windowCenter)

    , ((myModMask              , xK_Down      ), windows S.swapDown)
    , ((myModMask              , xK_Up        ), windows S.swapUp)
    , ((myModMask              , xK_Left      ), sendMessage Shrink)
    , ((myModMask              , xK_Right     ), sendMessage Expand)
    , ((myModMask              , xK_u         ), withFocused $ windows . S.sink)
    , ((myModMask              , xK_w         ), sendMessage (IncMasterN 1))
    , ((myModMask              , xK_v         ), sendMessage (IncMasterN (-1)))
    -- Application commands
    , ((myModMask              , xK_f         ), spawn "firefox -p -no-remote")
    , ((myModMask              , xK_g         ), spawn "google-chrome --incognito")
    -- Shutdown commands
    , ((myModMask              , xK_q         ), restart "xmonad" True)
    , ((myModMask              , xK_h         ), spawn "gksudo pm-hibernate")
    , ((myModMask .|. shiftMask, xK_q         ), spawn "gksudo lshutdown -P now")
    , ((myModMask              , xK_l         ), spawn "xflock4")
    -- Print Scree    , ((myModMask              , xK_Print     ), spawn "gnome-screenshot")
    , ((altMask                , xK_Print     ), spawn "gnome-screenshot -a")
    -- MPC and Volume commands
    , ((myModMask               , xK_Page_Up), spawn "mpc next")
    , ((myModMask               , xK_Page_Down), spawn "mpc prev")
    , ((myModMask               , xK_Pause), spawn "mpc toggle")
    , ((myModMask               , xK_Home), spawn "amixer -c0 -- sset Master Playback 2dB+")
    , ((myModMask               , xK_End), spawn "amixer -c0 -- sset Master Playback 2dB-")

    -- Screen Movement
    , ((controlMask              , xK_Tab       ), nextScreen >> windowCenter)
    , ((controlMask .|. shiftMask, xK_Tab       ), prevScreen >> windowCenter)

    , ((altMask .|. controlMask, xK_Left       ), prevScreen >> windowCenter)
    , ((altMask .|. controlMask, xK_Right      ), nextScreen >> windowCenter)

    , ((altMask .|. controlMask, xK_Down       ), shiftPrevScreen)
    , ((altMask .|. controlMask, xK_Up         ), shiftNextScreen)
    , ((altMask .|. controlMask .|. shiftMask, xK_Down       ), shiftPrevScreen >> prevScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Up         ), shiftNextScreen >> nextScreen >> windowCenter)
    , ((altMask .|. controlMask .|. shiftMask, xK_Left     ), swapPrevScreen) -- this does not work properly
    , ((altMask .|. controlMask .|. shiftMask, xK_Right     ), swapNextScreen)
    , ((myModMask              , xK_z         ), windowCenter)
    -- Tagging Windows
    , ((myModMask              ,   xK_t  ), tagPrompt defaultXPConfig (withFocused . addTag))
    , ((myModMask .|. shiftMask,   xK_t  ), tagDelPrompt defaultXPConfig)
    , ((altMask                ,   xK_t  ), tagPrompt defaultXPConfig (`withTaggedGlobalP` gotoWindow))
    ] ++ [
      ((myModMask              , key), (windows $ S.greedyView ws) ) | (ws, key) <- workspaceKeys
    ] ++ [
      ((altMask                , key), (windows $ S.shift ws)) | (ws, key) <- workspaceKeys
    ] ++ [
      ((myModMask .|. altMask  , key), (windows $ S.shift ws) >> (windows $ S.greedyView ws) >> windowCenter)
      | (ws, key) <- workspaceKeys
    ]
    where
        workspaceKeys     = zip myWorkspaces [xK_F1 .. xK_F9]
        windowsShift      = windows . onCurrentScreen S.shift
        windowsGreedyView = windows . onCurrentScreen S.greedyView
        windowCenter = warpToWindow (1 % 6) (1 % 6)

        gotoWindow :: Window -> WindowSet -> WindowSet
        gotoWindow window ws = case S.findTag window ws of
                                 Just i -> viewOnScreen (screenIdFromTag i) i ws
                                 Nothing -> ws
            where
               screenIdFromTag :: WorkspaceId -> ScreenId
               screenIdFromTag = S . read . takeWhile (/= '_')

-- mouse bindings that mimic Gnome's
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((altMask, button1), \w -> focus w >> mouseMoveWindow w)
    , ((altMask, button2), \w -> focus w >> mouseResizeWindow w)
    , ((altMask, button3), \w -> focus w >> withFocused (windows . S.sink))
    , ((altMask, button4), const $ windows S.swapUp)
    , ((altMask, button5), const $ windows S.swapDown)
    ]

-- put it all together
main = xmonad $ xfceConfig
  { modMask             = myModMask
  , workspaces          = myWorkspaces
  , layoutHook          = myLayoutHook
  , borderWidth         = myBorderWidth
  , normalBorderColor   = myNormalBorderColor
  , focusedBorderColor  = myFocusedBorderColor
  , keys                = myKeys
  , mouseBindings       = myMouseBindings
  }