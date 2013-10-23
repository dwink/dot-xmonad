import System.IO
import System.Posix.Env

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Core
import XMonad.Config
import XMonad.Operations

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeWindows

import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce


import qualified Data.Map as M

myTerminal = "uxterm"

myModMask = mod4Mask

myManageHook = composeAll
        [  className =? "stalonetray"	--> doIgnore
        ,  className =? "Gimp"			--> doFloat <+> doShift "8"
        ,  className =? "Pidgin" --> doShift "1"
        ,  classNotRole ("Pidgin", "buddy_list") --> doFloat
        ]
        where
          classNotRole :: (String, String) -> Query Bool
          classNotRole (c,r) = className =? c <&&> role /=? r

          role = stringProperty "WM_WINDOW_ROLE"

myFadeHook = composeAll [ isUnfocused --> opacity 0.85
                        , isFloating --> opacity 0.75
                        , className =? "UXTerm" --> opacity 0.8
                        ]

--keybindings for adjusting the non-Master tiles' size
resizeKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_a), sendMessage MirrorShrink)
	,  ((modm, xK_z), sendMessage MirrorExpand)
	]

--keybindings for handling urgency hints
urgentKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_BackSpace), focusUrgent)
	,  ((modm .|. shiftMask, xK_BackSpace), clearUrgents)
	]

customKeys conf@(XConfig {XMonad.modMask = modm}) =
	[  ((modm, xK_F12), spawn "xscreensaver-command -lock") 
	,  ((modm, xK_Left), prevWS)
	,  ((modm, xK_Right), nextWS)
	]

--merge all the keybindings together.
myKeys x = M.unions [ (keys defaultConfig x)
                    , (M.fromList (resizeKeys x))
                    , (M.fromList (urgentKeys x))
                    , (M.fromList (customKeys x))
                    ]

myStartupHook = setDefaultCursor xC_left_ptr
                <+> mapM_ spawnOnce [ "xscreensaver -no-splash"
                                    , "stalonetray"
                                    , "nm-applet"
                                    , "kwalletd"
                                    , "dropbox start"
                                    , "SpiderOak"
                                    , "unagi"
                                    , "feh --bg-scale `feh -U -z Dropbox/DigitalBlasphemy | head -1`"
                                    ]

myFont = "Inconsolata"
myFgColor = "#DCDCCC"
myBgColor = "#3f3f3f"
myStatusBar = "xmobar /home/dharks/.xmobarrc"
myDzenStatusBar = "dzen2 -x '0' -y '0' -h '24' -w '1728' -ta 'l' -fg '#FFFFFF' -bg '#000000'"

myLayoutHook = avoidStruts $ onWorkspace "1" imLayout $ standardLayouts
               where standardLayouts = tiled ||| Mirror tiled ||| Full
                     tiled = ResizableTall nmaster delta ratio []
                     imLayout = withIM (1/10) (Role "buddy_list") (standardLayouts) 
                     nmaster = 1 
                     delta = 0.03
                     ratio = 0.5

--myLogHook :: Handle -> X ()
myLogHook myStatusBarPipe = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn myStatusBarPipe
                                                        , ppUrgent = xmobarColor "white" "red"
                                                        , ppTitle = xmobarColor "purple" "" . shorten 120
							}

myDzenLogHook myStatusBarPipe = dynamicLogWithPP $ dzenPP
  { 
      ppOutput = hPutStrLn myStatusBarPipe
    , ppCurrent = dzenColor "#ebac54" "#ffffff" . pad
  }

main = do
	myStatusBarPipe <- spawnPipe myStatusBar
	xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig { 
                  terminal	= myTerminal,
	  	  modMask	= myModMask,
		  layoutHook	= myLayoutHook,
		  manageHook	= myManageHook,
		  keys		= myKeys,
		  startupHook	= myStartupHook,
                  logHook	= do
                                    fadeWindowsLogHook myFadeHook
                                    myLogHook myStatusBarPipe
		}
