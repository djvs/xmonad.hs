-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

--util
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Themes 

import Data.String.Utils

import XMonad.Prompt
import XMonad.Prompt.Theme
-- actions
import XMonad.Actions.GridSelect
--import XMonad.Actions.Volume

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.DwmStyle
import XMonad.Layout.SimpleFloat
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Themes
-- import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.CycleWindows


-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
  where 
    uhook = withUrgencyHookC NoUrgencyHook urgentConfig
    cmd = "bash -c \"tee >(xmobar -x0)\""
    pp = customPP
    kb = toggleStrutsKey
    conf = uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , layoutHook = layoutHook' 
                         , manageHook = manageHook' <+> doFloat
						 , startupHook = setWMName "LG3D"
                         }




-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         , className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> doFloat
                         , className =? "Vlc"       --> doFloat
						 , className =? "Firefox"   --> doFloat
						 , className =? "Pidgin"    --> doFloat
						 , className =? "Gajim"		--> doFloat
						 , className =? "Gedit"		--> doFloat
						 , className =? "JDownloader"		--> doFloat
			 --, insertPosition Below Newer
			 , transience'
                         ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#429942" "" . wrap "{" "}"
                     , ppHidden = xmobarColor "#DDDDDD" "" . wrap "(" ")"
                     , ppHiddenNoWindows = xmobarColor "#FFFFFF" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "!!" "!!" 
                     , ppLayout = xmobarColor "#C9A34E" ""
                     , ppTitle =  xmobarColor "#C9A34E" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 480 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 0
normalBorderColor'  = "#3F3F3F"
focusedBorderColor' = "#5F5F5F"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"]
			  ++ ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12", "FP", "FI", "FD"]

-- layouts

layoutHook' = simpleDeco shrinkText (myTheme) layoutDef' 

myTheme = defaultTheme { activeColor         = "#1F1F1F"
                       , inactiveColor       = "#000000"
                       , activeBorderColor   = "#000000"
                       , inactiveBorderColor = "#000000"
                       , activeTextColor     = "#bbbbbb"
                       , inactiveTextColor   = "#999999"
                       , decoHeight          = 14
                       , decoWidth           = 2000 
                       }


layoutDef' = mrtile ||| tile ||| mtile ||| tab ||| full ||| tcol ||| spir 
  where
    mrtile = smartBorders mouseResizableTile{draggerType = FixedDragger {gapWidth = 3, draggerWidth = 3}}
    tcol = ThreeCol 2 (3/100) (1/2) 
    spir = spiral (4/3)
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = renamed [Replace "[]="] $ smartBorders rt
    mtile = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
    tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full = renamed [Replace "[]"] $ noBorders Full 

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), safeSpawn "urxvt" []) 
    , ((modMask,               xK_p     ), safeSpawn "dmenu_run" []) 
    , ((modMask .|. shiftMask, xK_p     ), safeSpawn "gmrun" [])
    , ((modMask .|. shiftMask, xK_m     ), safeSpawn "claws-mail" [])
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)
    
    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- window selector
    , ((modMask                , xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
    , ((modMask                , xK_z), rotOpposite)
    , ((modMask                , xK_i), rotUnfocusedUp)
    , ((modMask                , xK_u), rotUnfocusedDown)
    , ((modMask .|. controlMask, xK_i), rotFocusedUp)
    , ((modMask .|. controlMask, xK_u), rotFocusedDown)

    , ((controlMask .|. shiftMask, xK_Print), spawn "scrot -u")
    , ((shiftMask              , xK_Print), spawn "scrot")

    -- theme selector
    , ((modMask .|. controlMask, xK_t), themePrompt myXPConfig)

    -- SPECIAL KEYS
--    , ((0, xF86XK_AudioRaiseVolume), spawn "/usr/bin/alsavol -i 2")
--    , ((0, xF86XK_AudioLowerVolume), spawn "/usr/bin/alsavol -d 2")
    , ((0, xF86XK_AudioMute), spawn "/usr/bin/alsavol -t")
    , ((0, xF86XK_AudioPrev), spawn "mpc prev")
    , ((0, xF86XK_AudioNext), spawn "mpc next")
    , ((0, xF86XK_AudioPlay), spawn "togglempdplay")
    , ((shiftMask, xF86XK_AudioMedia), spawn "mpc add /")
    , ((0, xF86XK_AudioMedia), spawn "urxvt -e ncmpcpp")
    , ((0, xF86XK_HomePage), spawn "xscreensaver-command -lock")
    , ((0, xF86XK_Calculator), spawn "extcalc")

	-- volume controls
    , ((modMask, xF86XK_AudioLowerVolume     ), runProcessWithInput "alsavol2" ["-s","5","down"] [] >>= newalert)
    , ((modMask, xF86XK_AudioRaiseVolume     ), runProcessWithInput "alsavol2" ["-s","5","up"] [] >>= newalert)

    , ((shiftMask, xF86XK_AudioLowerVolume     ), runProcessWithInput "alsavol" ["-s","16","down"] [] >>= newalert)
    , ((shiftMask, xF86XK_AudioRaiseVolume     ), runProcessWithInput "alsavol" ["-s","16","up"] [] >>= newalert)

    , ((0, xF86XK_AudioLowerVolume     ), runProcessWithInput "alsavol" ["-s","1.5","down"] [] >>= newalert)
    , ((0, xF86XK_AudioRaiseVolume     ), runProcessWithInput "alsavol" ["-s","1.5","up"] [] >>= newalert)



    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_p     ), spawn "tp")
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ [xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0, xK_minus, xK_equal, xK_BackSpace] ++ [xK_F1 .. xK_F12] ++ [xK_Print, xK_Insert, xK_Delete] 
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [0..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------

newalert x = spawn $ "killall notify-osd; notify-send -t 200 --icon=" ++ switchIcon (read $ replace "\n" "" $ replace "%" "" x :: Int) ++ " 'Volume (vmix0-outvol) changed to " ++ x ++ "'"

clamp x = (x `max` 0) `min` 100

switchIcon :: Int -> String
switchIcon c  
    | c == 0 = "audio-volume-muted"
    | c <= 33 = "audio-volume-low"
    | c <= 66 = "audio-volume-medium"
    | c < 100 = "audio-volume-high"
    | c == 100 = "emblem-cool"


myXPConfig =
    XPC { font              = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , bgColor           = "#000000"
        , fgColor           = "#DDDDDD"
        , fgHLight          = "#FFFFFF"
        , bgHLight          = "#333333"
        , borderColor       = "#FFFFFF"
        , promptBorderWidth = 0
        , position          = Bottom
        , height            = 16
        , historySize       = 256
        , defaultText       = ""
        , autoComplete      = Nothing
        , historyFilter     = id
        , showCompletionOnTab = False
        , alwaysHighlight = False
        }
