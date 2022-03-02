-- IMPORTS

-- CORE

import XMonad
import Data.Ratio
import Data.Monoid
import System.Exit
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

-- ACTIONS

import XMonad.Actions.Warp
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Actions.DwmPromote
import XMonad.Actions.MouseResize

-- HOOKS

import XMonad.ManageHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers

-- LAYOUTS

import XMonad.Layout.Spiral

-- LAYOUT MODIFIERS

import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.WindowArranger
import XMonad.Layout.MultiToggle.Instances

-- UTILITIES

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

-- For Polybar

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- User Set Variables

myGUIFileExplorer, myBrowser, myTerminal :: String

myTerminal        = "alacritty" -- Global Terminal         Variable
myBrowser         = "firefox"   -- Global Browser          Variable
myGUIFileExplorer = "nautilus"  -- Global GUI FileExplorer Variable

-- Change Focus To The Window Where The Mouse Is 

myFocusFollowsMouse :: Bool
myFocusFollowsMouse =  True

-- Weather To Only Focus With Mouse Click

myClickJustFocuses :: Bool
myClickJustFocuses =  False -- If myFocusFollowsMouse Is True Then This Should Be False By Default

myBorderWidth :: Dimension
myBorderWidth =  2 -- Border Width

-- Format Border

myFocusedBorderColor   = "#5E81AC" -- Focused Window
myUnfocusedBorderColor = "#3B4252" -- Unfocused Window

-- Mod Key

myModMask = mod4Mask -- mod1Mask Is "Alt" And mod4Mask Is "Super"  

-- WORKSPACES

myWorkspaces = ["  \62057  ", "  \61728  ", "  \61564  ", "  \61729  ", "  \61598  ", "  \61723  ", "  \62060  ", "  \61643  ", "  \61884  "]

-- SCRATCHPADS

myScratchPads =

  [

      NS "terminal"    spawnTerm findTerm manageTerm -- Alacritty
    , NS "spotify-tui" spawnSpt  findSpt  manageSpt  -- Spotify-TUI
    , NS "calculator"  spawnCalc findCalc manageCalc -- Qalculate

  ]

  where

    spawnTerm  = myTerminal ++ " -t scratchpad --class scratchpad,ScratchPad"
    findTerm   = resource   =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h

     where

        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnSpt  = myTerminal ++ " -t spt --class spt,SPT -e spt"
    findSpt   = resource   =? "spt"
    manageSpt = customFloating $ W.RationalRect l t w h

     where

        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h

     where

        h = 0.5
        w = 0.4
        t = 0.75 -h
        l = 0.70 -w


-- KEYBINDINGS

myKeys =

                [
                  -- CORE

                  ("M-S-c",              restart "xmonad" True)                                                                                                                  -- Recompile Xmonad
                , ("M1-<F4>",            io exitSuccess)                                                                                                                         -- Exit Xmonad

                 -- Actions                                                  

                , ("C-S-<Escape>",       spawn "/home/arbab/.config/i3lock/lock.sh")                                                                                             -- Custom Lockscript Using i3lock

                 -- KILL                                                     

                , ("M-q",                kill)                                                                                                                                   -- Kill Focused Window
                , ("M-C-q",              killAll)                                                                                                                                -- Kill All Windows On Workspace

                -- LAYOUTS                                                     

                , ("M-<Space>",          sendMessage NextLayout)                                                                                                                 -- Change Xmonad Layout  
                , ("M-n",                refresh)                                                                                                                                -- Restore Default Layouts
                , ("M-<Tab>",            moveTo Next NonEmptyWS)                                                                                                                 -- Cycle Through The Next Non-Empty Workspace
                , ("M-S-<Tab>",          moveTo Prev NonEmptyWS)                                                                                                                 -- Cycle Through The Previours Non-Empty Workspace
                , ("M-f",                sendMessage ( Toggle FULL ) >> sendMessage ToggleStruts)                                                                                -- Toggle FULLSCREEN Layout And Avoid Struts
                , ("M-C-f",              withFocused toggleFloat)                                                                                                                -- Toggle Float On Focused Window
                , ("M-.",                warpToWindow (1%10) (1%10))                                                                                                             -- Move Pointer To Focused Window                       

                -- LAYOUT WINDOW FOCUS                                                     

                , ("M-<Left>",           windows W.focusUp)                                                                                                                      -- Arrow Key <M-Left>  To Change Focus To The "Upper" Window
                , ("M-<Right>",          windows W.focusDown)                                                                                                                    -- Arrow Key <M-Right> To Change Focus To The "Down"  Window

                -- LAYOUT WINDOW SWAPS                                                     

                , ("M-S-<Left>",         windows W.swapUp)                                                                                                                       -- Arrow Key <M-S-Left>  To Swap To The "Upper" Window
                , ("M-S-<Right>",        windows W.swapDown)                                                                                                                     -- Arrow Key <M-S-Right> To Swap To The "Down"  Window
                , ("M-m",                dwmpromote)                                                                                                                             -- Swap Master Pane With Focused Window And If Focused Window Is Master, Swap With The Next Window In The Stack, Focus Stays On Master Pane

                -- MOVE WINDOWS                                                     

                , ("M-M1-<Up>",          sendMessage (MoveUp        10))                                                                                                         -- Arrow Key <M+ALT+Up>    To Move The Focused Window Position By 10 At The "Up"    Side
                , ("M-M1-<Down>",        sendMessage (MoveDown      10))                                                                                                         -- Arrow Key <M+ALT+Down>  To Move The Focused Window Position By 10 At The "Down"  Side
                , ("M-M1-<Left>",        sendMessage (MoveLeft      10))                                                                                                         -- Arrow Key <M+ALT+Left>  To Move The Focused Window Position By 10 At The "Left"  Side
                , ("M-M1-<Right>",       sendMessage (MoveRight     10))                                                                                                         -- Arrow Key <M+ALT+Right> To Move The Focused Window Position By 10 At The "Right" Side

                -- RESIZE WINDOWS   

                , ("M-C-<Up>",           sendMessage (IncreaseUp    10))                                                                                                         -- Arrow Key <M+C+Up>      To Increase The Focused Window Size By 10 At The "Up"    Side
                , ("M-C-<Down>",         sendMessage (IncreaseDown  10))                                                                                                         -- Arrow Key <M+C+Down>    To Increase The Focused Window Size By 10 At The "Down"  Side
                , ("M-C-<Left>",         sendMessage (IncreaseLeft  10))                                                                                                         -- Arrow Key <M+C+Left>    To Increase The Focused Window Size By 10 At The "Left"  Side
                , ("M-C-<Right>",        sendMessage (IncreaseRight 10))                                                                                                         -- Arrow Key <M+C+Right>   To Increase The Focused Window Size By 10 At The "Right" Side
                , ("M-<KP_Add>",         sendMessage (IncreaseUp    10) >> sendMessage (IncreaseDown  10) >> sendMessage (IncreaseLeft  10) >> sendMessage (IncreaseRight  10) ) -- Arrow Key <M+C+NumPad+> To Increase THe Focused Window Size By 10 At      All    Sides
                , ("M-S-C-<Up>",         sendMessage (DecreaseUp    10))                                                                                                         -- Arrow Key <M+S+C+Up>    To Decrease The Focused Window Size By 10 At The "Up"    Side
                , ("M-S-C-<Down>",       sendMessage (DecreaseDown  10))                                                                                                         -- Arrow Key <M+S+C+Down>  To Decrease The Focused Window Size By 10 At The "Down"  Side
                , ("M-S-C-<Left>",       sendMessage (DecreaseLeft  10))                                                                                                         -- Arrow Key <M+S+C+Left>  To Decrease The Focused Window Size By 10 At The "Left"  Side
                , ("M-S-C-<Right>",      sendMessage (DecreaseRight 10))                                                                                                         -- Arrow Key <M+S+C+Right> To Decrease The Focused Window Size By 10 At The "Right" Side
                , ("M-<KP_Subtract>",    sendMessage (DecreaseUp    10) >> sendMessage (DecreaseDown  10) >> sendMessage (DecreaseLeft  10) >> sendMessage (DecreaseRight  10) ) -- Arrow Key <M+C+NumPad+> To Decrease THe Focused Window Size By 10 At      All    Sides
                , ("M-<Up>",             sendMessage (IncMasterN    1))                                                                                                          -- Arrow Key <M+Up>   To Increase Windows In Master Pane
                , ("M-<Down>",           sendMessage (IncMasterN  (-1)))                                                                                                         -- Arrow Key <M+Down> To Decrease Windows In Master Pane

                -- RESIZE GAPS

                , ("M-C-<KP_Add>",       incWindowSpacing 10)                                                                                                                    -- Decrease Window Spacing
                , ("M-C-<KP_Subtract>",  decWindowSpacing 10)                                                                                                                    -- Increase Window Spacing
                , ("M-M1-<KP_Add>",      incScreenSpacing 10)                                                                                                                    -- Decrease Screen Spacing
                , ("M-M1-<KP_Subtract>", decScreenSpacing 10)                                                                                                                    -- Increase Screen Spacing

                -- SCRATCHPADS                                                  

                , ("M-s M-<Return>",     namedScratchpadAction myScratchPads "terminal")                                                                                         -- Spawn A Terminal As A ScratchPad
                , ("M-s s",              namedScratchpadAction myScratchPads "spotify-tui")                                                                                      -- Spawn A TUI Spotify Client As A ScratchPad
                , ("M-s c",              namedScratchpadAction myScratchPads "calculator")                                                                                       -- Spawn A Calculator As A ScratchPad

                -- FUNCTION KEYS                                                     

                , ("<XF86AudioPrev>",    spawn "playerctl previous")                                                                                                             -- Use "Fn+F5" With PlayerctlD To Play The Previous Media On The Last Active Player
                , ("<XF86AudioPlay>",    spawn "playerctl play-pause")                                                                                                           -- Use "Fn+F6" With PlayerctlD To Pause/Play Media On The Last Active Player
                , ("<XF86AudioNext>",    spawn "playerctl next")                                                                                                                 -- Use "Fn+F7" With PlayerctlD To Play The Next Media On The Last Active Player

                -- ROFI                                                     

                , ("M-d",                spawn "rofi -show drun -theme /home/arbab/.config/rofi/launcher/drun/launcher.rasi")                                                    -- Launcher -Drun
                , ("M-S-d",              spawn "rofi -show run -theme /home/arbab/.config/rofi/launcher/run/launcher.rasi")                                                      -- Launcher -Run
                , ("M-g",                spawn "/home/arbab/.config/rofi/screenshot/screenshot.sh")                                                                              -- Screenshot Menu Using Rofi
                , ("M-S-g",              spawn "/home/arbab/.config/rofi/screenshot/screenshot.sh --stop")                                                                       -- Stop Recording On The Screenshot Menu Using Rofi
                , ("M-S-q",              spawn "/home/arbab/.config/rofi/powermenu/powermenu.sh")                                                                                -- Powermenu Using Rofi

                -- APPLICATIONS                                                     

                , ("M-<Return>",         spawn myTerminal)                                                                                                                       -- Alacritty
                , ("M-b",                spawn myBrowser)                                                                                                                        -- Spawn Browser
                , ("M-z",                spawn myGUIFileExplorer)                                                                                                                -- Spawn FileManager
                , ("M-S-z",              spawn "thunar")                                                                                                                         -- Spawn Backup FileManager
                , ("M-r p",              spawn "/home/arbab/.config/polybar/scripts/launch.sh")                                                                                  -- Restart Polybar

                ]

         ++

         -- This Enables View Switching And Window Shifting

                  [("M" ++ m ++ ['-', k] , windows $ f i)
                       | (i, k) <- zip myWorkspaces ['1'..'9']
                       , (f, m) <- [(W.view, ""), (W.shift, "-S")]]

-- MOUSE BINDINGS

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList

    [

      -- Mod-Button1, Set The Window To Floating Mode And Move By Dragging


      ( (modm, button1),
        \w ->
            focus w >> mouseMoveWindow w
              >> windows W.shiftMaster
      ),

      -- Mod-Button2, Raise The Window To The Top Of The Stack

      ((modm, button2), \w -> focus w >> windows W.shiftMaster),

      -- Mod-Button3, Set The Window To Floating Mode And Resize By Dragging

      ( (modm, button3),
        \w ->
            focus w >> mouseResizeWindow w
              >> windows W.shiftMaster
      )

      -- You May Also Bind Events To The Mouse Scroll Wheel (button4 And button5)

    ]

-- LAYOUTS

-- MYLAYOUT VARIABLES

mySpacing n = spacingRaw True (Border n n n n) True (Border n n n n) True -- Window Spacing Where Integer "n" Is The Gap Between The Windows

-- BUNDLED LAYOUTS

spirals        = renamed [Replace "Spiral"]
                  $ mySpacing 5
                  $ spiral (6/7)

masterAndStack = renamed [Replace "MasterAndStack"]
                  $ mySpacing 5 
                    tiled

                      where

                        -- Default Tiling Algorithm Partitions The Screen Into Two Panes

                        tiled = Tall nmaster delta ratio

                        -- The Default Number Of Windows In The Master Pane

                        nmaster = 1

                        -- Default Proportion Of Screen Occupied By Master Pane

                        ratio = 1 / 2

                        -- Percent Of Screen To Increment By When Resizing Panes

                        delta = 3 / 100


myLayout =   avoidStruts
           $ mouseResize
           $ windowArrange
           $ smartBorders
           $ mkToggle (FULL ?? EOT)
           $ spirals
         ||| masterAndStack

-- WINDOW RULES

myManageHook =

  composeAll

    [

      -- FLOATS

       className  =? "confirm"                             --> doFloat
     , className  =? "file_progress"                       --> doFloat
     , className  =? "dialog"                              --> doFloat
     , className  =? "download"                            --> doFloat
     , className  =? "error"                               --> doFloat
     , className  =? "notification"                        --> doFloat
     , className  =? "pinentry-gtk-2"                      --> doFloat
     , className  =? "splash"                              --> doFloat
     , className  =? "toolbar"                             --> doFloat
     , className  =? "Lxpolkitr"                           --> doFloat
     , className  =? "Yad"                                 --> doCenterFloat
     , title      =? "Bulk Rename - Rename Multiple Files" --> doCenterFloat
     , isFullscreen                                        --> doFullFloat
     , (className =? "firefox" <&&> resource =? "Dialog")  --> doFloat       -- Float Firefox Dialog

     -- ASSIGN WORKSPACES

     , title      =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 0 )
     , className  =? "Alacritty"           --> doShift ( myWorkspaces !! 1 )
     , className  =? "Thunar"              --> doShift ( myWorkspaces !! 2 )
     , className  =? "Org.gnome.Nautilus"  --> doShift ( myWorkspaces !! 2 )
     , className  =? "Code"                --> doShift ( myWorkspaces !! 3 )
     , className  =? "Code - Insiders"     --> doShift ( myWorkspaces !! 3 )
     , className  =? "Steam"               --> doShift ( myWorkspaces !! 5 )
     , className  =? "Spotify"             --> doShift ( myWorkspaces !! 8 )

    ] <+> namedScratchpadManageHook myScratchPads


-- TOGGLE FLOAT FUNCTION

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s
    )

-- EVENTHOOK

myEventHook = fullscreenEventHook <+> docksEventHook <+> serverModeEventHook

-- LOGHOOK WITH DBUS FOR POLYBAR

myLogHook :: D.Client -> PP
myLogHook dbus =

  def

    -- Polybar Formatting 
    -- Check https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Hooks-DynamicLog.html#t:PP

    {

        ppOutput  = dbusOutput dbus
      , ppCurrent = wrap "%{u#5e81ac F#f3f4f5}%{+u}" "%{-u F-}"
      , ppUrgent  = wrap "%{F#db104e}" "%{F-}"
      , ppHidden  = wrap "%{F#abb2bf}" "%{F-}"
      , ppSort    = (.namedScratchpadFilterOutWorkspace) <$> ppSort defaultPP -- Filter Out "NPS" Workspace
      , ppOrder   = \(ws:l:t:_) -> [ws,"  ",l,"  "]                           -- Xmonad-Log Output With Workspaces And Current Layout
      , ppSep     = ""

    }

-- Xmobar-Log

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

-- STARTUP ACTIONS

myStartupHook =

  do

    spawnOnce "playerctld daemon"                                                             -- Playerctl Daemon
    spawnOnce "nitrogen --restore"                                                            -- Wallpaper Utility
    spawnOnce "rclone mount --daemon Drive_arbabashruff: $HOME/Mount/arbabashruff@gmail.com/" -- Mount Drive Account On Local Machine
    spawnOnce "xfce4-power-manager"                                                           -- Xfce Power Manager
    spawnOnce "lxsession"                                                                     -- Session Utility
    spawnOnce "volumeicon"                                                                    -- Pulseaudio Volume Manager In SysTray
    spawnOnce "nm-applet"                                                                     -- NetworkManager Systray Utility
    spawnOnce "kdeconnect-indicator"                                                          -- SysTray KDE-Indicator
    spawnOnce "/home/arbab/.config/dunst/scripts/load.sh"                                     -- Dunst Startup Script
    spawnOnce "picom --experimental-backends"                                                 -- Compositor
    spawnOnce "/home/arbab/.config/polybar/scripts/launch.sh"                                 -- Dock
    setWMName "LG3D"

main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ docks $ def
                    {
                      -- User Set Variables

                      terminal           = myTerminal,              -- Alacritty
                      focusFollowsMouse  = myFocusFollowsMouse,     -- True
                      clickJustFocuses   = myClickJustFocuses,      -- False
                      borderWidth        = myBorderWidth,           -- 2
                      modMask            = myModMask,               -- Super
                      workspaces         = myWorkspaces,            -- 1-9
                      normalBorderColor  = myUnfocusedBorderColor,  -- #3B4252
                      focusedBorderColor = myFocusedBorderColor,    -- #5E81AC

                      -- BINDINGS

                      mouseBindings = myMouseBindings,

                      -- HOOKS AND LAYOUTS

                      layoutHook      = myLayout,
                      manageHook      = myManageHook,
                      handleEventHook = myEventHook,
                      logHook         = dynamicLogWithPP (myLogHook dbus),
                      startupHook     = myStartupHook

                    } `additionalKeysP` myKeys
