{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
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
import XMonad.Hooks.WindowSwallowing

-- LAYOUTS

import XMonad.Layout.Spiral

-- LAYOUT MODIFIERS

import XMonad.Layout.Hidden
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import XMonad.Layout.MultiToggle.Instances

-- UTILITIES

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

import XMonad.Util.WorkspaceCompare

-- For Polybar

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Text.XHtml (title)

-- User Set Variables

myEmacs, myGUIFileExplorer, myBrowser, myTerminal, myMPDClient, myGUIMusicApp, myCalculator :: String
myTerminal        = "alacritty"     -- Global Terminal Variable
myBrowser         = "firefox"       -- Global Browser Variable
myGUIFileExplorer = "thunar"        -- Global GUI FileExplorer Variable
myMPDClient       = "ncmpcpp"       -- Global MPD Client Variable
myGUIMusicApp     = "spotify"       -- Global GUI Music App Variable
myCalculator      = "qalculate-gtk" -- Global Calculator Variable
myEmacs           = "emacsclient -c -a 'emacs'"


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

myWorkspaces = [
    "%{A1:wmctrl -s 0:}  1  %{A}"
  , "%{A1:wmctrl -s 1:}  2  %{A}"
  , "%{A1:wmctrl -s 2:}  3  %{A}"
  , "%{A1:wmctrl -s 3:}  4  %{A}"
  , "%{A1:wmctrl -s 4:}  5  %{A}"
  , "%{A1:wmctrl -s 5:}  6  %{A}"
  , "%{A1:wmctrl -s 6:}  7  %{A}"
  , "%{A1:wmctrl -s 7:}  8  %{A}"
  , "%{A1:wmctrl -s 8:}  9  %{A}"
  ]

-- SCRATCHPADS

myScratchPads =

  [

      NS "terminal"    spawnTerminal    findTerminal    manageTerminal    -- Alacritty
    , NS "fileManager" spawnFileManager findFileManager manageFileManager -- ls
    , NS "musicPlayer" spawnMusicPlayer findMusicPlayer manageMusicPlayer -- NCMPCPP And MPD

  ]

  where

    -- The Flags Are To Be Changed Depending On The Terminal

    spawnTerminal  = myTerminal ++ " " ++ "-t" ++ " " ++ "Terminal" ++ " " ++ "--class" ++ " " ++ "scratchpadterminal,ScratchPadTerminal"
    findTerminal   = XMonad.ManageHook.title =? "Terminal"
    manageTerminal = customFloating $ W.RationalRect l t w h

     where

        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    -- The Flags Are To Be Changed Depending On The Terminal

    spawnFileManager  = myTerminal ++ " " ++ "-t" ++ " " ++ "FileManager"  ++ " " ++ "--class" ++ " " ++ "lf,LF" ++ " " ++ "-e" ++ " " ++ "$HOME/local/bin/lfub"
    findFileManager   = XMonad.ManageHook.title =? "FileManager"
    manageFileManager = customFloating $ W.RationalRect l t w h

     where

        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

    -- The Flags Are To Be Changed Depending On The Terminal

    spawnMusicPlayer  = myTerminal ++ " " ++ "-t" ++ " " ++ "MusicPlayer"  ++ " " ++ "--class" ++ " " ++ "mpd-client,MPD-CLIENT" ++ " " ++ "-e" ++ " " ++ myMPDClient
    findMusicPlayer   = XMonad.ManageHook.title =? "MusicPlayer"
    manageMusicPlayer = customFloating $ W.RationalRect l t w h

     where

        h = 0.9
        w = 0.9
        t = 0.95 -h
        l = 0.95 -w

-- KEYBINDINGS

myKeys =

                [
                  -- CORE

                  ("M-S-c",   restart "xmonad" True) -- Recompile Xmonad
                , ("M1-<F4>", io exitSuccess)        -- Exit Xmonad

                 -- Actions

                , ("C-S-<Escape>", spawn "$HOME/i3lock/lock.sh") -- Custom Lockscript Using i3lock

                 -- KILL

                , ("M-q",   kill)    -- Kill Focused Window
                , ("M-C-q", killAll) -- Kill All Windows On Workspace

                -- LAYOUTS

                , ("M-<Space>",  sendMessage NextLayout) -- Change Xmonad Layout
                , ("M-n",        refresh)                -- Restore Default Layouts
                , ("M-<Tab> <Tab>",       nextScreen)          -- Cycle To The Next Screen
                , ("M-<Tab> S-<Tab>",     prevScreen)          -- Cycle To The Next Screen
                , ("M-S-<Tab> <Tab>",     shiftNextScreen)     -- Move  To The Next Screen
                , ("M-S-<Tab> S-<Tab>",   shiftPrevScreen)     -- Move  To The Next Screen
                , ("M1-S-<Tab> <Tab>",    swapNextScreen)      -- Swap the windows present on the two screen
                , ("M1-S-<Tab> S-<Tab>",  swapPrevScreen)      -- Swap the windows present on the two screen
                , ("M-f C-f",      withFocused toggleFloat)    -- Toggle Float On Focused Window
                , ("M-.",          warpToWindow (1%10) (1%10)) -- Move Pointer To Focused Window
                , ("M-S-h S-h",    withFocused hideWindow)     -- Hide Focused Window
                , ("M-S-h h",      popOldestHiddenWindow)      -- Pop Oldest Hidden Window
                , ("M-f S-f",      spawn ("polybar-msg cmd toggle")) -- Toggle FULLSCREEN Layout Without Avoiding Struts
                , ("M-f f",        sendMessage ( Toggle FULL ) >> sendMessage ToggleStruts) -- Toggle FULLSCREEN Layout And Avoid Struts

                -- LAYOUT WINDOW FOCUS

                , ("M-k",       windows W.focusUp)   -- k To Change Focus To The "Upper" Window
                , ("M-j",       windows W.focusDown) -- j To Change Focus To The "Down"  Window

                -- LAYOUT WINDOW SWAPS

                , ("M-<Up>",      sendMessage (IncMasterN    1))  -- Arrow Key <M+Up>   To Increase Windows In Master Pane
                , ("M-<Down>",    sendMessage (IncMasterN  (-1))) -- Arrow Key <M+Down> To Decrease Windows In Master Pane
                , ("M-S-k",       windows W.swapUp)   -- Shift + k To Swap To The "Upper" Window
                , ("M-S-j",       windows W.swapDown) -- Shift + j To Swap To The "Down"  Window
                , ("M-m",         dwmpromote)         -- Swap Master Pane With Focused Window And If Focused Window Is Master, Swap With The Next Window In The Stack, Focus Stays On Master Pane

                -- MOVE WINDOWS

                , ("M-M1-<Up>",          sendMessage (MoveUp    10)) -- Arrow Key <M+ALT+Up>    To Move The Focused Window Position By 10 At The "Up"    Side
                , ("M-M1-<Down>",        sendMessage (MoveDown  10)) -- Arrow Key <M+ALT+Down>  To Move The Focused Window Position By 10 At The "Down"  Side
                , ("M-M1-<Left>",        sendMessage (MoveLeft  10)) -- Arrow Key <M+ALT+Left>  To Move The Focused Window Position By 10 At The "Left"  Side
                , ("M-M1-<Right>",       sendMessage (MoveRight 10)) -- Arrow Key <M+ALT+Right> To Move The Focused Window Position By 10 At The "Right" Side

                -- RESIZE WINDOWS

                , ("M-<KP_Subtract>",    sendMessage (DecreaseUp 10) >> sendMessage (DecreaseDown  10) >> sendMessage (DecreaseLeft  10) >> sendMessage (DecreaseRight  10) ) -- Arrow Key <M+C+NumPad+> To Decrease THe Focused Window Size By 10 At      All    Sides
                , ("M-<KP_Add>",         sendMessage (IncreaseUp 10) >> sendMessage (IncreaseDown  10) >> sendMessage (IncreaseLeft  10) >> sendMessage (IncreaseRight  10) ) -- Arrow Key <M+C+NumPad+> To Increase THe Focused Window Size By 10 At      All    Sides

                -- RESIZE GAPS

                , ("M-C-<KP_Add>",       incWindowSpacing 10) -- Decrease Window Spacing
                , ("M-C-<KP_Subtract>",  decWindowSpacing 10) -- Increase Window Spacing
                , ("M-M1-<KP_Add>",      incScreenSpacing 10) -- Decrease Screen Spacing
                , ("M-M1-<KP_Subtract>", decScreenSpacing 10) -- Increase Screen Spacing

                -- SCRATCHPADS

                , ("M-s M-<Return>", namedScratchpadAction myScratchPads "terminal")    -- Spawn A Terminal As A ScratchPad (Alacritty)
                , ("M-s z",          namedScratchpadAction myScratchPads "fileManager") -- Spawn A TUI FileManager As A ScratchPad (ls)
                , ("M-s x",          namedScratchpadAction myScratchPads "musicPlayer") -- Spawn A TUI MusicPlayer As A ScratchPad (NCMPCPP And MPD)

                -- FUNCTION KEYS

                , ("<XF86Explorer>",   spawn myGUIFileExplorer)               -- Use "Fn+F1" To Open File Explorer
                , ("<XF86Search>",     spawn "rofi -show drun -theme $HOME/.config/rofi/launcher/drun/launcher.rasi") -- Use "Fn+F2" To Launch Rofi
                , ("<XF86Calculator>", spawn myCalculator)           -- Use "Fn+F3" To Launch Calculator
                , ("<XF86Tools>",      spawn myGUIMusicApp)          -- Use "Fn+F4" To Launch Spotify
                , ("<XF86AudioPrev>",  spawn "playerctl previous")   -- Use "Fn+F5" With PlayerctlD To Play The Previous Media On The Last Active Player
                , ("<XF86AudioPlay>",  spawn "playerctl play-pause") -- Use "Fn+F6" With PlayerctlD To Play The Next Media On The Last Active Player
                , ("<XF86AudioNext>",  spawn "playerctl next")       -- Use "Fn+F7" With With PlayerctlD To Pause/Play Media On The Last Active Player
                , ("<XF86AudioStop>",  spawn "playerctl stop")       -- Use "Fn+F8" With PlayerctlD To Stop The Last Active Media On The Last Active Player
                , ("<XF86AudioMute>",  spawn "pactl set-sink-mute 0 toggle") -- Use "Fn+F9" With pactl To Mute Volume
                , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%") -- Use "Fn+F9" With pactl To Raise Volume By 5%
                , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%") -- Use "Fn+F9" With pactl To Lower Volume by 5%

                -- ROFI

                , ("M-d d",   spawn "rofi -show drun -theme $HOME/.config/rofi/launcher/drun/launcher.rasi") -- Launcher -Drun
                , ("M-d S-d", spawn "rofi -show run -theme $HOME/.config/rofi/launcher/run/launcher.rasi")   -- Launcher -Run
                , ("M-d g",   spawn "$HOME/.config/rofi/screenshot/screenshot.sh")        -- Screenshot Menu Using Rofi
                , ("M-d S-g", spawn "$HOME/.config/rofi/screenshot/screenshot.sh --stop") -- Stop Recording On The Screenshot Menu Using Rofi
                , ("M-S-q",   spawn "$HOME/.config/rofi/powermenu/powermenu.sh")          -- Powermenu Using Rofi
                , ("M-d b",   spawn "$HOME/.config/rofi/bookmarks/bookmarks.sh")          -- Browser Menu Using Rofi
                , ("M-d x",   spawn "$HOME/.config/rofi/mpd/mpd.sh")                      -- MPD Menu Using Rofi
                , ("M-d s",   spawn "$HOME/.config/rofi/spotify/spotify.sh")              -- Spotify Menu Using Rofi

                -- APPLICATIONS

                , ("M-<Return>", spawn myTerminal)                -- Spawn Terminal (Alacritty)
                , ("M-a b",      spawn myBrowser)                 -- Spawn Browser (Firefox)
                , ("M-a z",      spawn myGUIFileExplorer)         -- Spawn FileManager (Nautilus)
                , ("M-a S-z",    spawn "pcmanfm")                 -- Spawn Backup FileManager (Thunar)
                , ("M-p r",      spawn "polybar-msg cmd restart") -- Restart Polybar

                -- EMACS

                , ("M-e e", spawn myEmacs) -- Launch Emacsclient And If No Server Is Running, Launch Emacs
                , ("M-e z", spawn (myEmacs ++ " --eval '(dired-jump)'"))     -- Launch dired
                , ("M-e M-<Return>", spawn (myEmacs ++ " --eval '(vterm)'")) -- Launch vterm

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
        )

      -- Mod-Button2, Raise The Window To The Top Of The Stack

    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

      -- Mod-Button3, Set The Window To Floating Mode And Resize By Dragging

      , ( (modm .|. shiftMask, button3),
          \w ->
              focus w >> mouseResizeWindow w
                >> windows W.shiftMaster
        )

      -- You May Also Bind Events To The Mouse Scroll Wheel (button4 And button5)

    ]

-- LAYOUTS

-- MYLAYOUT VARIABLES

mySpacing n = spacingRaw False (Border n n n n) True (Border n n n n) True -- Window Spacing Where Integer "n" Is The Gap Between The Windows

-- BUNDLED LAYOUTS

spirals        = renamed [Replace "Spiral"]
                  $ mySpacing 3
                  $ hiddenWindows
                  $ spiral (6/7)

masterAndStack = renamed [Replace "MasterAndStack"]
                  $ mySpacing 3
                  $ hiddenWindows
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

-- No borders on all floating windows
data AllFloats = AllFloats deriving (Read, Show)
instance SetsAmbiguous AllFloats where
    hiddens _ wset _ _ _ = M.keys $ W.floating wset

myLayout = avoidStruts
           $ lessBorders AllFloats
           $ mouseResize
           $ windowArrange
           $ lessBorders Screen
           $ mkToggle (FULL ?? EOT)
           $ spirals
         ||| masterAndStack

-- WINDOW RULES

myManageHook =

  composeAll

    [

      -- FLOATS

       className                    =? "confirm"                             --> doFloat
     , className                    =? "file_progress"                       --> doFloat
     , className                    =? "dialog"                              --> doFloat
     , className                    =? "download"                            --> doFloat
     , className                    =? "error"                               --> doFloat
     , className                    =? "notification"                        --> doFloat
     , className                    =? "pinentry-gtk-2"                      --> doFloat
     , className                    =? "toolbar"                             --> doFloat
     , className                    =? "Lxpolkitr"                           --> doFloat
     , className                    =? "scrcpy"                              --> doFloat
     , className                    =? "Yad"                                 --> doCenterFloat
     , XMonad.ManageHook.title      =? "xeyes"                               --> doCenterFloat
     , XMonad.ManageHook.title      =? "Bulk Rename - Rename Multiple Files" --> doCenterFloat
     , XMonad.ManageHook.title      =? "Unlock Login Keyring"                --> doCenterFloat
     , XMonad.ManageHook.title      =? "File Operation Progress"             --> doCenterFloat
     , isFullscreen                                                          --> doFullFloat
     , (className                   =? "firefox" <&&> resource =? "Dialog")  --> doFloat
     , (className                   =? "code" <&&> resource =? "Dialog")     --> doFloat


     -- BORDERS

     , className                    =? "scratchpadterminal"                  --> hasBorder False
     , className                    =? "spt"                                 --> hasBorder False
     , className                    =? "qalculate-gtk"                       --> hasBorder False
     , className                    =? "lf"                                  --> hasBorder False
     , className                    =? "mpd-client"                          --> hasBorder False
     , className                    =? "Qalculate-gtk"                       --> hasBorder False
     , className                    =? "scrcpy"                              --> hasBorder False
     , className                    =? "Spotify"                             --> hasBorder False
     , XMonad.ManageHook.title      =? "Bulk Rename - Rename Multiple Files" --> hasBorder False
     , (className                   =? "firefox" <&&> resource =? "Dialog")  --> hasBorder False
     , (className                   =? "code" <&&> resource =? "Dialog")     --> hasBorder False

     -- ASSIGN WORKSPACES

     , className                    =? "firefox"             --> doShift ( myWorkspaces !! 0 )
     , className                    =? "Alacritty"           --> doShift ( myWorkspaces !! 1 )
     , className                    =? "Thunar"              --> doShift ( myWorkspaces !! 2 )
     , className                    =? "Pcmanfm"             --> doShift ( myWorkspaces !! 2 )
     , className                    =? "Code"                --> doShift ( myWorkspaces !! 3 )
     , className                    =? "Code - Insiders"     --> doShift ( myWorkspaces !! 3 )
     , className                    =? "Steam"               --> doShift ( myWorkspaces !! 5 )
     , className                    =? "Spotify"             --> doShift ( myWorkspaces !! 8 )

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

myEventHook = fullscreenEventHook <+> docksEventHook <+> serverModeEventHook <+> swallowEventHook (className =? "Alacritty") (return True) <+> swallowEventHookSub (className =? "Alacritty") (return True)

-- LOGHOOK WITH DBUS FOR POLYBAR

myLogHook :: D.Client -> PP
myLogHook dbus =

  filterOutWsPP [scratchpadWorkspaceTag]
  $ def

    -- Polybar Formatting
    -- Check https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Hooks-DynamicLog.html#t:PP

    {

        ppOutput  = dbusOutput dbus
      , ppCurrent = wrap "%{u#5e81ac F#f3f4f5}%{+u}" "%{-u F-}"
      , ppUrgent  = wrap "%{F#db104e}" "%{F-}"
      , ppHidden  = wrap "%{F#abb2bf}" "%{F-}"
      , ppOrder   = \(ws:l:t:_) -> [ws,"  ","%{A1:$HOME/.xmonad/xmonadctl 25:}" ++ l ++ "%{A}","  "]                           -- Xmonad-Log Output With Workspaces And Current Layout
      , ppSep     = ""

    }

-- Xmobar-Log

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

-- STARTUP ACTIONS

myStartupHook =

  do

    -- spawnOnce "sxhkd &" -- SXHKD
    spawnOnce "rclone mount --daemon Drive_arbabashruff: $HOME/Mount/arbabashruff@gmail.com/"                                                                                                                                         -- Mount Drive Account On Local Machine
    spawnOnce "volctl"                                                                                                                                                                                                                -- Pipewire Volume Manager In SysTray
    spawnOnce "lxsession"                                                                                                                                                                                                             -- Polkit
    spawnOnce "xrandr --output DP-1 --off --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-2 --primary --mode 1920x1080 --rate 144.00 --pos 1920x0 --rotate normal --output HDMI-3 --off"                     -- Multi-Screen Xrandr
    spawnOnce "lxsession"                                                                                                                                                                                                             -- Session Utility
    spawnOnce "playerctld daemon"                                                                                                                                                                                                     -- Playerctl Daemon
    spawnOnce "xfce4-power-manager"                                                                                                                                                                                                   -- Xfce Power Manager
    spawnOnce "nm-applet"                                                                                                                                                                                                             -- NetworkManager Systray Utility
    spawnOnce "kdeconnect-indicator"                                                                                                                                                                                                  -- SysTray KDE-Indicator
    spawnOnce "$HOME/.config/dunst/scripts/load.sh"                                                                                                                                                                                   -- Dunst Startup Script
    spawnOnce "picom --experimental-backends"                                                                                                                                                                                         -- Compositor
    spawnOnce "mpd --kill;mpd"                                                                                                                                                                                                        -- MusicPlayerDaemon
    spawnOnce "$HOME/.config/polybar/scripts/launch.sh"                                                                                                                                                                               -- Dock
    spawnOnce "feh --bg-fill $HOME/.xmonad/wallpapers/wallpaper1.png --bg-fill $HOME/.xmonad/wallpapers/wallpaper2.png"                                                                                                               -- Set Background Multi-Screen
    spawnOnce "xsetroot -cursor_name  left_ptr"                                                                                                                                                                                       -- Set Cursor
    spawnOnce "unclutter"                                                                                                                                                                                                             -- Unclutter-xfixes
    -- spawnOnce "emacs --daemon"                                                                                                                                                                                                     -- Start Emacs Daemon
    setWMName "LG3D"

myFilter = filterOutWs [scratchpadWorkspaceTag] -- Scratchpad Filter For EWMH

main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
    xmonad . addEwmhWorkspaceSort (pure myFilter) . ewmh . withUrgencyHook NoUrgencyHook $ docks $ def
                    {
                      -- User Set Variables

                        terminal           = myTerminal              -- Alacritty
                      , focusFollowsMouse  = myFocusFollowsMouse     -- True
                      , clickJustFocuses   = myClickJustFocuses      -- False
                      , borderWidth        = myBorderWidth           -- 2
                      , modMask            = myModMask               -- Super
                      , workspaces         = myWorkspaces            -- 1-9
                      , normalBorderColor  = myUnfocusedBorderColor  -- #3B4252
                      , focusedBorderColor = myFocusedBorderColor    -- #5E81AC

                      -- BINDINGS

                      , mouseBindings = myMouseBindings

                      -- HOOKS AND LAYOUTS

                      , layoutHook      = myLayout
                      , manageHook      = myManageHook
                      , handleEventHook = myEventHook
                      , logHook         = dynamicLogWithPP $ myLogHook dbus
                      , startupHook     = myStartupHook

                    } `additionalKeysP` myKeys
