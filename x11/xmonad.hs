import XMonad

import XMonad.Actions.Warp
import XMonad.Actions.CycleWS
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.WindowBringer
import qualified XMonad.Actions.Search as S

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ThreeColumns
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation

import Data.Ratio ((%))

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL

import qualified XMonad.StackSet as W

import System.IO
import Control.Monad
import Text.Printf

import XMonad.Config.Desktop


-- Define useful aliases for the piping operators
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x


infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x


-- Set up the basic bits: modifier key, terminal
myModMask = mod4Mask
myTerminal = "sakura"
myBrowser = "firefox"


-- shortcut for constructing the command for opening a program in the terminal
-- (vanilla term)
myTerminalEx cmd = myTerminal ++ " -e " ++ cmd 
inTerm cmd = spawn <| myTerminalEx cmd


oneToZero = map show <| [1 .. 9] ++ [0]
oneToNine = map show <| [1 .. 9]
oneToTwelve = map show <| [1 .. 12]


-- Layouts
myLayout =
    let
        myTabbed = named "T" (simpleTabbedAlways)
        myFull = named "F" (Full)
        myTiled = named "!" (Tall 1 (3/100) (1/2))
    in
        avoidStruts
        <| smartBorders
        <| mkToggle (single MIRROR)
        <| mkToggle (single REFLECTY)
        <| mkToggle (single REFLECTX)
        <| windowNavigation
        <| myTiled ||| myFull ||| myTabbed


-- Workspaces
myWorkspaces = oneToZero

myAddViewWorkspace :: String -> X ()
myAddViewWorkspace newtag = addHiddenWorkspace newtag >> windows (W.view newtag)

myAddShiftWorkspace :: String -> X ()
myAddShiftWorkspace newtag = addHiddenWorkspace newtag >> windows (W.shift newtag)

myRegularKeys = oneToZero


myManageHook =
    composeAll
        [ className =? "float" --> doFloat
        , className =? "rdesktop" --> doFloat
        , className =? "feh" --> doFloat
        , className =? "ROX-Filer" --> doFloat
        , appName =? "Godot_Engine" --> doFloat
        , (stringProperty "WM_NAME" =? "Volume Control") --> doFloat
        , (stringProperty "WM_NAME" =? "Slack") --> (doShift "0")
        , (stringProperty "WM_NAME" =? "Zoom Cloud Meetings") --> (doShift "0")
        , className =? "zoom" --> (doShift "zoom")
        , manageDocks
        ]
        <+> manageHook def


-- XPConfig styles the prompts in XMonad.Prompt
myXPConfig =
    def { position = Top }


-- Configure the dzen statusbar:
myPP =
    def { ppSep = " | ", ppLayout = (\a -> a), ppTitle = (\_ -> "") }


-- Key bindings, readably formatted for additionalKeysP in EZConfig
myKeyMapping =
    [ 
    -- Terminal: Either scratchpad, persistent terminal, conf, wifi
    ("M-d", spawn myTerminal)
    , ("M-u", spawn "xterm -class float -geometry 200x10")
    , ("M-S-u", inTerm "vim ~/.xmonad/xmonad.hs") 

    -- Some random launchers
    , ("M-S-f f", spawn <| myBrowser ++ " --incognito")
    , ("M-S-f p", spawn <| myBrowser ++ " --app=$(xclip -o)")
    , ("M-p r", spawn "~/bin/remind_prompt")
    , ("M-p t", spawn "~/bin/toggle-scheme")
    -- force-refresh the statusbar
    , ("M-r", spawn "~/bin/stop_statusbar.sh")

    -- Edit a template, the clipboard, a temp file, ...
    , ("M-e t", inTerm "vim $(mktemp -t 'ff_XXX.txt')")
    , ("M-e S-t", inTerm "~/bin/check-todo")
    , ("M-e C-t", inTerm "~/bin/add-todo")
    , ("M-e p", inTerm "/bin/bash /home/med/bin/vimclip")
    , ("M-e S-p", inTerm "curlvimclip")
    , ("M-e S-e", inTerm "vim $(xclip -o)")
    , ("M-e m", inTerm "~/bin/viewmsgs")

    -- Screenshot stuff
    , ("M-e s", spawn "~/bin/hcsnap")
    , ("M-e S-s", spawn "feh -. /tmp/hc.png")
    , ("M-e C-s", spawn "~/bin/drop-latest-img")
    , ("M-e .", spawn "~/bin/latest-img | xargs pinta")
    ] ++

    [ ("M-e r r", spawn "~/scripts/do_trigger") ] ++
    [ ("M-e r " ++ num, spawn <| "~/scripts/swaptrigger " ++ num) | num <- oneToZero ] ++
    [ ("M-e S-r " ++ num, inTerm <| "~/scripts/edit_trigger " ++ num) | num <- oneToZero ] ++

    -- Save a macro into /tmp/macro1 .. /tmp/macro0
    [ ("M-s M-S-d " ++ num, spawn <| "mkmacro macro" ++ num) | num <- oneToZero ] ++
    -- Run a macro stored from /tmp/macro1 .. /tmp/macro0
    [ ("M-s d " ++ num, spawn <| "/bin/bash /tmp/macro" ++ num) | num <- oneToZero ] ++

    -- Convenient clipboard-related stuff
    [
      ("M-s s", spawn "xclip -o | xclip -fi -selection CLIPBOARD")
    ] ++

    -- Process the clipboard through /tmp/c1 .. /tmp/c0
    [ ("M-s " ++ num, spawn <| "~/scripts/clip-from " ++ num) | num <- oneToZero ] ++
    [ ("M-s C-" ++ num, spawn <| "~/scripts/clip-to " ++ num) | num <- oneToZero ] ++
    [
    -- Prompts: Search, web browse, or edit 
    ("M-f", spawn myBrowser)
    , ("M-e e", AL.launchApp myXPConfig <| myTerminalEx "vim ")

    -- Window management
    , ("M-t", selectWorkspace def)
    , ("M-S-t c", addWorkspacePrompt def)
    , ("M-S-t d", removeEmptyWorkspace)
    , ("M-S-t x", removeEmptyWorkspace)
    , ("M-S-t n", withWorkspace def (windows . W.shift))
    , ("M-S-t m", withWorkspace def (windows . copy))
    , ("M-S-t r", renameWorkspace def)
    , ("M-o", windows W.focusMaster) 
    , ("M-S-o", windows W.swapMaster) 
    , ("M-i", gotoMenu)
    , ("M-S-i /", sendMessage <| Toggle MIRROR)
    , ("M-S-i x", sendMessage <| Toggle REFLECTX)
    , ("M-S-i y", sendMessage <| Toggle REFLECTY)
    , ("M-w", return ())

    , ("M-p S-s", withFocused $ windows . W.sink) 
    , ("M-c", kill) 
    , ("M-S-c", spawn "xkill") 
    , ("M-n", nextScreen)
    , ("M-m", toggleWS)
    , ("M-S-n", shiftNextScreen)
    , ("M-C-n", swapNextScreen)
    , ("M-b", prevScreen)
    , ("M-S-b", shiftPrevScreen)
    , ("M-C-b", swapPrevScreen)
    , ("M-C-l", sendMessage <| Move R)
    , ("M-C-h", sendMessage <| Move L)
    , ("M-C-k", sendMessage <| Move U)
    , ("M-C-j", sendMessage <| Move D)
    , ("M-y l", sendMessage <| Go R)
    , ("M-y h", sendMessage <| Go L)
    , ("M-y k", sendMessage <| Go U)
    , ("M-y j", sendMessage <| Go D)
    , ("M-p s", sendMessage ToggleStruts)
    , ("M-;", banish LowerRight)
    ] ++

    [ ("M-" ++ key, myAddViewWorkspace key) | key <- myRegularKeys ] ++ 
    [ ("S-M-" ++ key, myAddShiftWorkspace key) | key <- myRegularKeys ]


-- Fix the settings
myXMonadConfig = desktopConfig
    { modMask = myModMask
    , terminal = myTerminal
    , layoutHook = myLayout
    , workspaces = myWorkspaces
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#FFFF00"
    , borderWidth = 3
    , manageHook = myManageHook
    , logHook = dynamicLogWithPP myPP
    } `additionalKeysP` myKeyMapping


-- Spawn the pipe that reads in to the dzen statusbar, then apply these settings
main = do
    putStrLn "XMonad starting..."
    xmonad myXMonadConfig

