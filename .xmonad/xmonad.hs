import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.AppLauncher(launchApp)
import XMonad.Prompt.Shell(prompt)
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.StackSet(sink)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import System.IO

promptStyle = def
  { font = "xft:Droid Sans Mono Slashed:style=Regular:size=10"
  , bgColor = "#373737"
  , fgColor = "#c0c0c0"
  , borderColor = "#373737"
  }

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/jason/.xmobarrc"
  xmonad $ def
    { workspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι" ]
    , terminal = "urxvtc -e /usr/bin/tmux"
    , normalBorderColor = "#373737"
    , focusedBorderColor = "#373737"
    , manageHook = manageDocks <+> composeAll [ className =? "rdesktop" --> doSink ]
    , layoutHook = avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor "#b71fb5" "" . wrap "[" "]"
      , ppTitle = xmobarColor "#c0c0c0" "" . shorten 50
      , ppUrgent= xmobarColor "#dc322f" ""
      , ppVisible = wrap "(" ")"
      }
    } `additionalKeys`
    [    ((mod1Mask .|. shiftMask, xK_BackSpace), spawn "urxvtc")
       , ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
       , ((mod1Mask .|. shiftMask, xK_p), shellPrompt promptStyle)
       , ((mod1Mask .|. shiftMask, xK_m), prompt "urxvtc -e mux" promptStyle)
       , ((mod1Mask .|. shiftMask, xK_r), prompt "/bin/sh rdesktop" promptStyle)
    ] 
     `additionalKeysP`
    [    ("<XF86TouchpadToggle>", spawn "~/bin/syntoggle")
       , ("<XF86AudioMute>", spawn "/usr/bin/pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
       , ("<XF86AudioRaiseVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1%")
       , ("<XF86AudioLowerVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -1%")
    ]

doSink :: ManageHook
doSink = ask >>= doF . sink
