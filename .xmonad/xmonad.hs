import XMonad
import XMonad.Actions.CopyWindow(copyToAll, killAllOtherCopies)
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
  { font = "xft:Droid Sans Mono Slashed:style=Regular:size=8:anitalias=true:autohint=true"
  , bgColor = "#262626"
  , fgColor = "#f9f9f9"
  , borderColor = "#262626"
  }

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/jason/.xmobarrc"
  xmonad $ docks def
    { workspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]
    , terminal = "st -e /usr/bin/tmux"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#f9f9f9"
    , manageHook = manageDocks <+> composeAll [ className =? "rdesktop" --> doSink ]
    , layoutHook = avoidStruts $ layoutHook def
    , startupHook = sendMessage $ SetStruts [minBound..maxBound] [] -- FIXME
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor "#f9f9f9" "" . wrap "[" "]"
      , ppTitle = xmobarColor "#f9f9f9" "" . shorten 50
      , ppUrgent= xmobarColor "#9c9c05" ""
      , ppVisible = wrap "(" ")"
      }
    } `additionalKeys`
    [    ((mod1Mask .|. shiftMask, xK_a), windows copyToAll)
       , ((mod1Mask .|. shiftMask, xK_o), killAllOtherCopies)
       , ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
       , ((mod1Mask .|. shiftMask, xK_p), shellPrompt promptStyle)
       , ((mod1Mask .|. shiftMask, xK_m), prompt "st -e mux" promptStyle)
       , ((mod1Mask .|. shiftMask, xK_r), prompt "/bin/sh rdesktop" promptStyle)
       , ((mod1Mask .|. shiftMask, xK_BackSpace), spawn "st")
    ]
     `additionalKeysP`
    [    ("<XF86TouchpadToggle>", spawn "~/bin/syntoggle")
       , ("<XF86AudioMute>", spawn "/usr/bin/pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
       , ("<XF86AudioRaiseVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1%")
       , ("<XF86AudioLowerVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -1%")
    ]

doSink :: ManageHook
doSink = ask >>= doF . sink
