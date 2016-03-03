import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.AppLauncher(launchApp)
import XMonad.Prompt.Shell(prompt)
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys, additionalKeysP)
import System.IO

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/jason/.xmobarrc"
  xmonad $ def
    { normalBorderColor = "#373737"
    , focusedBorderColor = "#373737"
    , manageHook = manageDocks <+> manageHook def
    , layoutHook = avoidStruts  $  layoutHook def
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn xmproc
      , ppCurrent = xmobarColor "#b71fb5" "" . wrap "[" "]"
      , ppTitle = xmobarColor "#c0c0c0" "" . shorten 50
      , ppUrgent= xmobarColor "#dc322f" ""
      , ppVisible = wrap "(" ")"
      }
    , terminal = "/usr/bin/urxvtc -e /usr/bin/tmux"
    } `additionalKeys`
    [    ((mod1Mask .|. shiftMask, xK_BackSpace), spawn "/usr/bin/urxvtc")
       , ((mod1Mask .|. shiftMask, xK_z), spawn "/usr/bin/xscreensaver-command -lock")
       , ((mod1Mask .|. shiftMask, xK_p), shellPrompt def
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#373737"
           }
         )
       , ((mod1Mask .|. shiftMask, xK_m), prompt "/usr/bin/urxvtc -e mux" def
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#373737"
           })
       , ((mod1Mask .|. shiftMask, xK_r), prompt "/bin/sh rdesktop" def
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#373737"
           })
    ] 
     `additionalKeysP`
    [    ("<XF86TouchpadToggle>", spawn "~/bin/synaptics/syntoggle")
       , ("<XF86AudioMute>", spawn "/usr/bin/pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
       , ("<XF86AudioRaiseVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1%")
       , ("<XF86AudioLowerVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -1%")
    ]

