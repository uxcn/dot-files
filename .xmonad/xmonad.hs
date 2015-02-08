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
  xmonad $ defaultConfig
    { normalBorderColor = "#222222"
    , focusedBorderColor = "#657b83"
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
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
       , ((mod1Mask .|. shiftMask, xK_p), shellPrompt defaultXPConfig
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#222222"
           }
         )
       , ((mod1Mask .|. shiftMask, xK_m), prompt "/usr/bin/urxvtc -e mux" defaultXPConfig
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#222222"
           })
       , ((mod1Mask .|. shiftMask, xK_r), prompt "/bin/sh rdesktop" defaultXPConfig
           { bgColor = "#373737"
           , fgColor = "#657b83"
           , borderColor = "#222222"
           })
    ] 
     `additionalKeysP`
    [    ("<XF86TouchpadToggle>", spawn "~/bin/synaptics/syntoggle")
       , ("<XF86AudioMute>", spawn "/usr/bin/pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle")
       , ("<XF86AudioRaiseVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +1%")
       , ("<XF86AudioLowerVolume>", spawn "/usr/bin/pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -1%")
    ]

