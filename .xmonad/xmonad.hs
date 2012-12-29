import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Prompt
import XMonad.Prompt.Shell(shellPrompt)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
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
      , ppCurrent = xmobarColor "#d33682" "" . wrap "[" "]"
      , ppTitle = xmobarColor "#c0c0c0" "" . shorten 50
      , ppUrgent= xmobarColor "#dc322f" ""
      , ppVisible = wrap "(" ")"
      }
    , terminal = "/usr/bin/uxterm -tn xterm-256color"
    } `additionalKeys`
    [    ((mod1Mask .|. shiftMask, xK_z), spawn "/usr/bin/xscreensaver-command -lock")
       , ((mod1Mask .|. shiftMask, xK_p), shellPrompt defaultXPConfig
           { bgColor = "#1b1b1b"
           , fgColor = "#585858"
           , borderColor = "#222222"
           }
         )
    ] 

