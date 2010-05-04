import XMonad
import XMonad.Config.Gnome

startup :: X ()
startup = do
  spawn "xmonad-startup-script.sh"

main = xmonad $ gnomeConfig
       { terminal = "gnome-terminal"
       , modMask = mod4Mask
       , startupHook = startup
       }