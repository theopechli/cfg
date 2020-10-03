import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks   (ToggleStruts(..),avoidStruts,docks,manageDocks)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.Gaps

main = do
    xmproc <- spawnPipe "xmobar"
    cproc <- spawnPipe "xsetroot -cursor_name left_ptr"
    rsproc <- spawnPipe "redshift"
    fproc <- spawnPipe "feh --randomize --bg-fill ~/Pictures/*"
    xmonad $ docks def
        { modMask = mod4Mask
        , terminal = "urxvt"
        , layoutHook = avoidStruts $ layoutHook def
        , logHook = dynamicLogWithPP $ def
        { ppOutput = hPutStrLn xmproc
        , ppCurrent = xmobarColor "yellow" ""
        }
        , manageHook = manageDocks <+> manageHook def
        } `additionalKeys`
        [ ((mod4Mask, xK_r), spawn "dmenu_run")
        , ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
        , ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
--        , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
--        , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 1%-")
--        , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 1%+")
        , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
        , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -1%")
        , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1%")
        , ((mod4Mask, xK_F9), spawn "light -A 10")
        , ((mod4Mask, xK_F8), spawn "light -U 10")
--        , ((mod4Mask, xK_F5), spawn "amixer -q set Master toggle")
--        , ((mod4Mask, xK_F6), spawn "amixer -q set Master 1%-")
--        , ((mod4Mask, xK_F7), spawn "amixer -q set Master 1%+")
        , ((mod4Mask, xK_F5), spawn "pactl set-sink-mute 0 toggle")
        , ((mod4Mask, xK_F6), spawn "pactl set-sink-volume 0 -1%")
        , ((mod4Mask, xK_F7), spawn "pactl set-sink-volume 0 +1%")
        , ((mod4Mask, xK_p), spawn "mpc toggle")
        , ((mod4Mask, xK_i), spawn "mpc volume -5")
        , ((mod4Mask, xK_o), spawn "mpc volume +5")
        , ((mod4Mask, xK_y), spawn "mpc prev")
        , ((mod4Mask, xK_u), spawn "mpc next")
        , ((mod4Mask, xK_F2), spawn "systemctl hibernate")
        , ((mod4Mask, xK_F3), spawn "systemctl reboot")
        , ((mod4Mask, xK_F4), spawn "systemctl poweroff")
        , ((mod4Mask, xK_f), spawn "firefox")
        , ((mod4Mask .|. shiftMask, xK_f), spawn "prime-run firefox")
        , ((mod4Mask, xK_v), spawn "virt-manager")
        , ((mod4Mask, xK_n), spawn "redshift")
        , ((mod4Mask .|. shiftMask, xK_n), spawn "killall redshift")
        , ((mod4Mask, xK_b), sendMessage ToggleStruts)
--        , ((mod4Mask .|. shiftMask, xK_m), spawn "xrandr --output VGA1 --mode 1920x1080 --pos 0x0 --rotate normal --output eDP1 --mode 1366x768 --pos 1920x580 --rotate normal")
        , ((mod4Mask .|. shiftMask, xK_m), spawn "xrandr --output VGA1 --mode 1360x768 --pos 0x0 --rotate normal --right-of eDP1 --output eDP1 --mode 1366x768 --pos 1366x420 --rotate normal")
        ]
