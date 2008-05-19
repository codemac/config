import XMonad
import XMonad.Operations
import XMonad.Actions.DwmPromote
import XMonad.Hooks.DynamicLog   ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Layouts            
import XMonad.Layout.NoBorders   ( smartBorders )
import XMonad.Prompt             ( XPConfig(..), XPPosition(..) )
import XMonad.Prompt.Shell       ( shellPrompt )
import XMonad.Util.Run
 
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import Data.Ratio
import Graphics.X11
import System.IO
 
statusBarCmd= "dzen2 -bg '#2c2c32' -fg 'grey70' -sa c -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859' -w 1280 -e '' -ta l"
 
main = do din <- spawnPipe statusBarCmd
          xmonad $ defaultConfig
 
                     { borderWidth        = 1
                     , normalBorderColor  = "#111111"
                     , focusedBorderColor = "#aecf96" 
                     , workspaces         = ["1:dev", "2:www", "3:com", "4:doc", "5:ham", "6:tmp", "7", "8", "9:poker"] 
                     , terminal           = "urxvt"
                     , modMask            = mod4Mask
                     , defaultGaps        = [(13,0,0,0)]
					 , manageHook		  = codemacMHooks
                     , logHook            = dynamicLogWithPP $ codemacPP din
					 , layoutHook         = smartBorders $ tiled ||| Mirror tiled ||| Full
                     , keys               = \c -> codemacKeys `M.union` 
                                                  keys defaultConfig c 
                     }
                     where
                       tiled = Tall 1 (3%100) (680%1000)
 
codemacMHooks = composeAll . concat $
				[[ className =? c --> doFloat | c <- floats ]
				,[ resource =? r --> doIgnore | r <- ignore ]
				,[ resource =? "gecko" --> doF (W.shift "2:www") ]
				,[ resource =? "emacs" --> doF (W.shift "3:com")]]
	where floats = ["MPlayer", "Gimp"] ; ignore = ["trayer", "panel"]
 
-- redifine some keys
--
codemacKeys = M.fromList $
                   [ ((mod4Mask     , xK_p     ), shellPrompt codemacSPConfig)
				   , ((mod4Mask     , xK_Return), dwmpromote)
				   , ((mod4Mask		, xK_Up	   ), spawn "aumix -v+3")
				   , ((mod4Mask		, xK_Down   ), spawn "aumix -v-3")
				   , ((mod4Mask		, xK_Left	   ), spawn "mocp -r")
				   , ((mod4Mask		, xK_Right	   ), spawn "mocp -f")
				   , ((mod4Mask		, xK_z	   ), spawn "mocp -G")
                   ]
 
-- dynamiclog pretty printer for dzen
--
codemacPP h = defaultPP 
                 { ppCurrent = wrap "^fg(#000000)^bg(#a6c292)^p(2)^i(/home/codemac/.bitmaps/has_win.xbm)" "^p(2)^fg()^bg()"
                  , ppVisible = wrap "^bg(grey30)^fg(grey75)^p(2)" "^p(2)^fg()^bg()"
                  , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
                  , ppLayout  = dzenColor "#80AA83" "" .
                                (\x -> case x of
                                         "Tall" -> "^i(/home/codemac/.bitmaps/tall.xbm)"
                                         "Mirror Tall" -> "^i(/home/codemac/.bitmaps/mtall.xbm)"
                                         "Full" -> "^i(/home/codemac/.bitmaps/full.xbm)"
                                )
                  , ppTitle   = dzenColor "white" "" . wrap "< " " >" 
                  , ppOutput   = hPutStrLn h
                  }
 
-- shellprompt config
--
codemacSPConfig = XPC { 
                           font              = "-*-profont-*-*-*-*-11-*-*-*-*-*-iso8859"
                         , bgColor           = "#111111"
		         , fgColor           = "#d5d3a7"
		         , bgHLight          = "#aecf96"
		         , fgHLight          = "black"
		         , borderColor       = "black"
                         , promptBorderWidth = 0
		         , position          = Bottom
                         , height            = 15
                         , historySize       = 256
                   }
