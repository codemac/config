--
-- Ion main configuration file
--
-- This file only includes some settings that are rather frequently altered.
-- The rest of the settings are in cfg_ioncore.lua and individual modules'
-- configuration files (cfg_modulename.lua).
--

-- Set default modifiers. Alt should usually be mapped to Mod1 on
-- XFree86-based systems. The flying window keys are probably Mod3
-- or Mod4; see the output of 'xmodmap'.
META="Mod4+"
--ALTMETA=""

-- Terminal emulator
XTERM="urxvt"

-- Some basic settings
ioncore.set{
    -- Maximum delay between clicks in milliseconds to be considered a
    -- double click.
    --dblclick_delay=250,

    -- For keyboard resize, time (in milliseconds) to wait after latest
    -- key press before automatically leaving resize mode (and doing
    -- the resize in case of non-opaque move).
    --kbresize_delay=1500,

    -- Opaque resize?
    --opaque_resize=false,

    -- Movement commands warp the pointer to frames instead of just
    -- changing focus. Enabled by default.
    --warp=true,
}

-- cfg_ioncore contains configuration of the Ion 'core'
dopath("cfg_ioncore")

-- Load some kludges to make apps behave better.
dopath("cfg_kludges")

-- Load defaults for new workspaces, etc.
dopath("cfg_layouts")

-- Load some modules. Disable the loading of cfg_modules by commenting out 
-- the corresponding line with -- if you don't want the whole default set 
-- (everything except mod_dock). Then uncomment the lines for the modules
-- you want.
-- dopath("cfg_modules")
dopath("mod_query")
dopath("mod_menu")
dopath("mod_tiling")
dopath("mod_statusbar")
--dopath("mod_dock")
dopath("mod_sp")

-- Deprecated.
dopath("cfg_user", true)

-- Do my key configs!


defbindings("WClientWin", {
    bdoc("Kill client owning the client window."),
    kpress_wait(META.."C", "WClientWin.rqclose_propagate(_,_sub)"),
})


defbindings("WMPlex", {
    bdoc("Close current object."),
    kpress_wait(META.."C", "WRegion.rqclose_propagate(_, _sub)"),
})

defbindings("WMPlex.toplevel", {
	bdoc("Toggle mpd"),
	kpress(META.."Z", "ioncore.exec_on(_, 'mpc toggle')"),

	bdoc("Skip to next song in mpd."),
	kpress(META.."Right", "ioncore.exec_on(_, 'mpc next')"),

	bdoc("Skip to previous song in mpd."),
	kpress(META.."Left", "ioncore.exec_on(_, 'mpc prev')"),

	bdoc("Volume up"),
	kpress(META.."Up", "ioncore.exec_on(_, 'aumix -v+1')"),

	bdoc("Volume down."),
	kpress(META.."Down", "ioncore.exec_on(_, 'aumix -v-1')"),

})

defbindings("WFrame.toplevel", {
    bdoc("Switch to next/previous object within the frame."),
    kpress(META.."J", "WFrame.switch_next(_)"),
    kpress(META.."Q", "WFrame.switch_prev(_)"),
	bdoc("Remove toggle bar."),
	kpress(META.."B", "WFrame.mode(_) == 'tiled' and WFrame.set_mode(_,'tiled-alt') or WFrame.set_mode(_, 'tiled')"),
    
})
