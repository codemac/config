-- look at /etc/notion for descriptions
META="Mod4+"
ALTMETA="Mod4+"

-- xterm set up to be by default .. utf8-y
XTERM="term"

ioncore.set{
    dblclick_delay=250,
    kbresize_delay=1500,
    opaque_resize=false,
    warp=true,
    switchto=true,
    frame_default_index='next',
    unsqueeze=true,
    screen_notify=true,
    autosave_layout=true,
    mousefocus="sloppy",
    window_stacking_request="ignore",
    focuslist_insert_delay=1000,
    activity_notification_on_all_screens=false,
    workspace_indicator_timeout=0,
}

dopath("cfg_defaults")
dopath("cfg_look")
dopath("net_client_list.lua")
