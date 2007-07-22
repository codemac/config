--[[
	min_tabs.lua

	lua code to auto show/hide tabs as the number of windows in an ion frame
	changes from/to 1
	this affects windows on WIonWS and WPaneWS workspaces but leave
	windows on WFloatWS's alone

	Originally created by David Tweed

	11 Feb 2005 David Tweed: modified to work with ion3 svn versions
	15 Feb 2005 Cas Cremers: only frames with one window hide the tabbar
	(as opposed to <= 2)
	01 Sep 2005 Cas Cremers: single client windows that are tagged still
	show the tabbar.

	- Easier to work with using a style with slightly wider frame
	  borders and more vivid "active frame" colours.
	- The keybinding MOD1..T is defined at the end of the file: this
	  should override the normal binging, thus min_tabs should be
	  loaded *after* the normal definition of MOD1..T. It is not
	  appropriate to have key bindings hard-coded in extensions, but I
	  don't currently know of a better way.
	- Currently supports ion3 with old toggling as well as new
	  toggling function conventions; this can be removed later on.

	one way to enable this is by adding
	dopath("min_tabs")
	to cfg_ion.lua

]]

function show_only_necessary_tabs_in_frame(fp)
    if obj_typename(fp) ~= "WFrame" then
	-- Escape: floatws thing should not be handled
        return
    end

    -- First the logic, then the propagation back to ion

    -- It should *not* be shown if there is only one app
    -- However, this (single) app should not be tagged,
    -- because then we would want to show it.

    -- Assume the tabbar should be shown.
    local show_bar = true
    if fp:lcount(1) == 1 then
	local rg = fp:llist(1)[1]
	if not rg:is_tagged() then
	    show_bar = false
	end
    end

    -- Propagate choice

    -- Note the ugly code: this actually caters for two versions of ion3
    -- I am only including this because I like Ubuntu [CC]
    local oldversion = (fp["toggle_tabbar"] ~= nil)
    if oldversion then
        -- old version (for Ubuntu, can be removed later on)
        local t_hid=fp:is_tabbar_hidden()
        -- Unfortunately we don't have an primitive xor in lua
        if (t_hid and show_bar) or (not(t_hid or show_bar)) then
    	    fp:toggle_tabbar()
        end
    else
        -- new version
        ioncore.defer(function()
            if show_bar then
                fp:set_tabbar("set")
            else
                fp:set_tabbar("unset")
            end
        end)
    end
end



function show_only_necessary_tabs_in_frame_wrapper(ftable)
    show_only_necessary_tabs_in_frame(ftable.reg)
end



function min_tabs_setup_hook()
    local hk=ioncore.get_hook("frame_managed_changed_hook")
    hk:add(show_only_necessary_tabs_in_frame_wrapper)
end



function min_tabs_tag_wrapper(fr,reg)

    -- Note the ugly code: this actually caters for two versions of ion3
    -- I am only including this because I like Ubuntu [CC]

    local oldversion = (reg["toggle_tag"] ~= nil)
    if oldversion then

        -- old version (for Ubuntu, can be removed later on)
        reg:toggle_tag()

    else

        -- new version
        reg:set_tagged("toggle")

    end

    -- recompute tabbar state
    show_only_necessary_tabs_in_frame(fr)
end


--[[
	Special keybinding override for this extension
]]

defbindings("WFrame", {
    bdoc("Tag current object within the frame."),
    kpress(META.."T", "min_tabs_tag_wrapper(_,_sub)", "_sub:non-nil"),
})



min_tabs_setup_hook()
