-- send_to_ws.lua
--
-- Send a clientwin to the next or previous workspace.
-- Focus follows if jumpto==true.
-- On a WIonWS, the frame sent to will be the most recently active.
-- A new WIonWS is created if no workspace is found.
-- Maximized windows are skipped over in search of an actual workspace.
--

-- These are the bindings I use, as an example.
defbindings("WMPlex", {
    submap(MOD1.."K", {
        submap("bracketleft", {
	    kpress("H", "send_to_ws_byoffset(_sub, -1, true)", "_sub:WClientWin"),
	    kpress("L", "send_to_ws_byoffset(_sub, 1, true)", "_sub:WClientWin"),
	    kpress("1", "send_to_ws(_sub, 0, true)", "_sub:WClientWin"),
	    kpress("2", "send_to_ws(_sub, 1, true)", "_sub:WClientWin"),
	    kpress("3", "send_to_ws(_sub, 2, true)", "_sub:WClientWin"),
	    kpress("4", "send_to_ws(_sub, 3, true)", "_sub:WClientWin"),
	    kpress("5", "send_to_ws(_sub, 4, true)", "_sub:WClientWin"),
	    kpress("6", "send_to_ws(_sub, 5, true)", "_sub:WClientWin"),
	    kpress("7", "send_to_ws(_sub, 6, true)", "_sub:WClientWin"),
	    kpress("8", "send_to_ws(_sub, 7, true)", "_sub:WClientWin"),
	    kpress("9", "send_to_ws(_sub, 8, true)", "_sub:WClientWin"),
	    kpress("F", "send_to_new_floatws(_sub)", "_sub:WClientWin"),
        }),
    }),
    kpress(MOD1.."backslash", "send_to_ws_byoffset(_sub, 0, false)", "_sub:WClientWin"),
})

local function destroy_empty(scr,curws)
    -- If there's nothing left, destroy the starting ws.
    local curwstype=obj_typename(curws)
    if curwstype=="WFloatWS" then
	local curws_cwins=table.getn(curws:managed_list())
	if curws_cwins<=1 then
	    curws:rqclose(true)
	elseif curws_cwins==2 then
	    --nothing left except a status display.
	    if scr:get_stdisp() then
		curws:rqclose(true)
	    end
	end
    elseif curwstype=="WIonWS" then
	if table.getn(curws:managed_list())<=1 and 
	  curws:current():current()==nil then
	    curws:rqclose(true)
	end
    end
end

-- offset can be positive or negative.
-- offset=0 has a special meaning of "next or previous, whichever exists"
--
function send_to_ws_byoffset(cwin, offset, jumpto)
    local wsindex
    local curws, nextws, nextwstype
    local scr=cwin:screen_of()

    curws=cwin:manager()
    while curws:manager()~=scr do
	curws=curws:manager()
    end

    wsindex=WMPlex.get_index(scr, curws)
    nextws=WMPlex.lnth(scr, 1, wsindex+offset)
    nextwstype=obj_typename(nextws)
    -- begin dbw added.  Sends in the direction of available workspace.
    if offset==0 then
	if not(WMPlex.lnth(scr, 1, wsindex+1)) then
	    send_to_ws_byoffset(cwin, -1, jumpto)
	else
	    send_to_ws_byoffset(cwin, 1, jumpto)
	end
    -- end dbw added
    elseif nextwstype=="WFloatWS" then
	nextws:attach(cwin)
    elseif nextwstype=="WIonWS" or nextwstype=="WPaneWS" then
	nextws:current():attach(cwin, {switchto=true})
    elseif nextwstype=="WClientWin" or nextwstype=="WFrame" then
	if offset>0 then offset=offset+1 else offset=offset-1 end
	send_to_ws_byoffset(cwin, offset)
    else
	local newws=scr:attach_new{type="WIonWS"}
	newws:current():attach(cwin)
    end

    if jumpto then
	cwin:goto()
	destroy_empty(scr,curws)
    end
end

-- To a specific workspace.  
-- send_to_ws starts at 0 because WScreen.switch_nth starts at 0.
-- (In both cases the bindings make it look like they start at 1)
--
function send_to_ws(cwin, wsindex, jumpto)
    local scr=cwin:screen_of()

    local destws=WMPlex.lnth(scr, 1, wsindex)
    local destwstype=obj_typename(destws)

    --local curws=scr:current()
    local curws=cwin:manager()
    while curws:manager()~=scr do
	curws=curws:manager()
    end
    if curws==destws then return false end

    if destwstype=="WFloatWS" then
	destws:attach(cwin)
    elseif destwstype=="WIonWS" then
	destws:current():attach(cwin, {switchto=true})
    end

    if jumpto then
	cwin:goto()
	destroy_empty(scr,curws)
    end
end

-- To its own FloatWS
--
function send_to_new_floatws(cwin)
    local n
    n=cwin:screen_of():lcount(1)
    ioncore.create_ws(cwin:screen_of(), "WFloatWS")
    cwin:screen_of():lnth(1, n):attach(cwin)
    cwin:goto()
end
