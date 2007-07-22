--[[

 Grabbed menu for activation history based cycling of windows.

 2005-09-16 The list now skips anything whose parent is a WDock.
   
 
 To use it, bind it e.g. as follows:
--]]
 
 ioncore.defbindings("WScreen", {
     kpress(MOD1.."Q", 'mod_menu.grabmenu(_, _sub, "wcirculate", "Q")'),
 })



local last_activated={}

local sup_act_id=1000*1000
local act_id_counter=0

local function get_act_id()
    id={tonumber(os.date('%Y%m%d%H%M%S')), act_id_counter}
    act_id_counter=math.mod(act_id_counter+1, sup_act_id)
    return id
end

local function compare_act_id(a, b)
    return (a[1]<b[1] or (a[1]==b[1] and a[2]<b[2]))
end

local function compare_cwin(w1, w2)
    local id1, id2=last_activated[w1], last_activated[w2]
    if not id2 then
        return (not id1)
    elseif not id1 then
        return true
    else
        return compare_act_id(id1, id2)
    end
end

local function activated_handler(reg)
    if obj_is(reg, "WClientWin") then
        last_activated[reg]=get_act_id()
    end
end

local function unmapped_handler(xid)
    for cwin, _ in pairs(last_activated) do
        if not obj_exists(cwin) then
            last_activated[cwin]=nil
        end
    end
end

local function mkhistmenu()
    local function tweaked_compare(w1, w2)
        if w1:is_active() then
            return false
        elseif w2:is_active() then
            return true
        else
            return not compare_cwin(w1, w2)
        end
    end    
    local function mkentry(cwin)
        return menuentry(cwin:name(), function() cwin:goto() end)
    end

    local function filterdockapps(cwinlist)
        local cwinlist2 = {}
        for cw in pairs(cwinlist) do
	    if obj_typename(cwinlist[cw]:parent()) ~= "WDock" then
		table.insert(cwinlist2,cwinlist[cw])
            end
        end
        return cwinlist2
    end

    local function filterByWinprop( cwinlist )
        local resultList = {}

        for cw in pairs(cwinlist) do
            local wp = ioncore.getwinprop( cwinlist[cw] )
            if ((not wp) or (not wp.winlist_ignore)) then
                table.insert( resultList, cwinlist[cw] )
            end
        end

        return resultList
    end

    local l=ioncore.clientwin_list()
    l=filterdockapps(l)
    l = filterByWinprop( l )
    table.sort(l, tweaked_compare)
    return table.map(mkentry, l)
end

local function init()
    ioncore.defmenu("wcirculate", mkhistmenu)
    
    ioncore.get_hook("clientwin_unmapped_hook"):add(unmapped_handler)
    ioncore.get_hook("region_activated_hook"):add(activated_handler)
end

init()


