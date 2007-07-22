--
-- statusbar_act.lua
-- 
-- Copyright (c) Tuomo Valkonen 2006.
--
-- Ion is free software; you can redistribute it and/or modify it under
-- the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation; either version 2.1 of the License, or
-- (at your option) any later version.
--

local list={}

local function clear()
    mod_statusbar.inform('act_first', '--')
    mod_statusbar.inform('act_first_hint', 'normal')
    mod_statusbar.inform('act_n', '0')
    mod_statusbar.inform('act_n_hint', 'normal')
end

local function notifyact()
    mod_statusbar.inform('act_first', list[1]:name())
    mod_statusbar.inform('act_first_hint', 'critical')
    mod_statusbar.inform('act_n', tostring(table.getn(list)))
    mod_statusbar.inform('act_n_hint', 'critical')
end

local function hookhandler(reg)
    if reg:is_activity() then
        table.insert(list, reg)
    else
        for k, v in pairs(list) do
            if v==reg then
                table.remove(list, k)
                break
            end
        end
    end
    
    if table.getn(list)==0 then
        ioncore.defer(function() clear() mod_statusbar.update() end)
    else
        ioncore.defer(function() notifyact() mod_statusbar.update() end)
    end
end

ioncore.get_hook('region_activity_hook'):add(hookhandler)

clear()
mod_statusbar.inform('act_first_template', 'Mxxxxxxxxxxxxx')
mod_statusbar.update(true)
