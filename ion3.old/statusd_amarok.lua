-- statusd_amarok.lua
--
-- Public domain
--
-- Use the key "amarok" to get the currently playing song in amaroK.
-- Uses dcop.  TODO: which is faster, MySQL or dcop??

local defaults={ update_interval=3*1000, }
local settings=table.join(statusd.get_config("amarok"), defaults)

function amarok_do_call_dcop()
        local f=io.popen('dcop amarok player nowPlaying 2> /dev/null')
        local amarok = f:read('*l')
        f:close()
        -- strip off a newline
        local i, j, amarok = string.find( amarok, '(%C*)')
        return amarok
end

function amarok_do_get_status()        
        local f=io.popen('dcop amarok player status 2> /dev/null')
	local status = f:read('*l')
	f:close()
        return status
end
        
function get_amarok()
        local status = amarok_do_get_status()

        if status == "0" then
                return "stopped"
        elseif status == "1" then
                return amarok_do_call_dcop().." (paused)"
        elseif status == "2" then
                return amarok_do_call_dcop()
        else
                return "not running"
        end
end

function update_amarok()
	local amarok = get_amarok()
	statusd.inform("amarok", amarok)
	amarok_timer:set(settings.update_interval, update_amarok)
end

amarok_timer = statusd.create_timer()
update_amarok()
