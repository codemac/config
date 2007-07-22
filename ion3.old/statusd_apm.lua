-- Public domain, written by Greg Steuck
-- Allows displaying apm information in the statusbar.
-- To install:
--    save the file into ~/.ion3/statusd_apm.lua,
--    copy the default cfg_statusbar.lua to ~/.ion3, edit it to include:
--       apm={} into mod_statusbar.launch_statusd call
--       some of %apm_{pct, ac, estimate, state} into template passed to mod_statusbar.create
--          e.g. template="[ %date || load:% %>load_1min || battery %apm_pct%%, A/C %apm_ac ]",

local unknown = "?", "?", "?", "?"

-- Runs apm utility and grabs relevant pieces from its output.
-- Most likely will only work on OpenBSD due to reliance on its output pattern.
function get_apm()
    local f=io.popen('/usr/sbin/apm', 'r')
    if not f then
        return unknown
    end
    local s=f:read('*all')
    f:close()
    local _, _, state, pct, estimate, ac = 
	string.find(s, "Battery state: (.*)\n"..
		       "Battery remaining: (.*) percent\n"..
		       "Battery life estimate: (.*) minutes\n"..
		       "A/C adapter state: ([^\n]*)\n")
    if not state then
	return unknown
    end
    return state, pct, estimate, ac
end

local function inform(key, value)
    statusd.inform("apm_"..key, value)
end

local apm_timer = statusd.create_timer()

local function update_apm()
    local state, pct, estimate, ac = get_apm()
    inform("state", state)
    inform("pct", pct)
    inform("estimate", estimate)
    inform("ac", ac)
    apm_timer:set(60*1000, update_apm)
end

update_apm()

