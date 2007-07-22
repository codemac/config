-- statusd_iwinfo.lua
--
-- Copyright (c) Relu Patrascu 2004.
--
-- Ion is free software; you can redistribute it and/or modify it under
-- the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation; either version 2.1 of the License, or
-- (at your option) any later version.
--
-- Nov. 5, 2004
-- Disclaimer
-- Neither am I a lua expert nor do I have the time to invest in writing
-- better code here.  I simply needed this utility and it works OK for me.
-- I give no guarantees of any kind for this code.  Suggestions for
-- improvement are welcome.
-- ikoflexer at gmail dot com

-- Nov 17, 2004
-- ikoflexer at gmail dot com
--
-- Made it return a space " " instead of empty string ""
-- when /proc/net/wireless doesn't report any interface.
-- Otherwise old info lingers in the status bar.

if not statusd_iwinfo then
  statusd_iwinfo = { interval = 10*1000 }
end

local proto, ssid, bitrate, linkq, signal, noise

local function get_iwinfo_iwcfg()
	local f = io.open('/proc/net/wireless', 'r')
	if not f then
		return ""
	end
-- first 2 lines -- headers
	f:read('*l')
	f:read('*l')
-- the third line has wifi info
	local s = f:read('*l')
	f:close()
	local st, en, iface = 0, 0, 0
	if not s then
		return tostring(" ")
	end
	st, en, iface = string.find(s, '(%w+):')
	local f1 = io.popen("/sbin/iwconfig " .. iface)
	if not f1 then
		return tostring(" ")
	else
		local iwOut = f1:read('*a')
		f1:close()
		st,en,proto = string.find(iwOut, '(802.11[%-]*%a+)')
		if not proto then
			proto = " N/A"
		else
			proto = " "..proto
		end
		st,en,ssid = string.find(iwOut, 'ESSID[=:]"([%w+%s*]+)"', en)
		if not ssid then
			ssid = "N/A "
		else
			ssid = ssid.." "
		end
		st,en,bitrate = string.find(iwOut, 'Bit Rate[=:](%w+[%s%w]*%/%a+)', en)
		bitrate = string.gsub(bitrate, "%s", "")
		if not bitrate then
			bitrate = "N/A "
		else
			bitrate = bitrate.." "
		end
		st,en,linkq = string.find(iwOut, 'Link Quality[=:](%d+%/%d+)', en)
		if not linkq then
			linkq = "N/A "
		else
			linkq = linkq.." "
		end
		st,en,signal = string.find(iwOut, 'Signal level[=:](%-%d+)', en)
		if not signal then
			signal = "N/A "
		end
		st,en,noise = string.find(iwOut, 'Noise level[=:](%-%d+)', en)
		if not noise then
			noise = "N/A"
		end
		return tostring(ssid .. bitrate .. linkq .. signal .. "/" .. noise .. "dBm" .. proto)
	end
end

local iwinfo_timer

local function update_iwinfo()
	statusd.inform("iwinfo_cfg", get_iwinfo_iwcfg())
	statusd.inform("iwinfo_proto", proto)
	statusd.inform("iwinfo_ssid", ssid)
	statusd.inform("iwinfo_bitrate", bitrate)
	statusd.inform("iwinfo_linkq", linkq)
	statusd.inform("iwinfo_signal", signal)
	statusd.inform("iwinfo_noise", noise)
	iwinfo_timer:set(statusd_iwinfo.interval, update_iwinfo)
end

-- Init
iwinfo_timer = statusd.create_timer()
update_iwinfo()
