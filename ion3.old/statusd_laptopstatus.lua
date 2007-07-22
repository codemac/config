-- statusd_laptopstatus.lua v0.0.2 (last modified 2005-06-13)
--
-- Copyright (C) 2005 Jari Eskelinen <jari.eskelinen@iki.fi>
--	modified by René van Bevern <rvb@pro-linux.de> for error handling
--
-- Permission to copy, redistirbute, modify etc. is granted under the terms
-- of GNU GENERAL PUBLIC LICENSE Version 2.
--
-- This is script for displaying some interesting information about your
-- laptops power saving in Ion's status monitor. Script is very Linux
-- specific (uses /proc interface) and needs ACPI -support new enough (don't
-- know exactly but 2.6.x kernels should be fine). Also if you have some
-- kind of exotic hardware (multiple processors, multiple batteries etc.)
-- this script probably will fail or show incorrect information.
--
-- Just throw this script under ~/.ion3 and add following keywords to your
-- cfg_statusbar.lua's template-line with your own taste:
--
-- %laptopstatus_cpuspeed
-- %laptopstatus_temperature
-- %laptopstatus_batterypercent
-- %laptopstatus_batterytimeleft
-- 
-- Template example: template="[ %date || load:% %>load || CPU: %laptopstatus_cpuspeed %laptopstatus_temperature || BATT: %laptopstatus_batterypercent %laptopstatus_batterytimeleft ]"
--
-- You can also run this script with lua interpreter and see if you get
-- right values.
--
-- NOTICE: This is my first ion/lua-script, so probably this can be done better.
-- Feel free to improve this script.
--
-- TODO: * Is it stupid to use file:read("*all"), can this cause infinite
--         loops in some weird situations?
--       * Do not poll for information not defined in template to be used
--       * Auto-detect right acpi devices instead of hardcoded BATT0 etc.

--
-- SETTINGS
--

if not statusd_laptopstatus then
  statusd_laptopstatus = {
    interval = 10,                    -- Polling interval in seconds
    temperature_important = 66,       -- Temperature which will cause important hint
    temperature_critical = 71,        -- Temperature which will cause critical hint
    batterypercent_important = 10,    -- Battery percent which will cause important hint
    batterypercent_critical = 5,      -- Battery percent which will cause critical hint
    batterytimeleft_important = 600,  -- Battery time left (in secs) which will cause important hint
    batterytimeleft_critical = 300,   -- Battery time left (in secs) which will cause critical hint
    ac_state = {"/proc/acpi/ac_adapter/AC/state",
    	        "/proc/acpi/ac_adapter/ACAD/state",
		"/proc/acpi/ac_adapter/ADP0/state",
		"/proc/acpi/ac_adapter/ADP1/state"},
    temp_info = {"/proc/acpi/thermal_zone/THRM/temperature",
                 "/proc/acpi/thermal_zone/THM0/temperature",
                 "/proc/acpi/thermal_zone/THM1/temperature"},
    bat_info = {"/proc/acpi/battery/BAT0/info",
                "/proc/acpi/battery/BAT1/info"},
    bat_state = {"/proc/acpi/battery/BAT0/state",
                 "/proc/acpi/battery/BAT1/state"}
  }
end

statusd_laptopstatus=table.join(statusd.get_config("laptopstatus"), statusd_laptopstatus)

--
-- CODE 
--
local laptopstatus_timer

function try_open(files, mode)
  for _, file in pairs(files) do
    local fd = io.open(file, mode)
    if fd then return fd, file end
  end
end

local function get_cpu()
  local mhz, hint
  if pcall(function ()
       local status
       local file = io.open("/proc/cpuinfo", "r")
       status, _, mhz = string.find(file:read("*all"),
                                    "cpu MHz%s+: (%d+)")
       if not status then error("could not get MHz") end
       file:close()
     end)
     then return {speed=string.format("%4dMHz", math.ceil(mhz/5)*5),
                  hint=hint}
     else return {speed="n/a", hint=hint} end
end


local function get_ac()
  local file = try_open(statusd_laptopstatus.ac_state, "r")
  if not string.find(file:read("*all"), "state:%s+on.line") then return 0
  else return 1 end
  file:close()
end


local function get_thermal()
  local temp, hint = nil, "normal"

  if pcall(function ()
       local status
       local file=try_open(statusd_laptopstatus.temp_info, "r")
       status, _, temp = string.find(file:read("*all"),
                                     "temperature:%s+(%d+).*")
       if not status then error("could not get temperature") end
       temp = tonumber(temp)
       file:close();
     end)
     then if temp >= statusd_laptopstatus.temperature_important then
             hint = "important" end
          if temp >= statusd_laptopstatus.temperature_critical then
	     hint = "critical" end
          return {temperature=string.format("%02dC", temp), hint=hint}
     else return {temperature="n/a", hint = "hint"} end
end


local function get_battery()
  local percenthint = "normal"
  local timelefthint = "normal"
  local lastfull, rate, remaining

  if pcall(function ()
       local status
       local file=try_open(statusd_laptopstatus.bat_info, "r")
       local infocontents = file:read("*all")
       file:close();
  
       local file=try_open(statusd_laptopstatus.bat_state, "r")
       local statecontents = file:read("*all")
       file:close();
  
       status, _, lastfull = string.find(infocontents, "last full capacity:%s+(%d+).*")
       if not status then error("could not get full battery capacity") end
       lastfull = tonumber(lastfull)
       if string.find(statecontents, "present rate:%s+unknown.*") then
           rate = -1
       else
           status, _, rate = string.find(statecontents, "present rate:%s+(%d+).*")
           if not status then error("could not get battery draining-rate") end
           rate = tonumber(rate)
       end
       status, _, remaining = string.find(statecontents, "remaining capacity:%s+(%d+).*")
       if not status then error("could not get remaining capacity") end
       remaining = tonumber(remaining)
     end) then
       local percent = math.floor(remaining / lastfull * 100 + 5/10)
       local timeleft
       local hours, secs, mins
       if get_ac() == 1 then
           timeleft = " *AC*"
       elseif rate <= 0 then
           timeleft = "n/a"
       else
          secs = 3600 * (remaining / rate)
          mins = secs / 60
          hours = math.floor(mins / 60)
          mins = math.floor(mins - (hours * 60))
          timeleft = string.format("%02d:%02d", hours, mins)
       end
  
       if secs ~= nil and secs <= statusd_laptopstatus.batterytimeleft_important then timelefthint = "important" end 
       if secs ~= nil and secs <= statusd_laptopstatus.batterytimeleft_critical then timelefthint = "critical" end
       if percent <= statusd_laptopstatus.batterypercent_important then percenthint = "important" end
       if percent <= statusd_laptopstatus.batterypercent_critical then percenthint = "critical" end
  
       return { percent=string.format("%02d%%", percent), timeleft=timeleft, percenthint=percenthint, timelefthint=timelefthint}
  else return { percent="n/a", timeleft="n/a", percenthint=percenthint, timelefthint=timelefthint} end
end

local last_timeleft = nil

local function update_laptopstatus ()
  cpu = get_cpu()
  thermal = get_thermal()
  battery = get_battery()

  -- Informing statusd OR printing to stdout if statusd not present
  if statusd ~= nil then
    statusd.inform("laptopstatus_cpuspeed", cpu.speed)
    statusd.inform("laptopstatus_cpuspeed_template", "xxxxMHz")
    statusd.inform("laptopstatus_cpuspeed_hint", cpu.hint)
    statusd.inform("laptopstatus_temperature", thermal.temperature)
    statusd.inform("laptopstatus_temperature_template", "xxxC")
    statusd.inform("laptopstatus_temperature_hint", thermal.hint)
    statusd.inform("laptopstatus_batterypercent", battery.percent)
    statusd.inform("laptopstatus_batterypercent_template", "xxx%")
    statusd.inform("laptopstatus_batterypercent_hint", battery.percenthint)
    if battery.timeleft ~= "n/a" or last_timeleft == " *AC*" then
        statusd.inform("laptopstatus_batterytimeleft", battery.timeleft)
        last_timeleft = battery.timeleft
    end
    statusd.inform("laptopstatus_batterytimeleft_template", "hh:mm")
    statusd.inform("laptopstatus_batterytimeleft_hint", battery.timelefthint)
    laptopstatus_timer:set(statusd_laptopstatus.interval*1000, update_laptopstatus)
  else
    io.stdout:write("CPU: "..cpu.speed.." "..thermal.temperature.." || BATT: "..battery.percent.." "..battery.timeleft.."\n")
  end
end


if statusd ~= nil then
    laptopstatus_timer = statusd.create_timer()
end
update_laptopstatus()
