-- Bind to this function instead of the normal context menu one for a frame
-- and you will get an additional submenu containing the client windows in the
-- frame. Selecting a client window will switch to that client.
--
-- Set start_on_current to true if you want the submenu to open with the
-- current client window highlighted instead of the first window highlighted.

function clientlist_menu(frame, cwin)
	local start_on_current = false

	if mod_menu then
		local framemenufn = ioncore.getmenu("ctxmenu")

		local framemenu = framemenufn(frame, cwin)
		local myframemenu = {}

		local cwins = frame:llist(1)

		local index = 1
		local initial = 1
		table.foreach(cwins, function(k, v)
			                     if v == cwin then
				                     initial = index
			                     end
			                     index = index + 1
			                     table.insert(myframemenu, ioncore.menuentry(v:name(), function() v:goto() end))
		                     end)
		if start_on_current then
			table.insert(framemenu, ioncore.submenu("Client windows", myframemenu, {initial=initial}))
		else
			table.insert(framemenu, ioncore.submenu("Client windows", myframemenu, {initial=1}))
		end
		mod_menu.menu(frame, cwin, framemenu)
	end
end
