--[[
    lock_frame.lua

    Setup: Add dopath("lock_frame") to cfg_ion.lua/cfg_modules.lua

    Usage: F11 toggles the lock of keyboard close and resize on the currently
           active frame.

    Author: James Gray <james at solitaryfield dot org> (yaarg in #ion)

    Date: Sat Dec 17 01:34:22 GMT 2005

    Last Modified: Fri Apr 21 14:37:44 BST 2006 
]]

lock_frame = {}
local locks = {}

function lock_frame.toggle_lock()
    frame = ioncore.current()
    i = 1
    for _, f in pairs(locks) do
        if f == frame then
            table.remove(locks, i)
            return
        end
        i = i + 1
    end
    table.insert(locks, frame)
end

function lock_frame.check(frame)
    if not locks then return end

    ioncore.defer(function() 
        ioncore.defbindings("WMPlex",{
            kpress_wait(META.."C",
            "WRegion.rqclose_propagate(_, _sub)")
        })
    end)

    ioncore.defer(function()
        ioncore.defbindings("WFrame",{
            kpress_wait(META.."R",
            "WFrame.begin_kbresize(_)")
        })
    end)

    for _, f in pairs(locks) do
        if f == frame then
            ioncore.defer(function()
            -- disable close function
            ioncore.defbindings("WMPlex",{
                    -- print is bound here instead of nil so the key strokes
                    -- don't pass through to the underlying app and have
                    -- potentially dangerous consquences..!
                    kpress(META.."C", "print")
                })
            end)
            -- disable resize function
            ioncore.defer(function()
                ioncore.defbindings("WFrame",{
                    kpress(META.."R", "print")
                })
            end)
            -- disable something else here?
            return
        end
    end
end

function lock_frame.init()
    local hook = ioncore.get_hook("region_activated_hook")

    if hook then
        hook:add(lock_frame.check)
    end 

    ioncore.defbindings("WScreen",{
        kpress("F11", "lock_frame.toggle_lock()")
    })
end

lock_frame.init()
