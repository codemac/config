-- Move current window in a frame to another frame in specified direction

move_current={}

function move_current.move(frame, dir)
    local curr=frame:current()
    local mgr=frame:manager()
    
    if not curr or not obj_is(mgr, "WIonWS") then 
        return 
    end
    
    local frame2=mgr:nextto(frame, dir)
    
    if frame2 then
        frame2:attach(curr, { switchto=true })
    end
end

defbindings("WFrame-on-WIonWS", {
    submap("Mod1+K", {
        kpress("Up", function(f) move_current.move(f, "above") end),
        kpress("Down", function(f) move_current.move(f, "below") end),
        kpress("Left", function(f) move_current.move(f, "left") end),
        kpress("Right", function(f) move_current.move(f, "right") end),
    }),
})
