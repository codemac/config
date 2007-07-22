-- Cycle through frames on a workspace.
-- To use, bind "framecycle.cycle(_)" to a key.

local framecycle={}
_G["framecycle"]=framecycle

function framecycle.tlmost(s)
    if obj_is(s, "WSplitSplit") then
        local s2=framecycle.tlmost(s:tl())
        if not s2 then
            s2=framecycle.tlmost(s:br())
        end
        return s2
    end
    
    if obj_is(s, "WSplitRegion") and not obj_is(s, "WSplitST") then
        return s
    end
end


function framecycle.nextnode(s)
    local p=s:parent()
    local s2
    
    if not p then
        return nil
    end
    
    if p:br()~=s then
        s2=framecycle.tlmost(p:br())
        if s2 then 
            return s2
        end
    end
    
    s2=framecycle.nextnode(p)
    if not s2 and p:br()==s then
        s2=framecycle.tlmost(p:tl())
    end
    return s2
end

function framecycle.cycle(frame)
    local ws=frame:manager()
    if not obj_is(ws, "WIonWS") then
        warn("Expecting WIonWS")
        return
    end
    
    local s=ws:node_of(frame)
    
    local s2=framecycle.nextnode(s)
    if s2 then
        s2:reg():goto()
    end
end
