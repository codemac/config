-- A version of WIonWS.goto_dir that may be useful on multihead setups
-- Replace WIonWS.goto_dir with goto_multihead.goto_dir in cfg_ionws.lua
-- to use it.

goto_multihead={}

function goto_multihead.goto_dir(ws, dir)
    if dir=="up" or dir=="down" then
        ws:goto_dir(dir)
        return
    end
    
    local nxt, nxtscr
    
    nxt=ws:nextto(ws:current(), dir)
    
    if not nxt then
        local otherdir
        local fid=ioncore.find_screen_id
        if dir=="right" then
            otherdir="left"
            nxtscr=fid(ws:screen_of():id()+1) or fid(0)
        else
            otherdir="right"
            nxtscr=fid(ws:screen_of():id()-1) or fid(-1)
        end
        nxt=nxtscr:current()
        if obj_is(nxt, "WIonWS") then
            nxt=nxt:farthest(otherdir)
        end
    end
    
    nxt:goto()
end
