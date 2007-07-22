-- Move all windows on a WIonWS to a single frame and destroy the rest.
-- (like C-x 1 in Emacs)
-- This is the ion3 version.

collapse={}


function collapse.move_managed(tgt, src)
   local l=WMPlex.llist(src,1)
   for _, m in pairs(l) do
      tgt:attach(m)
   end
end

function collapse.collapse(ws)
   local l=ws:managed_list()
   local tgt=ws:current()
   for _, f in pairs(l) do
      if obj_is(f, "WMPlex") then
         if tgt ~= f then
            collapse.move_managed(tgt, f)
            f:rqclose()
         end
      end
   end
   WRegion.goto(tgt)
end
