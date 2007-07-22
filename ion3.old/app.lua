--
-- By Jeremy Hankins <nowan@nowan.org>
-- Multi-head support added by Johannes Segitz
-- Cycling contributed by
--   Antti Kaihola <antti.kaihola@linux-aktivaattori.org>
--
-- Time-stamp: <2005-11-16 12:02:46 Jeremy Hankins>
--             <2005-08-26 11:35:00 kaihola>
--
-- With these functions you can bind a key to start an application if
-- it's not already running, or cycle its windows if it is.  You can
-- use it by doing something like this in your bindings file:
--
-- kpress(MOD1.."C", "app.byname('xterm -title shell', 'shell')"),
-- kpress(MOD1.."T", "app.byclass('emacs', 'Emacs')"),
--
-- The byname function expects an executable and a window title, the
-- byclass expects a class.
--
-- NOTE: cycling windows requires ioncore.current(), so it will only
-- work with snapshot XXX or later.
--
-- If you use a multihead setup you can use something like this to start
-- applictions on your current screen
--
-- kpress(MOD1.."C", "app.byname('xterm -title shell', 'shell', _)"),
-- kpress(MOD1.."T", "app.byclass('emacs', 'Emacs', _)"),
--
-- For emacs users there's also app.emacs_eval, and app.query_editfile,
-- which interacts with any currently running emacs process (using
-- gnuclient), or starts emacs to run the given command.
-- app.query_editfile is a replacement for query_lib.query_editfile to
-- use the currently running emacs rather than ion-edit.
--

app={}

function app.match_class(class)
   -- Return matching client windows as a table.
   -- If the current window is among them, it's placed last on the list
   -- and its successor at the beginning of the list. This facilitates
   -- cycling multiple windows of an application.
   local wins = ioncore.clientwin_list()
   if not wins then return {} end

   local result = {}
   local offset = 0
   local currwin = pcall(ioncore.current)

   for i, win in pairs(wins) do
      if class == win:get_ident().class then
	 table.insert(result, table.getn(result)-offset+1, win)
      end
      if win == currwin then
	 -- Current client window found, continue filling the table from
	 -- the beginning.
	 offset = table.getn(result)
      end
   end
   return result
end

function app.byname(prog, name, where)
   local win = ioncore.lookup_clientwin(name)
   if win then
      win:goto()
   else
      if where then
	  ioncore.exec_on(where, prog)
      else
	  ioncore.exec(prog)
      end
   end
end

function app.byclass(prog, class, where)
   local win = app.match_class(class)[1]
   if win then
      win:goto()
   else
      if where then
	  ioncore.exec_on(where, prog)
      else
	  ioncore.exec(prog)
      end
   end
end

function app.emacs_eval(expr)
   local emacswin = app.match_class("Emacs")[1]
   if emacswin then
      ioncore.exec("gnuclient -batch -eval '"..expr.."'")
      emacswin:goto()
   else
      ioncore.exec("emacs -eval '"..expr.."'")
   end
end

function app.query_editfile(mplex, dir) 
   local function handler(file)
      app.emacs_eval("(find-file \""..file.."\")")
   end

   mod_query.do_query(mplex,
		     'Edit file:',
		     dir or mod_query.get_initdir(),
		     handler,
		     mod_query.file_completor)
end



