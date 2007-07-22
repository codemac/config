##!/bin/sh
#
##
## ~/.xinitrc
##
## Executed by startx (run your window manager from here)
##
##exec /usr/bin/fluxbox 
xmodmap ~/.Xmodmap
xset m 4 5
xset dpms 60
# background
#display -window root ~/bin/background.png
exec `cat ~/.fehbg` &
#start apps here

#gkrellm &
#xclock &
#aoss firefox &
#urxvt & #-e screen &


#wm's
#exec openbox
#exec ratpoison
#exec pekwm
#exec /usr/bin/wmii
#exec /usr/bin/ion3
#exec /usr/bin/fvwm
#exec startkde
#exec icewm
#exec blackbox
#exec fluxbox
#exec /usr/bin/dwm
#while true; do
#	ruby /home/codemac/bin/dwm.rb
#done | /usr/bin/dwm
#while true; do
#	echo "`date +'%A %B %d, %Y'` `uptime`"
#	sleep 1
#done | /usr/bin/dwm
ruby /home/codemac/hacks/dwm.rb | /usr/bin/dwm

############ For Beryl!
#. /etc/profile
#
#xrdb $HOME/.Xresources
#
## start beryl and beryl-manager
#beryl --use-tfp &
#emerald &
#beryl-manager &
## start necessary xfce components
#xfce-mcs-manager
#xfdesktop & 
#xfce4-panel & wmpid=$!
#
## start any other needed programs here
#
#wait $wmpid
