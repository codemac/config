##!/bin/sh
#
##
## ~/.xinitrc
##
## Executed by startx (run your window manager from here)
##

xmodmap ~/.Xmodmap
xset m 4 5
xset dpms 60
# background
#display -window root ~/bin/background.png
exec `cat ~/.fehbg` &
#start apps here

# show reminders as needed!
#
#remind -z1 -k'notify-send --urgency=low --expire-time=0 "remind says" \"%s\"' ~/.reminders &

#gkrellm &
#xclock &
#aoss firefox &
#urxvt & #-e screen &
#ruby ~/hacks/dwm.rb | dzen2 -fn "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-iso8859-1" -bg "#aecf96" -fg "#2c2c32" -p 2 -e '' -y 786 -ta r &
#zsh ~/hacks/mon.zsh | dzen2 -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859' -bg '#2c2c32' -fg '#aecf96' -p 2 -e '' -ta c -y 786 &

# get mocp running so it's not a child of the status blah
mocp -S

#wm's
#exec openbox
#exec ratpoison
#exec pekwm
#exec /usr/bin/wmii
#exec /usr/bin/ion3
#exec /usr/bin/fvwm
#exec /home/codemac/.xmonad/xmonad
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
#amazing &
#exec awesome
ruby /home/codemac/hacks/dwm.rb | /usr/bin/dwm
#exec awesome
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
