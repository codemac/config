#
# You have to first run this and commit the login stuff
#
#
# docker run --rm -u `{id -u}^':'^`{id -g} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY -w $HOME '--net=host' --ipc host --hostname `{hostname} -v $HOME/Dropbox:$HOME/Dropbox -v $HOME/.dropbox-dist:$HOME/.dropbox-dist -v $HOME/.dropbox:$HOME/.dropbox jmickey-dropbox dropbox start -i ; docker run --rm -u `{id -u}^':'^`{id -g} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY -w $HOME '--net=host' --hostname `{hostname} -v $HOME/Dropbox:$HOME/Dropbox -v $HOME/.dropbox-dist:$HOME/.dropbox-dist -v $HOME/.dropbox:$HOME/.dropbox jmickey-dropbox $HOME/.dropbox-dist/dropboxd
#

FROM debian:jessie
MAINTAINER Jeff Mickey <j@codemac.net>
ENV DEBIAN_FRONTEND noninteractive
ENV QT_DEBUG_PLUGINS 1
RUN echo 'deb http://linux.dropbox.com/debian jessie main' > /etc/apt/sources.list.d/dropbox.list \
        && apt-get -qqy update \
	&& apt-get -qqy install gnupg
	
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E

RUN apt-get -qqy update \
        && apt-get -qqy install ca-certificates curl python3-gpgme dropbox libc6 libglapi-mesa libxdamage1 libxfixes3 libxcb-glx0 libxcb-dri2-0 libxcb-dri3-0 libxcb-present0 libxcb-sync1 libxshmfence1 libxxf86vm1 libxcb-xinerama0 libqt5dbus5 libqt5widgets5 libqt5network5 libqt5gui5 libqt5core5a libdouble-conversion1 libxcb-xinerama0 \
	&& apt-get -qqy autoclean \
	&& rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

#ENTRYPOINT ["/usr/bin/dropbox"]
#ENTRYPOINT ["/home/codemac/.dropbox-dist/dropboxd"]
