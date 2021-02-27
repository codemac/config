#
# Docker container for bluejeans, covid-19.
#
# docker run --rm -it -u `{id -u}^:`{getent group video | cut -d: -f3} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY --device /dev/dri '--device='^`{ls /dev/video*} -w $HOME '--net=host' --hostname `{hostname} -v $HOME/downloads:$HOME/downloads:ro -v $HOME^'/.config/Blue Jeans:'^$HOME^'/.config/Blue Jeans' -v $HOME/.config/bluejeans-v2:$HOME/.config/bluejeans-v2 -v $HOME/.config/autostart:$HOME/.config/autostart -v /run/user/^`{id -u}^/pulse:/run/pulse jmickey-bluejeans
#

FROM debian:testing

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get -qy dist-upgrade && \
 apt-cache search libatyatana-appinidicator3-dev
RUN apt-get install -qy --no-install-recommends \
        ca-certificates \
        wget \
        libnss3 \
        gconf2 \
        libnotify4 \
	libayatana-appindicator3-dev \
        libxtst6 \
        libxss1 \
        libgtk-3-0 \
        libx11-xcb1 \
        libasound2 \
	&& rm -rf /var/lib/apt/lists/*

ARG BJN_URL=https://swdl.bluejeans.com/desktop-app/linux/2.21.2/BlueJeans_2.21.2.1.deb
ADD $BJN_URL /tmp/bjn.deb
RUN dpkg -i /tmp/bjn.deb
RUN apt-get -f install

CMD ["/opt/BlueJeans/bluejeans-v2"]