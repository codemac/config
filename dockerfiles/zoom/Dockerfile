#
# Docker container for zoom, covid-19.
#
# docker run --rm -it -u `{id -u}^:`{getent group video | cut -d: -f3} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY --device /dev/dri '--device='^`{ls /dev/video*} -w $HOME '--net=host' --hostname `{hostname} -v $HOME/.config/zoomus.conf:$HOME/.config/zoomus.conf -v $HOME/.zoom:$HOME/.zoom -v $HOME/downloads:$HOME/downloads:ro -v $HOME/.cache/zoom:$HOME/.cache/zoom -v /run/user/^`{id -u}^/pulse:/run/pulse jmickey-zoom
#

FROM debian:testing

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get -qy dist-upgrade && apt-get install -qy \
  desktop-file-utils firefox-esr ibus ibus-gtk lib32z1 libegl1-mesa \
  libfontconfig1 libgl1-mesa-glx libglib2.0-0 libgstreamer-plugins-base1.0-0 \
  libgstreamer1.0-0 libnss3 libpulse0 libsm6 libsqlite3-0 libx11-6 \
  libxcb-image0 libxcb-keysyms1 libxcb-randr0 libxcb-shape0 libxcb-shm0 \
  libxcb-xfixes0 libxcb-xtest0 libxcomposite1 libxi6 libxrender1 libxslt1.1 \
  libxss1 sudo

ARG ZOOM_URL=https://zoom.us/client/latest/zoom_amd64.deb

# I use ADD instead of curl & rm because ADD checks if the url etag
# has changed.

ADD $ZOOM_URL /tmp/zoom_setup.deb
RUN dpkg -i /tmp/zoom_setup.deb
RUN apt-get -f install

ENTRYPOINT ["/usr/bin/zoom"]
