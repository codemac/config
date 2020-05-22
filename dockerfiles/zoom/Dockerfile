#
# Docker container for zoom, covid-19.
#
# docker run --rm -it -u `{id -u}^:`{getent group video | cut -d: -f3} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY --device /dev/dri --device /dev/video0 --device /dev/video1 -w $HOME '--net=host' --hostname `{hostname} -v $HOME/.config/zoomus.conf:$HOME/.config/zoomus.conf -v $HOME/.zoom:$HOME/.zoom -v $HOME/downloads:$HOME/downloads:ro -v $HOME/.cache/zoom:$HOME/.cache/zoom -v /run/user/^`{id -u}^/pulse:/run/pulse jmickey-zoom
#

FROM debian:testing

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get -qy dist-upgrade

RUN apt-get install -qy curl firefox-esr sudo desktop-file-utils lib32z1 \
  libx11-6 libegl1-mesa libxcb-shm0 \
  libglib2.0-0 libgl1-mesa-glx libxrender1 libxcomposite1 libxslt1.1 \
  libgstreamer1.0-0 libgstreamer-plugins-base1.0-0 libxi6 libsm6 \
  libfontconfig1 libpulse0 libsqlite3-0 \
  libxcb-shape0 libxcb-xfixes0 libxcb-randr0 libxcb-image0 \
  libxcb-keysyms1 libxcb-xtest0 ibus ibus-gtk \
  libnss3 libxss1

ARG ZOOM_URL=https://zoom.us/client/latest/zoom_amd64.deb

# I use ADD instead of curl & rm because ADD checks if the url etag
# has changed.

ADD $ZOOM_URL /tmp/zoom_setup.deb
RUN dpkg -i /tmp/zoom_setup.deb
RUN apt-get -f install

ENTRYPOINT ["/usr/bin/zoom"]
