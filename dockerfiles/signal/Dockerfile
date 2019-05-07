#
# docker run --rm -d -u `{id -u}^:^`{id -g} --rm -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY -v $HOME/.config/Signal:$HOME/.config/Signal -v /tmp/.X11-unix:/tmp/.X11-unix -w $HOME '--net=host' jmickey-signal
#

FROM archlinux/base

RUN pacman -Sy --noconfirm gconf gtk3 libnotify nss xdg-utils libxss
RUN pacman -Sy --noconfirm wget
RUN pacman -Sy --noconfirm libarchive
RUN wget -O /tmp/signal-desktop_1.22.0_amd64.deb \
    'https://updates.signal.org/desktop/apt/pool/main/s/signal-desktop/signal-desktop_1.22.0_amd64.deb'
RUN bsdtar xf /tmp/signal-desktop_1.22.0_amd64.deb -C /
RUN tar xf /data.tar.xz -C /

ENTRYPOINT ["/opt/Signal/signal-desktop"]
