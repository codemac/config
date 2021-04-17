#
# Docker container for telegram.
#
# docker run --rm -it -u `{id -u}^:`{getent group video | cut -d: -f3} -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY --device /dev/dri '--device='^`{ls /dev/video*} -w $HOME '--net=host' --hostname `{hostname} -v /etc/localtime:/etc/localtime:ro -v $HOME/.TelegramDesktop:$HOME/.local/share/TelegramDesktop/ -v $HOME/downloads:$HOME/downloads:ro -v /run/user/^`{id -u}^/pulse:/run/pulse jmickey-telegram
#

FROM debian:testing

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get -qy dist-upgrade && apt-get install -qy \
    apt-utils \
    software-properties-common \
    wget \
    dbus-x11 \
    dunst \
    hunspell-en-us \
    python3-dbus \
    software-properties-common \
    libx11-xcb1 \
    libpulse0 \
    gconf2 \
    libdrm2 \
    libice6 \
    libsm6 \
    libegl1-mesa-dev \
    libgl1-mesa-glx \
    xz-utils \
    --no-install-recommends && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /tmp

RUN wget https://telegram.org/dl/desktop/linux -O /tmp/telegram.tar.xz
RUN tar xf /tmp/telegram.tar.xz

ENV QT_XKB_CONFIG_ROOT=/usr/share/X11/xkb

CMD ["/tmp/Telegram/Telegram"]
