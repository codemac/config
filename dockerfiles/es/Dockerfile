#
# docker run --rm -it -u `{id -u}^:^`{id -g} --rm -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY -w $HOME '--net=host' jmickey-es
#

FROM archlinux/base

RUN pacman -Sy --noconfirm wget

RUN wget -O /tmp/es.tar.gz https://github.com/wryun/es-shell/releases/download/v0.9.1/es-0.9.1.tar.gz

RUN pacman -Sy --noconfirm gcc tar automake autoconf m4 grep gettext libtool make


WORKDIR /tmp
RUN tar xf /tmp/es.tar.gz
#RUN ls -l
#RUN rm -rf autom4te
#RUN autoreconf -vfi
#RUN ls -l
RUN ./configure --prefix=/usr
RUN pacman -Sy --noconfirm bison
RUN make
RUN make install

ENTRYPOINT ["/usr/bin/es"]
