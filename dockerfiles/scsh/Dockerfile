#
# docker run --rm -it -u `{id -u}^:^`{id -g} --rm -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY -w $HOME '--net=host' jmickey-scsh
#

FROM archlinux/base

RUN pacman -Sy --noconfirm wget tar gcc automake autoconf m4 make

RUN wget -O /tmp/scheme48.tar.gz http://s48.org/1.9.2/scheme48-1.9.2.tgz
WORKDIR /tmp
RUN tar xf /tmp/scheme48.tar.gz
WORKDIR /tmp/scheme48-1.9.2
RUN pacman -Sy --noconfirm grep
RUN ./configure --prefix=/usr
RUN make
RUN make install


RUN pacman -Sy --noconfirm git
RUN git clone http://github.com/scheme/scsh /scsh_git
WORKDIR /scsh_git
RUN git submodule update --init
RUN autoreconf
RUN ./configure --with-scheme48=/usr --prefix=/usr
RUN make
RUN make install

ENTRYPOINT ["/usr/bin/scsh"]
