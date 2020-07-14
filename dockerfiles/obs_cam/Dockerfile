#
# run this modprobe first
# sudo modprobe v4l2loopback devices=1 video_nr=10 card_label="OBS Cam" exclusive_caps=1
#
# docker run --rm -it -u `{id -u}^:44 -e HOME -e DISPLAY -e XAUTHORITY -v $XAUTHORITY:$XAUTHORITY --device /dev/dri --device /dev/video10 --device /dev/video0 --device /dev/video1 --device /dev/video2 --device /dev/video3 -w $HOME '--net=host' --hostname `{hostname} -v $HOME/.config/obs-studio:$HOME/.config/obs-studio -v $HOME/downloads:$HOME/downloads:ro -v $HOME/lectures:$HOME/lectures:ro jmickey-obs_cam
#

FROM debian:testing
RUN apt upgrade && apt update
RUN apt install -y obs-studio
RUN mkdir /obs_build
RUN cd /obs_build 
RUN apt install -y qtbase5-dev git cmake gcc g++ libobs-dev
RUN git clone --recursive https://github.com/obsproject/obs-studio.git
RUN git clone https://github.com/CatxFish/obs-v4l2sink.git
RUN cd obs-v4l2sink && mkdir build && cd build && cmake -DLIBOBS_INCLUDE_DIR="../../obs-studio/libobs" -DCMAKE_INSTALL_PREFIX=/usr .. && make -j4 && make install
RUN cp /usr/lib/obs-plugins/v4l2sink.so /usr/lib/x86_64-linux-gnu/obs-plugins/v4l2sink.so

CMD ["obs"]
