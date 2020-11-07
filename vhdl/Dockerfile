# Make Sure vxSrv is running

FROM ghdl/ghdl:ubuntu20-mcode
RUN apt update && apt install -y gtkwave
# replace IP with your local IP
ENV DISPLAY=host.docker.internal:0.0 
COPY . /vwlwah
WORKDIR /vwlwah
CMD make wave