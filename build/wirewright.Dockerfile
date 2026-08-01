FROM crystallang/crystal:1.21.0

# Install dependencies
RUN apt-get update \
  && apt-get install --no-install-recommends -y \
       libfribidi-dev libharfbuzz-dev libfreetype-dev libxxhash-dev tzdata xz-utils \
       meson file wget cmake gnome-desktop-testing libasound2-dev libpulse-dev libaudio-dev \
       libfribidi-dev libjack-dev libsndio-dev libx11-dev libxext-dev libxrandr-dev libxcursor-dev \
       libxfixes-dev libxi-dev libxss-dev libxtst-dev libxkbcommon-dev libdrm-dev libgbm-dev \
       libgl1-mesa-dev libgles2-mesa-dev libegl1-mesa-dev libdbus-1-dev libibus-1.0-dev \
       libudev-dev libthai-dev libusb-1.0-0-dev libsqlite3-dev \
  && apt-get clean

# Download linuxdeploy
RUN wget -O linuxdeploy.AppImage https://github.com/linuxdeploy/linuxdeploy/releases/download/1-alpha-20251107-1/linuxdeploy-x86_64.AppImage \
  && chmod +x linuxdeploy.AppImage

# Download and Build SDL3
# NOTE: Job count is set to 2 so that my PC doesn't explode.
WORKDIR /
RUN wget -O libsdl3.tar.gz https://github.com/libsdl-org/SDL/releases/download/release-3.4.8/SDL3-3.4.8.tar.gz \
  && mkdir libsdl3 \
  && tar xf libsdl3.tar.gz -C libsdl3 --strip-components 1 \
  && rm libsdl3.tar.gz \
  && cd libsdl3 \
  && mkdir build \
  && cd build \
  && cmake -DCMAKE_BUILD_TYPE=Release .. \
  && cmake --build . --config Release -j 2 \
  && cmake --install . --config Release

# Download and build Wirewright dependencies

# Build raqm 0.10.5
WORKDIR /
RUN wget -O raqm.tar.xz https://github.com/HOST-Oman/libraqm/releases/download/v0.10.5/raqm-0.10.5.tar.xz \
  && mkdir raqm \
  && tar xf raqm.tar.xz -C raqm --strip-components 1 \
  && rm raqm.tar.xz \
  && cd raqm \
  && meson setup build -Ddefault_library=static \
  && ninja -C build \
  && ninja -C build install

# Build PlutoVG 1.3.3
WORKDIR /
RUN wget -O plutovg.tar.gz https://github.com/sammycage/plutovg/archive/refs/tags/v1.3.3.tar.gz \
  && mkdir plutovg \
  && tar xf plutovg.tar.gz -C plutovg --strip-components 1 \
  && rm plutovg.tar.gz \
  && cd plutovg \
  && meson setup build -Ddefault_library=static \
  && ninja -C build \
  && ninja -C build install

# Build PlutoSVG 0.0.8
WORKDIR /
RUN wget -O plutosvg.tar.gz https://github.com/sammycage/plutosvg/archive/refs/tags/v0.0.8.tar.gz \
  && mkdir plutosvg \
  && tar xf plutosvg.tar.gz -C plutosvg --strip-components 1 \
  && rm plutosvg.tar.gz \
  && cd plutosvg \
  && PKG_CONFIG_PATH="/usr/local/lib64/pkgconfig" meson setup build -Ddefault_library=static --wrap-mode=nofallback \
  && ninja -C build \
  && ninja -C build install

# Build libunibreak 7.0
WORKDIR /
RUN wget -O libunibreak.tar.xz https://github.com/adah1972/libunibreak/releases/download/libunibreak_7_0/libunibreak-7.0.tar.gz \
  && mkdir unibreak \
  && tar xf libunibreak.tar.xz -C unibreak --strip-components 1 \
  && rm libunibreak.tar.xz \
  && cd unibreak \
  && ./configure \
  && make \
  && make install

# Build xxhash
# Use the xxhash version we've installed.

# Update ld cache
RUN ldconfig

# Add Wirewright
WORKDIR /
ADD --unpack=true wirewright.tar.gz /wirewright

# Install shards
WORKDIR /wirewright
RUN shards install

# Build devtool
RUN crystal build src/dev.cr --progress -Dpreview_mt -Dexecution_context -Dsyslibs
