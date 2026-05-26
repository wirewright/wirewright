FROM debian:bookworm-slim

# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    tzdata gcc pkg-config libssl-dev libxml2-dev libyaml-dev libgmp-dev \
    git make libpcre2-dev libz-dev libgc-dev libfribidi-dev libharfbuzz-dev \
    libfreetype-dev xz-utils meson file wget cmake gnome-desktop-testing \
    libasound2-dev libpulse-dev libaudio-dev libjack-dev libsndio-dev \
    libx11-dev libxext-dev libxrandr-dev libxcursor-dev libxfixes-dev \
    libxi-dev libxss-dev libxtst-dev libxkbcommon-dev libdrm-dev libgbm-dev \
    libgl1-mesa-dev libgles2-mesa-dev libegl1-mesa-dev libdbus-1-dev \
    libibus-1.0-dev libudev-dev libthai-dev libusb-1.0-0-dev ca-certificates \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Download and Build SDL3
WORKDIR /tmp
RUN wget -O libsdl3.tar.gz https://github.com/libsdl-org/SDL/releases/download/release-3.4.8/SDL3-3.4.8.tar.gz && \
    mkdir libsdl3 && \
    tar xf libsdl3.tar.gz -C libsdl3 --strip-components 1 && \
    cd libsdl3 && mkdir build && cd build && \
    cmake -DCMAKE_BUILD_TYPE=Release .. && \
    cmake --build . --config Release --parallel && \
    cmake --install . --config Release && \
    cd /tmp && rm -rf libsdl3 libsdl3.tar.gz

# Install Crystal & Add to PATH
WORKDIR /
RUN wget -O crystal.tar.gz https://github.com/crystal-lang/crystal/releases/download/1.20.2/crystal-1.20.2-1-linux-x86_64.tar.gz && \
    mkdir crystal && \
    tar xf crystal.tar.gz -C crystal --strip-components 1 && \
    rm crystal.tar.gz
ENV PATH="/crystal/bin:${PATH}"

# Download linuxdeploy
RUN wget -O linuxdeploy.AppImage https://github.com/linuxdeploy/linuxdeploy/releases/download/1-alpha-20251107-1/linuxdeploy-x86_64.AppImage && \
    chmod +x linuxdeploy.AppImage

# Clone Wirewright and prepare vendor builds
RUN git clone --depth 1 https://github.com/wirewright/wirewright
WORKDIR /wirewright/vendor

# Build raqm 0.10.5
RUN cd raqm && \
    wget -O raqm.tar.xz https://github.com/HOST-Oman/libraqm/releases/download/v0.10.5/raqm-0.10.5.tar.xz && \
    mkdir code && tar xf raqm.tar.xz -C code --strip-components 1 && \
    rm raqm.tar.xz && cd code && \
    meson setup build -Ddefault_library=static && \
    ninja -C build && mv build/src/libraqm.a ../lib/libraqm.a

# Build patched PlutoVG
RUN cd plutovg/code && \
    meson setup build -Ddefault_library=static && \
    meson compile -C build && meson install -C build && \
    mv build/libplutovg.a ../lib/libplutosvg.a

# Build PlutoSVG 0.0.8
RUN cd plutosvg && \
    wget -O plutosvg.tar.gz https://github.com/sammycage/plutosvg/archive/refs/tags/v0.0.8.tar.gz && \
    mkdir code && tar xf plutosvg.tar.gz -C code --strip-components 1 && \
    rm plutosvg.tar.gz && cd code && \
    PKG_CONFIG_PATH="/usr/local/lib64/pkgconfig" \
    meson setup build -Ddefault_library=static --wrap-mode=nofallback && \
    meson compile -C build && mv build/libplutosvg.a ../lib/libplutosvg.a

# Build libunibreak 7.0
RUN cd unibreak && \
    wget -O libunibreak.tar.xz https://github.com/adah1972/libunibreak/releases/download/libunibreak_7_0/libunibreak-7.0.tar.gz && \
    mkdir code && tar xf libunibreak.tar.xz -C code --strip-components 1 && \
    rm libunibreak.tar.xz && cd code && \
    ./configure && make && mv src/.libs/libunibreak.a ../lib/libunibreak.a

# Install shards and build devtool
WORKDIR /wirewright
RUN shards install && \
    crystal build src/dev.cr --progress -Dpreview_mt -Dexecution_context
