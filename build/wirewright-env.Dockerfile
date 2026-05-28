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

