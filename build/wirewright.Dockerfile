FROM wirewright-env:latest

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
