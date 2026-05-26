FROM wirewright-base:latest AS builder

WORKDIR /wirewright

# Build MuSoma binaries
RUN ./dev g musoma && ./dev b

# Scaffold AppDir layout and copy binaries/assets
RUN mkdir -p AppDir/usr/bin AppDir/usr/lib AppDir/usr/share/musoma && \
    cp musoma AppDir/usr/bin/ && \
    cp -r runtime AppDir/usr/share/musoma && \
    cp img/musoma-256x256.png AppDir/musoma-256x256.png

# Create Desktop Entry
RUN echo "[Desktop Entry]\n\
Name=musoma\n\
Exec=musoma\n\
Icon=musoma-256x256\n\
Type=Application\n\
Categories=Development" > AppDir/musoma.desktop

# Create AppRun
RUN echo '#!/bin/sh\n\
export WW_RUNTIME="${APPDIR}/usr/share/musoma/runtime"\n\
export LD_LIBRARY_PATH="${APPDIR}/usr/lib:${LD_LIBRARY_PATH}"\n\
exec "${APPDIR}/usr/bin/musoma" "$@"' > AppDir/AppRun && \
    chmod +x AppDir/AppRun

# Execute linuxdeploy and create distribution archive
RUN ldconfig && \
    /linuxdeploy.AppImage --appimage-extract-and-run --appdir AppDir --output appimage && \
    mkdir musoma-dist && \
    mv musoma-x86_64.AppImage musoma-dist/musoma-x86_64.AppImage && \
    cp -r examples musoma-dist/ && \
    tar czf musoma-dist.tar.gz musoma-dist

# Export
FROM scratch AS export
COPY --from=builder /wirewright/musoma-dist.tar.gz /
