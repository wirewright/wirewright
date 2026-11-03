FROM wirewright:latest AS builder

WORKDIR /wirewright

# Build MuSoma binaries
RUN ./dev g musoma \
  && ./dev flag syslibs \
  && ./dev b

RUN strip --strip-unneeded musoma

# Create AppImage and the distribution archive.
RUN mkdir -p AppDir/usr/bin AppDir/usr/lib \
  && cp musoma AppDir/usr/bin/ \
  && cp build/AppRun.musoma AppDir/AppRun \
  && chmod +x AppDir/AppRun \
  && /linuxdeploy.AppImage --appimage-extract-and-run --appdir AppDir -d build/musoma.desktop -i img/musoma-256x256.png --output appimage

# Export
FROM scratch AS export
COPY --from=builder /wirewright/musoma-x86_64.AppImage /
