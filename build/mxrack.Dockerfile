FROM wirewright:latest AS builder

WORKDIR /wirewright

# Build MxRack binaries
RUN ./dev g mxrack \
  && ./dev flag syslibs \
  && ./dev b

RUN strip --strip-unneeded mxrack

# Create AppImage and the distribution archive for MxRack.
RUN mkdir -p AppDir/usr/bin AppDir/usr/lib \
  && cp mxrack AppDir/usr/bin/ \
  && cp build/AppRun.mxrack AppDir/AppRun \
  && chmod +x AppDir/AppRun \
  && /linuxdeploy.AppImage --appimage-extract-and-run --appdir AppDir -d build/mxrack.desktop -i img/musoma-256x256.png --output appimage

# Export
FROM scratch AS export
COPY --from=builder /wirewright/mxrack-x86_64.AppImage /
