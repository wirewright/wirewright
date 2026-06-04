FROM wirewright:latest AS builder

WORKDIR /wirewright

# Build MuSoma binaries
RUN ./dev g musoma \
  && ./dev flag syslibs \
  && ./dev b

# Create AppImage and the distribution archive.
RUN mkdir -p AppDir/usr/bin AppDir/usr/lib \
  && cp musoma AppDir/usr/bin/ \
  && cp build/AppRun AppDir/AppRun \
  && chmod +x AppDir/AppRun \
  && /linuxdeploy.AppImage --appimage-extract-and-run --appdir AppDir -d build/musoma.desktop -i img/musoma-256x256.png --output appimage \
  && mkdir musoma-dist \
  && mv musoma-x86_64.AppImage musoma-dist/ \
  && cp -r examples musoma-dist/ \
  && cp -r runtime musoma-dist/ \
  && tar czf musoma-dist.tar.gz musoma-dist/

# Export
FROM scratch AS export
COPY --from=builder /wirewright/musoma-dist.tar.gz /
