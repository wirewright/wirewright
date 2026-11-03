FROM crystallang/crystal:1.21-alpine AS builder

WORKDIR /
ADD --unpack=true wirewright.tar.gz /wirewright

WORKDIR wirewright/
RUN apk add gmp-static sqlite-static
RUN shards install
RUN crystal build src/dev.cr
RUN crystal build src/irack.cr --release --static \
  && ./dev g docdump && ./dev r \
  && crystal build src/doctool.cr --release --static

RUN strip --strip-unneeded irack \
  && strip --strip-unneeded doctool

# Export
FROM scratch AS export
COPY --from=builder /wirewright/irack /
COPY --from=builder /wirewright/doctool /
