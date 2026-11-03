#!/bin/sh

git archive -o build/wirewright.tar.gz --format=tar.gz HEAD \
&& cd build/ \
&& docker build -f lightweight.Dockerfile --output type=local,dest=. . \
&& docker build -f wirewright.Dockerfile -t wirewright:latest . \
&& docker build -f musoma.Dockerfile --output type=local,dest=. . \
&& docker build -f mxrack.Dockerfile --output type=local,dest=. . \
&& mkdir ww-dist \
&& cp -r ../examples ww-dist/ \
&& cp -r ../guides ww-dist/ \
&& cp -r ../runtime ww-dist/ \
&& cp -r dist-README ww-dist/README \
&& mv mxrack-x86_64.AppImage musoma-x86_64.AppImage irack doctool ww-dist/ \
&& tar czf ww-dist.tar.gz ww-dist/ \
&& rm -rf ww-dist \
&& mv ww-dist.tar.gz .. \
&& rm wirewright.tar.gz \
&& cd ..
