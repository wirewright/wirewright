#!/bin/sh
git archive -o wirewright.tar.gz --format=tar.gz HEAD
mv wirewright.tar.gz build/
cd build/
docker build -f wirewright.Dockerfile -t wirewright:latest .
docker build -f musoma.Dockerfile --output type=local,dest=. .
mv musoma-dist.tar.gz ..
rm wirewright.tar.gz
cd ..
