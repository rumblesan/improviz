#! /bin/bash

BUILD_DIR="dist/improviz-nix"
mkdir -p $BUILD_DIR
cp -r ./textures $BUILD_DIR
cp -r ./examples $BUILD_DIR
cp ./improviz.yaml $BUILD_DIR
cp $(stack exec -- which improviz) $BUILD_DIR

tar -C ./dist -zcvf improviz-nix-${TRAVIS_TAG}.tar.gz improviz-nix
