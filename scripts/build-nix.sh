#! /bin/bash

BUILD_DIR="dist/improviz-nix"
DOCUMENTATION_DIR="$BUILD_DIR/documentation"
mkdir -p $BUILD_DIR
cp -r ./assets $BUILD_DIR
cp -r ./stdlib $BUILD_DIR
cp -r ./textures $BUILD_DIR
cp -r ./examples $BUILD_DIR
cp ./improviz.yaml $BUILD_DIR
mkdir -p $DOCUMENTATION_DIR
cp ./docs/getting-started.md $BUILD_DIR/getting-started.txt
cp ./docs/language.md $DOCUMENTATION_DIR/language.txt
cp ./docs/interacting.md $DOCUMENTATION_DIR/interacting.txt
cp ./docs/reference.md $DOCUMENTATION_DIR/reference.txt
cp ./docs/textures.md $DOCUMENTATION_DIR/textures.txt
cp ./docs/configuration.md $DOCUMENTATION_DIR/configuration.txt
cp $(stack exec -- which improviz) $BUILD_DIR

tar -C ./dist -zcvf improviz-nix-${TRAVIS_TAG}.tar.gz improviz-nix
