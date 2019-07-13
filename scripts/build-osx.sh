#! /bin/bash

BUILD_DIR="./dist"
BUNDLE_DIR="$BUILD_DIR/improviz-osx"
mkdir -p $BUILD_DIR

git clone --depth=1 --branch=master https://github.com/rumblesan/improviz-performance.git $BUNDLE_DIR
rm -rf $BUNDLE_DIR/.git
rm -r $BUNDLE_DIR/README.md

cp -r ./assets $BUNDLE_DIR
cp -r ./examples $BUNDLE_DIR

DOCUMENTATION_DIR="$BUNDLE_DIR/documentation"
mkdir -p $DOCUMENTATION_DIR
cp ./docs/getting-started.md $BUNDLE_DIR/getting-started.txt
cp ./docs/language.md $DOCUMENTATION_DIR/language.txt
cp ./docs/interacting.md $DOCUMENTATION_DIR/interacting.txt
cp ./docs/reference.md $DOCUMENTATION_DIR/reference.txt
cp ./docs/textures.md $DOCUMENTATION_DIR/textures.txt
cp ./docs/configuration.md $DOCUMENTATION_DIR/configuration.txt

cp $(stack exec -- which improviz) $BUNDLE_DIR

tar -C $BUILD_DIR -zcvf improviz-osx-${TRAVIS_TAG}.tar.gz improviz-osx
