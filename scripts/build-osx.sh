#! /bin/bash

BUILD_DIR="$(pwd)/dist"
BUNDLE_DIR="$BUILD_DIR/improviz-osx"
mkdir -p $BUILD_DIR

git clone --depth=1 --branch=master https://github.com/rumblesan/improviz-performance.git $BUNDLE_DIR
rm -rf $BUNDLE_DIR/.git
rm -r $BUNDLE_DIR/README.md

#APP_DIR="$BUNDLE_DIR/improviz.app"
#mkdir -p "$APP_DIR/Contents/MacOS"
#mkdir -p "$APP_DIR/Contents/Resources"
VERSION="${TRAVIS_TAG:-0.0.0}"

#iconutil -c icns -o "$APP_DIR/Contents/Resources/improviz.icns" ./builds/osx/improviz.iconset

#defaults write "$APP_DIR/Contents/Info" CFBundleName Improviz
#defaults write "$APP_DIR/Contents/Info" CFBundleDisplayName Improviz
#defaults write "$APP_DIR/Contents/Info" CFBundleIdentifier com.rumblesan.improviz
#defaults write "$APP_DIR/Contents/Info" CFBundleVersion "$VERSION"
#defaults write "$APP_DIR/Contents/Info" CFBundlePackageType APPL
#defaults write "$APP_DIR/Contents/Info" CFBundleSignature impz
#defaults write "$APP_DIR/Contents/Info" CFBundleExecutable improviz
#defaults write "$APP_DIR/Contents/Info" CFBundleIconFile improviz.icns
#cp -r ./assets $BUNDLE_DIR
#cp -r ./examples $BUNDLE_DIR

DOCUMENTATION_DIR="$BUNDLE_DIR/documentation"
mkdir -p $DOCUMENTATION_DIR
cp ./docs/getting-started.md $BUNDLE_DIR/getting-started.txt
cp ./docs/language.md $DOCUMENTATION_DIR/language.txt
cp ./docs/interacting.md $DOCUMENTATION_DIR/interacting.txt
cp ./docs/reference.md $DOCUMENTATION_DIR/reference.txt
cp ./docs/textures.md $DOCUMENTATION_DIR/textures.txt
cp ./docs/configuration.md $DOCUMENTATION_DIR/configuration.txt

cp $(stack exec -- which improviz) "$BUNDLE_DIR"

tar -C $BUILD_DIR -zcvf improviz-osx-${VERSION}.tar.gz improviz-osx
