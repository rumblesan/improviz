$BUILD_DIR = "./dist/improviz-win"

mkdir -p $BUILD_DIR
cp -r ./assets $BUILD_DIR
cp -r ./stdlib $BUILD_DIR
cp -r ./textures $BUILD_DIR
cp -r ./examples $BUILD_DIR
cp ./improviz.yaml $BUILD_DIR
cp ./docs/getting-started.md $BUILD_DIR/getting-started.txt
./stack.exe install --local-bin-path $BUILD_DIR

7z a "improviz-win-${ENV:APPVEYOR_REPO_TAG_NAME}.zip" ./dist/improviz-win
