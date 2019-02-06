$BUILD_DIR = "./dist/improviz-win"

mkdir -p $BUILD_DIR
cp -r ./assets $BUILD_DIR
cp -r ./usercode $BUILD_DIR
cp -r ./textures $BUILD_DIR
cp -r ./examples $BUILD_DIR
cp ./improviz.yaml $BUILD_DIR
./stack.exe install --local-bin-path $BUILD_DIR

7z a "improviz-win-${ENV:APPVEYOR_REPO_TAG_NAME}.zip" ./dist/improviz-win
