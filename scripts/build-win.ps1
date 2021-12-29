$BUILD_DIR = "./dist"
$BUNDLE_DIR = "$BUILD_DIR/improviz-win"
mkdir -p $BUNDLE_DIR

cp -r ./assets $BUNDLE_DIR
cp -r ./examples $BUNDLE_DIR
cp -r ./geometries $BUNDLE_DIR
cp -r ./hellocatfood $BUNDLE_DIR
cp -r ./stdlib $BUNDLE_DIR
cp -r ./textures $BUNDLE_DIR
cp -r ./usercode $BUNDLE_DIR
cp -r ./materials $BUNDLE_DIR
cp -r ./filters $BUNDLE_DIR
cp -r ./improviz.yaml $BUNDLE_DIR

$DOCUMENTATION_DIR = "$BUNDLE_DIR/documentation"
mkdir -p $DOCUMENTATION_DIR
cp ./docs/configuration.md $DOCUMENTATION_DIR/configuration.txt
cp ./docs/development.md $DOCUMENTATION_DIR/development.txt
cp ./docs/filters.md $DOCUMENTATION_DIR/filters.txt
cp ./docs/geometries.md $DOCUMENTATION_DIR/geometries.txt
cp ./docs/getting-started.md $BUNDLE_DIR/getting-started.txt
cp ./docs/how-it-works.md $DOCUMENTATION_DIR/how-it-works.txt
cp ./docs/interacting.md $DOCUMENTATION_DIR/interacting.txt
cp ./docs/language.md $DOCUMENTATION_DIR/language.txt
cp ./docs/materials.md $DOCUMENTATION_DIR/materials.txt
cp ./docs/osc.md $DOCUMENTATION_DIR/osc.txt
cp ./docs/reference.md $DOCUMENTATION_DIR/reference.txt
cp ./docs/textures.md $DOCUMENTATION_DIR/textures.txt
cp ./docs/web.md $DOCUMENTATION_DIR/web.txt

stack install --local-bin-path $BUNDLE_DIR

$ENV:VERSION = ${ENV:VERSION_TAG}

echo "${ENV:VERSION}" > "${BUNDLE_DIR}/version.txt"

7z a "improviz-win-${ENV:VERSION}.zip" $BUNDLE_DIR
