rm -r dist
cabal-dev configure
cabal-dev build
# ./dist/build/DynAnalizer/CreateInfo +RTS -N6 -s -RTS
