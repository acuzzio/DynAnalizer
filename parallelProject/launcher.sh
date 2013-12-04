rm -r dist
cabal-dev configure
cabal-dev build
# ./dist/build/DynAnalizer/DynAnalizer +RTS -N6 -s -RTS
