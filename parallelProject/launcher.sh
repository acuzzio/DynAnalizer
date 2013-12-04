rm -r dist
cabal-dev configure
cabal-dev build
/home/alessio/Desktop/DynAnalizer/dist/build/DynAnalizer/DynAnalizer +RTS -N4 -s -RTS
