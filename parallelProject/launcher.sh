rm -r dist
cabal-dev configure
cabal-dev build
cp dist/build/CreateInfo/CreateInfo ~/bin/
echo 'CreateInfo +RTS -N6 -s -RTS'
