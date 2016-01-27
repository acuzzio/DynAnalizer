rm -r dist
cabal install
cp dist/build/DynAnalyzer/DynAnalyzer ~/bin/LOLLA
echo 'Installed !!!!'
cd src
