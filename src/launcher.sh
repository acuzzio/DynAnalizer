cd ..
rm -r dist
cabal install
cp dist/build/DynAnalyzer/DynAnalyzer ~/bin/
echo 'Installed !!!!'
cd src
