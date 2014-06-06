rm -r dist
cabal install
cp dist/build/DynAnalyzer/DynAnalyzer ~/bin/
cp dist/build/DynAnalyzer/DynAnalyzer ${PATH%%:*}
echo 'Installed !!!!'
cd src
