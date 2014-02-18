#!/bin/bash
if [ -z $1 ]; then
echo ""
echo "You can also launch this script writing the command $ graphicCorrectCT.sh fileCCCC "
echo "but you didn't... so..."
PS3="type a number or 'q' to quit: "
fileList=$(find *CCCC -maxdepth 1 -type f)

if [ -z $fileList ]; then
echo ""
echo "Please launch this command in a directory containing files folderCCCC !!"
echo ""
exit
fi

select fileName in $fileList; do
    if [ -n "$fileName" ]; then
        RE=${fileName}
    fi    
if [ -z $RE ];then
echo "nothing to do..."
   exit
fi
break
done
else
RE=$1
fi

fn=$(basename $RE CCCC)

# THIS IS THE GREEN DENSITY MAP
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "Density Map"
set output '${fn}3Dmap.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set pm3d interpolate 8,8
set palette defined (0 '#005824',  1 '#238B45',  2 '#41AE76',  3 '#66C2A4',  4 '#99D8C9',  5 '#CCECE6',  6 '#E5F5F9',  12 '#F7FCFD')
set nokey
set xrange [0:200]
set yrange [-270:90]
set zrange [0:800]
set view map
splot "${fn}Density" u 1:2:3 w pm3d, "${fn}HOP10" u 2:3:(0) w p pt 7 ps 2 t "Hop to S0", "${fn}HOP01" u 2:3:(0) w p pt 7 ps 2 t "Hop to S1", "${fn}S1AVG" u 1:2:(0) w points
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script

################## Figure 1 ####################
#cat > gnuplot.script << MOROKUMA
#set title "Averages"
#set output 'averages.png'
#set terminal pngcairo size 2048,1060 enhanced font ", 25"
#set yrange [-270:90]
#set view map
#plot "${fn}AVERAGES" u 1 t "no Isom", "" u 2 t "Isom", "" u 3 t "no Hop"
#MOROKUMA
################################################
#gnuplot < gnuplot.script
#rm gnuplot.script

for i in TOT S0
do
for thr in 0.4 0.5 0.6 
do

# these are the pointed red black graphics

################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${thr}$i"
set output '${fn}${i}${thr}Ct.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"
plot "${fn}cT${thr}LO$i" u 2:4 w p pt 7 ps 1.5 linecolor rgb "black" t "CT lower than ${thr}", "${fn}cT${thr}HI$i" u 2:4 w p pt 7 ps 1.5 linecolor rgb "red" t "CT bigger than ${thr}"
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script
################# Figure 2 ####################
cat > gnuplot.script << MOROKUMA
set title "${thr}${i}Tra"
set output '${fn}${i}${thr}TraCt.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"
plot "${fn}cT${thr}LO$i" u 1:2 w p pt 7 ps 1.5 linecolor rgb "black" t "CT lower than ${thr}", "${fn}cT${thr}HI$i" u 1:2 w p pt 7 ps 1.5 linecolor rgb "red" t "CT bigger than ${thr}"
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script

done
done
