#!/bin/bash
ps=3
ps2=5
lw=4
lw2=8
#range=[-540:180] # TRANS
yrange=[-300:100] # CIS

xrange=[0:210]

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


grep "S1 10" ${fn}Corrected > ${fn}10
grep "S0 01" ${fn}Corrected > ${fn}01
grep "S1 10" ${fn}2Corrected > ${fn}210
grep "S0 01" ${fn}2Corrected > ${fn}201
declare -A labels
#labels=([3]=cccc [4]=beta [5]=tau)
labels=([3]=cccc [4]=beta [5]=tau)
for i in ${!labels[@]}
do
value=${labels[$i]}
################## Figure 1 ####################
#cat > gnuplot.script << MOROKUMA
#set title "${fn} : $value"
#set output '${fn}Corr${value}.png'
#set terminal pngcairo size 2048,1060 enhanced font ", 25"
#set yrange $yrange
#set xrange $xrange
#set key off
#plot "${fn}Corrected" u 2:$i lw $lw linecolor rgb "black" w lines, "${fn}01" u 2:$i pt 7 ps $ps w p, "${fn}10" u 2:$i pt 7 ps $ps w p
#MOROKUMA
################################################
#gnuplot < gnuplot.script
#rm gnuplot.script

for typ in "TOT" #"S0"
do
for thr in 0.4 0.5 0.6
do
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${fn} CT $thr : $value"
set output '${fn}Corr${value}${typ}${thr}.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set yrange $yrange
set xrange $xrange
set xtics (0,50,100,150,200)
set xtics font "Times-Roman, 40"
set ytics font "Times-Roman, 40"
set key off
plot "${fn}cT${thr}LO${typ}" u 2:$i lw $lw linecolor rgb "grey" w lines, "${fn}cT${thr}HI${typ}" u 2:$i lw $lw linecolor rgb "pink" w lines, "${fn}2cT${thr}LO${typ}" u 2:$i lw $lw2 linecolor rgb "black" w lines, "${fn}2cT${thr}HI${typ}" u 2:$i lw $lw2 linecolor rgb "red" w lines, "${fn}10" u 2:$i linecolor rgb "blue" pt 7 ps $ps w p , "${fn}01" u 2:$i linecolor rgb "cyan" pt 7 ps $ps w p, "${fn}210" u 2:$i linecolor rgb "green" pt 7 ps $ps2 w p
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script
done
done
done
mv *.png ../Dropbox/photos/perFede/ciao/ 
