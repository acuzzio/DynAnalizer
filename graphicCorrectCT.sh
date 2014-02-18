#!/bin/bash
ps=1.1
lw=1
#range=[-540:180] # TRANS
range=[-360:360] # CIS

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
declare -A labels
labels=([3]=cccc [4]=beta [5]=tau)
for i in ${!labels[@]}
do
value=${labels[$i]}
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${fn} : $value"
set output '${fn}Corr${value}.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set yrange $range
set key off
plot "${fn}Corrected" u 2:$i lw $lw linecolor rgb "black" w lines, "${fn}01" u 2:$i pt 7 ps $ps w p, "${fn}10" u 2:$i pt 7 ps $ps w p
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script

for thr in 0.4 0.5 0.6
do
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${fn} CT $thr : $value"
set output '${fn}Corr${value}${thr}.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set yrange $range
set key off
plot "${fn}cT${thr}LOTOT" u 2:$i lw $lw linecolor rgb "black" w lines, "${fn}cT${thr}HITOT" u 2:$i lw $lw linecolor rgb "red" w lines, "${fn}10" u 2:$i pt 7 ps $ps w p , "${fn}01" u 2:$i linecolor rgb "green" pt 7 ps $ps w p
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script
done
done
