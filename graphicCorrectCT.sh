#!/bin/bash
ps=1.1
echo "Wich file?"
read fn
grep "S1 10" ${fn}Corrected > ${fn}10
grep "S0 01" ${fn}Corrected > ${fn}01
declare -A labels
labels=([3]=cccc [4]=beta [5]=tau)
for i in ${!labels[@]}
do
for thr in 0.4 0.5 0.6
do
value=${labels[$i]}
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${fn} CT $thr : $value"
set output '${fn}${value}${thr}.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set yrange [-500:100]
set key off
plot "${fn}cT${thr}LOTOT" u 2:$i lw 0.5 linecolor rgb "black" w lines, "${fn}cT${thr}HITOT" u 2:$i lw 0.5 linecolor rgb "red" w lines, "${fn}10" u 2:$i pt 7 ps $ps w p , "${fn}01" u 2:$i linecolor rgb "green" pt 7 ps $ps w p
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script
done
done
