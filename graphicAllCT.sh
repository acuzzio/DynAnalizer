#!/bin/bash
echo "Which folder?"
read fn

for i in TOT S0
do
for thr in 0.4 0.5 0.6 
do
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "${thr}$i"
set output '${i}${thr}Ct.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"
plot "${fn}cT${thr}LO$i" u 2:4 w p pt 7 ps 1.5 linecolor rgb "black" t "CT lower than ${thr}", "${fn}cT${thr}HI$i" u 2:4 w p pt 7 ps 1.5 linecolor rgb "red" t "CT bigger than ${thr}"
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script
################# Figure 2 ####################
cat > gnuplot.script << MOROKUMA
set title "${thr}${i}Tra"
set output 'Tra${i}${thr}Ct.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"
set yrange [0:200]
plot "${fn}cT${thr}LO$i" u 1:2 w p pt 7 ps 1.5 linecolor rgb "black" t "CT lower than ${thr}", "${fn}cT${thr}HI$i" u 1:2 w p pt 7 ps 1.5 linecolor rgb "red" t "CT bigger than ${thr}"
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script

done
done
