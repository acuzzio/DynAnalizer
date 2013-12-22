#!/bin/bash
for thr in 0.4 0.5 0.6 
do
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "$fn"
set output '${fn}side.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"
plot "CCCCcT0.4HI" u 2:4 w p pt 7 ps 0.5 linecolor rgb "black", "CCCCcT0.4LO" u 2:4 w p pt 7 ps 0.5 linecolor rgb "red"
MOROKUMA
###############################################

gnuplot < gnuplot.script
rm gnuplot.script

################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "$fn"
set output '${fn}Up.png'
set terminal pngcairo size 2048,1060 enhanced font ", 12"

MOROKUMA
###############################################

gnuplot < gnuplot.script
rm gnuplot.script
done
