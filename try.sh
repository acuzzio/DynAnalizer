#!/bin/bash
################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set ticslevel 0
set key off 
set xtics font "Times-Roman, 20" 
set ytics font "Times-Roman, 20" 
set ztics font "Times-Roman, 20" 
set xtics (0,50,100,150,200)
set ytics (-200,-150,-100,-50,0,50)
set ztics (-0.3,-0.2,-0.1,0,0.1,0.2,0.3)
set zrange [-0.4:0.3]
set yrange [-250:100]
set xrange [0:500]
splot "CCCCcT0.6HITOT" u 2:3:7 w l linecolor rgb "black", "CCCCcT0.6LOTOT" u 2:3:7 w l linecolor rgb "red", "CCCCHOP10" u 2:3:7 w p pt 7 ps 2 linecolor rgb "blue", "CCCCHOP01" u 2:3:7 w p pt 7 ps 2 linecolor rgb "green"
MOROKUMA
###############################################
#gnuplot < gnuplot.script 

#"TOT" u 1:2:(-0.4) w l linecolor rgb "grey", 

