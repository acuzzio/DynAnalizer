fn=CCCC

################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "Density Map"
set output '${fn}3Dmap.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set pm3d interpolate 8,8
set palette defined (0 '#005824',  1 '#238B45',  2 '#41AE76',  3 '#66C2A4',  4 '#99D8C9',  5 '#CCECE6',  6 '#E5F5F9',  12 '#F7FCFD')
set nokey
set xrange [0:600]
set yrange [-270:90]
set zrange [0:800]
set view map
splot "${fn}Density" u 1:2:3 w pm3d, "CCCCHOP10" u 2:3:(0) w p pt 7 ps 2 t "Hop to S0", "CCCCHOP01" u 2:3:(0) w p pt 7 ps 2 t "Hop to S1", "CCCCS1AVG" u 1:2:(0) w points
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script


################# Figure 1 ####################
cat > gnuplot.script << MOROKUMA
set title "Averages"
set output 'averages.png'
set terminal pngcairo size 2048,1060 enhanced font ", 25"
set yrange [-270:90]
set view map
plot "CCCCAVERAGES" u 1 t "no Isom", "" u 2 t "Isom", "" u 3 t "no Hop"
MOROKUMA
###############################################
gnuplot < gnuplot.script
rm gnuplot.script


#TRANS
#set yrange [-270:90]
