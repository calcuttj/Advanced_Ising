set terminal png
set xlabel 'Temp'
set ylabel 'Magnetization'
set grid
set key off
set title 'Magnetization'
set output 'MagVSTemp.png'
plot 'MagVsT.txt'
