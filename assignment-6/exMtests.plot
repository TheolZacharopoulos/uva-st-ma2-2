set xtics ("e=0" 0.25, "e=1" 1.75, "e=2" 3.25, "e=3" 4.75, "e=4" 6.25, "e=5" 7.75, "e=6" 9.25, "e=7" 10.75, "e=8" 12.25)

set logscale y
set term png size 800,600
set output"exMtests.png"

set boxwidth 0.5
set style fill solid

plot 'exMtests.dat' every 2    using 1:2 with boxes ls 1 title "expM 10^6 10^e 333",\
     'exMtests.dat' every 2::1 using 1:2 with boxes ls 2 title "exM 10^6 10^e 333"