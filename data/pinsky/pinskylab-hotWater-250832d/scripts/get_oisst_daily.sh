#!/bin/bash

# expects a sub-directory oisst_daily in which to save all files

for yr in `seq 1986 2005`; 
do
	for mo in 01 02 03 04 05 06 07 08 09 10 11 12; 
	do		
		wget -r -nd --no-parent -P oisst_daily -A 'avhrr-only-v2.*.nc' https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/access/avhrr-only/$yr$mo/
	done
done  
