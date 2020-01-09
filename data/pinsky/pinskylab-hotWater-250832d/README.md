# hotWater
Comparison of climate impacts on land and in the ocean.

These scripts are provided in the interests of open science. If you have questions or find errors, please let us know.

Contact:<br/>
Malin Pinsky<br/>
Rutgers University<br/>
[malin.pinsky@rutgers.edu](malin.pinsky@rutgers.edu)

## Directory structure
- [data](data/): has the raw data
- data_dl: has data that is easily downloaded (not tracked by git)
- [output](output/): files output by scripts (tracked by git)
- temp: files output by scripts (not tracked by git)
- [scripts](scripts/): run using data, data_dl, and output, produce temp, output, figures, and tables
- [figures](figures/): plotted figures
- [tables](tables/): output tables

## Analysis workflow (roughly... see below for more details)
- [ghcn_download.r](scripts/ghcn_download.r) to get GHCND data
- [nichemapRscript_nonequil.R](scripts/nichemapRscript_nonequil.R) to calculate Tb for terrestrial species
- [thermal_safety_vs_lat_bylatlon_calculations.r](scripts/thermal_safety_vs_lat_bylatlon_calculations.r) to calculate TSM
- [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r) to fit GAMMs for Tmax, Tb, and TSM
- [future_tsm_byyear.r](scripts/future_tsm_byyear.r) to calculate future TSMs
- [local_extirpations.R](scripts/local_extirpations.R) to analyze extirpation data and output some tables
- [figures_for_paper.r](scripts/figures_for_paper.r) to make figures and some tables

## Scripts and files behind figures, tables, and other calculations
All figures and Table S1 are produced by figures_for_paper.r.<br/>

1. [Table 1](tables/Table1.csv)
	1. [local_extirpations.R](scripts/local_extirpations.R)
		1. [data/contraction/range_contraction.csv](data/contraction/range_contraction.csv)
		2. temp/range_contraction_taxonomy.csv
			1. [local_extirpation_make_taxonomy.r](scripts/local_extirpation_make_taxonomy.r)
				1. [data/contraction/range_contraction.csv](data/contraction/range_contraction.csv)
				2. [data/contraction/taxonomy_by_hand.csv](data/contraction/taxonomy_by_hand.csv)
2. [Figure 1a](figures/fig1.pdf) needs
	1. temp/hadex2txxlat.csv and temp/sstmaxhrlat.csv, produced by
		1. [Thab_vs_lat.r](scripts/Thab_vs_lat.r), which reads in
			1. temp/tosmax.rdata, produced by 
				1. [climatology_OISST.r](scripts/climatology_OISST.r), which reads in
					1. temp/tosmaxday.rdata, produced by
						1. [climatology_OISST_maxday.r](scripts/climatology_OISST_maxday.r) followed by [climatology_OISST_maxday_process.r](scripts/climatology_OISST_maxday_process.r), which read
							1. oisst_daily data 1986-2005 downloaded by [get_oisst_daily.sh](scripts/get_oisst_daily.sh) on 9/22/2017
					2. temp/dtrclimatology_interp0.25.rdata, produced by
						1. [haddtr_interp.r](scripts/haddtr_interp.r), which reads
							1. temp/oisst1986_1990.rdata, which is produced by
								1. [climatology_OISST_readncdf.R](scripts/climatology_OISST_readncdf.R), which reads
									1. data_dl/oisst/tos_OISST_L4_AVHRR-only-v2_198601-199012.nc
									2. data_dl/oisst/tos_OISST_L4_AVHRR-only-v2_199101-199512.nc
									3. data_dl/oisst/tos_OISST_L4_AVHRR-only-v2_199601-200012.nc
									4. data_dl/oisst/tos_OISST_L4_AVHRR-only-v2_200101-200512.nc
										1. OISST files downloaded from [North Carolina Inst. for Climate Studies](http://monitor.cicsnc.org/obs4MIPs/data/OISST/Monthly/) on 5/18/2017
							1. temp/dtrclimatology0.25.rdata, produced by
								1. [haddtr_read.r](scripts/haddtr_read.r), which reads
									1. data_dl/hatdtr/monthly_climatology.txt
										1. HadDTR data downloaded from [Met Office](http://www.metoffice.gov.uk/hadobs/haddtr/) on 7/26/2017
			2. temp/hadex2_txx.rdata
				1. [hadex2_read.r](scripts/hadex2_read.r), which reads
					1. data_dl/hadex2/H2_TXx_1901-2010_RegularGrid_global_3.75x2.5deg_LSmask.nc
						1. HadEX2 data downloaded from [Met Office](http://www.metoffice.gov.uk/hadobs/hadex2/download.html) on 7/27/2017
3. [Figure 1b](figures/fig2.pdf) needs
	1. temp/warmingmaxhr_bylat_land.csv and temp/warmingmaxhr_bylat_ocean.csv, produced by
		1. [future_temperatures_maxhr.r](scripts/future_temperatures_maxhr.r), which reads
			1. temp/tosmax.rdata, produced by
				1. [climatology_OISST.r](scripts/climatology_OISST.r)
					1. temp/tosmaxday.rdata (see Fig. 1a)
					2. temp/dtrclimatology_interp0.25.rdata (see Fig. 1a)
			2. temp/hadex2_txx.rdata (see Fig. 1a)
			3. temp/dtrclimatology_interp1.25.rdata, produced by
				1. [haddtr_interp.r](scripts/haddtr_interp.r), which reads
					1. temp/dtrclimatology1.25.rdata, produced by
						1. [haddtr_read.r](scripts/haddtr_read.r) (see Fig. 1a)
			4. data_dl/cmip5/txx_yr_modmean_rcp26_ave.nc
				1. Downloaded CMIP5 RCP 2.6 TXx (maximum daily maximum near-surface temperature) ensemble mean from [KNMI Climate Explorer](http://climexp.knmi.nl) as modmean22 rcp26 txx on 4/6/2018
				2. See [Donat, M. G., et al. (2013), Updated analyses of temperature and precipitation extreme indices since the beginning of the twentieth century: The HadEX2 dataset, J. Geophys. Res. Atmos., 118, 2098–2118](https://doi.org/10.1002/jgrd.50150)
			5. data_dl/cmip5/txx_yr_modmean_rcp85_ave.nc
				1. Downloaded CMIP5 RCP 8.5 TXx (maximum daily maximum near-surface temperature) ensemble mean from [KNMI Climate Explorer](http://climexp.knmi.nl) as modmean22 rcp85 txx on 4/6/2018
			6. data_dl/cmip5/tos_Omon_modmean_rcp26_ave.nc
				1. Downloaded CMIP5 RCP 2.6 tos (temperature ocean surface) ensemble mean from [KNMI Climate Explorer](http://climexp.knmi.nl) as modmean rcp26 tos on 7/25/2017
			7. data_dl/cmip5/tos_Omon_modmean_rcp85_ave.nc
				1. Downloaded CMIP5 RCP 8.5 tos (temperature ocean surface) ensemble mean from [KNMI Climate Explorer](http://climexp.knmi.nl) as modmean rcp85 tos on 7/25/2017
4. [Fig. 2a and 2b](figures/fig2.pdf)
	1. temp/warmingtolerance_bylatlon.csv
		1. [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r)
			1. temp/warmingtolerance_byspecies.csv
				1. [thermal_safety_vs_lat_bylatlon_calculations.r](scripts/thermal_safety_vs_lat_bylatlon_calculations.r)
					1. output/dataset_1_hotwater_Nichemapped_GHCNDnonequil.csv
						1. [nichemapRscript_nonequil.R](scripts/nichemapRscript_nonequil.R)
							1. [data/tmax_data/dataset_1_hotwater.csv](data/tmax_data/dataset_1_hotwater.csv)
							2. [data/tmax_data/dataset_1_traits.csv](data/tmax_data/dataset_1_traits.csv)
								1. Compiled in part with [compile_traits.r](scripts/compile_traits.r)
							3. [micro_ghcnd_fromfiles.R](scripts/micro_ghcnd_fromfiles.R)
								1. data_dl/ghcnd/ghcnd_latLAT_lonLON.rds
									1. [ghcn_download.r](scripts/ghcn_download.r)
										1. [ghcn_get.r](scripts/ghcn_get.r)
										2. temp/ghcnd_stations.rdata
											1. Downloaded with ghcnd_station() function in [rnoaa package v.0.7.0](https://CRAN.R-project.org/package=rnoaa)
										3. [data/tmax_data/dataset_1_hotwater.csv](data/tmax_data/dataset_1_hotwater.csv)
								2. Data from [NicheMapR package v1.1.3 April 2018](https://github.com/mrke/NicheMapR/releases/tag/v1.1.3)
					2. [data/tmax_data/dataset_1_traits.csv](data/tmax_data/dataset_1_traits.csv)
					3. [data/tmax_data/dataset_1_ARRs.tsv](data/tmax_data/dataset_1_ARRs.tsv)
					4. temp/elev.rdata
						1. [gmted2010_read.r](scripts/gmted2010_read.r)
							1. data_dl/gmted2010/GMTED2010_15n060_0250deg.nc
								1. Downloaded GMTED2010 0.25x0.25° elevation grid from [Tropospheric Emission Monitoring
Internet Service](http://www.temis.nl/data/gmted2010/) on 9/21/2017
								2. See [Danielson, J.J., and Gesch, D.B., 2011, Global multi-resolution terrain elevation data 2010 (GMTED2010): U.S. Geological Survey Open-File Report 2011-1073, 26 p.](http://pubs.usgs.gov/of/2011/1073/pdf/of2011-1073.pdf)
					5. temp/lstclimatology.rdata, temp/lstclimatology_warm3.rdata, temp/lstclimatology_warmestmonth.rdata
						1. [climatology_UDel.r](scripts/climatology_UDel.r)
							1. data_dl/udel/air_temp_2014/air_temp.YEAR (where year is 1986-2005)
								1. Downloaded [U. Delaware Terrestrial Air Temperature: 1900-2014 Gridded Monthly Time Series V 4.01](http://climate.geog.udel.edu/~climate/html_pages/download.html#T2014)
								2. [Willmott, C. J. and K. Matsuura (2001) Terrestrial Air Temperature and Precipitation: Monthly and Annual Time Series (1950 - 1999)](http://climate.geog.udel.edu/~climate/html_pages/README.ghcn_ts2.html)
					6. temp/sstclimatology.rdata, temp/sstclimatology_warm3.rdata, temp/sstclimatology_warmestmonth.rdata, temp/tos95max.rdata, temp/tosmax.rdata
						1. [climatology_OISST.r](scripts/climatology_OISST.r)
							1. temp/oisst_YEAR_YEAR.rdata
								1. [climatology_OISST_readncdf.R](scripts/climatology_OISST_readncdf.R) (see Fig. 1a)
							2. temp/tos95day.rdata
								1. [climatology_OISST_95day_doall.r](scripts/climatology_OISST_95day_doall.r), followed by [climatology_OISST_95day_process.r](scripts/climatology_OISST_95day_process.r), which read
									1. oisst_daily netCDF data (see Fig. 1a)
							3. temp/tosmaxday.rdata (see Fig. 1a)
	2. temp/warmingtolerance_byspecies.csv (see Fig. 2a)
5. [Fig. 2c](figures/fig2.pdf)
	1. temp/warmingtolerance_bylatlon.csv (see Fig. 2)
	2. temp/warmingtolerance_bylat_to2100.csv
		1. [future_tsm_byyear.r](scripts/future_tsm_byyear.r)
			1. temp/tasmax26dyr.rdata, temp/tasmax85dyr.rdata, temp/tosmax26dyr.rdata, temp/tosmax85dyr.rdata
				1. [future_temperatures_maxhr.r](scripts/future_temperatures_maxhr.r) (see Fig. 1b)
			2. temp/warmingtolerance_byspecies.csv (see Fig. 2a)
			3. [data/tmax_data/dataset_1_ARRs.tsv](data/tmax_data/dataset_1_ARRs.tsv)
6. [Extended Data Fig. 1](figures/figS1.png)
	1. temp/warmingtolerance_byspecies.csv (see Fig. 2)
7. [Extended Data Fig. 2](figures/figS2.png)
	1. temp/warmingtolerance_bylatlon.csv (see Fig. 2)
	2. temp/peaks_modTbGHCND95_terr.rds, temp/peaks_modTmax_terr.rds, and temp/peaks_modGHCND95_terr.rds
		1. [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r)
			1. temp/warmingtolerance_byspecies.csv (see Fig. 2a)
	3. temp/warmingtolerance_bylatband_land.csv
		1. [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r)
			1. temp/warmingtolerance_byspecies.csv (see Fig. 2a)
 8. [Extended Data Fig. 3](figures/figS3.png)
	1. temp/warmingtolerance_bylat_to2100.csv (see Fig. 2c)
	2. temp/warmingtolerance_bylatlon.csv (see Fig. 2)
	3. temp/lstmaxhrlat_2081-2100.csv and temp/sstmaxhrlat_2081-2100.csv
		1. [future_temperatures_maxhr.r](scripts/future_temperatures_maxhr.r) (see Fig. 1b)
 9. [Extended Data Fig. 4](figures/figS4.png)
	1.temp/warmingtolerance_bylatlon.csv and temp/warmingtolerance_byspecies.csv (see Fig. 2a)
10. [Extended Data Fig. 5](figures/figS5.png)
	1. [data/topt_tmax/Chen_2015_fbv009supp.csv](data/topt_tmax/Chen_2015_fbv009supp.csv)
	2. [data/topt_tmax/Deutsch_thermal_curves.csv](data/topt_tmax/Deutsch_thermal_curves.csv)
	3. [data/topt_tmax/Huey_2009_supp_data.csv](data/topt_tmax/Huey_2009_supp_data.csv)
	4. temp/topt_tmax_fit.csv
		1. [tmax_topt_analysis.R](scripts/tmax_topt_analysis.R)
			1. [data/topt_tmax/Chen_2015_fbv009supp.csv](data/topt_tmax/Chen_2015_fbv009supp.csv)
			2. [data/topt_tmax/Deutsch_thermal_curves.csv](data/topt_tmax/Deutsch_thermal_curves.csv)
			3. [data/topt_tmax/Huey_2009_supp_data.csv](data/topt_tmax/Huey_2009_supp_data.csv)
11. [Extended Data Table 1](tables/TableS1.csv)
 	1. [figures_for_paper.r](scripts/figures_for_paper.r)
 		1. temp/warmingtolerance_byspecies.csv (see Fig. 2a)
12. [Extended Data Table 2](tables/TableS2_gamm_models.csv)
	1. [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r) (see Fig. 2a)
13. Extended Data Table 3
	1. defined in [scripts/thermal_safety_ARR_sensitivity_analysis.r](scripts/thermal_safety_ARR_sensitivity_analysis.r)
14. [Extended Data Table 4](tables/TableS4.csv)
	1. [local_extirpations.R](scripts/local_extirpations.R) (see Table 1)
15. Thermal safety margins (TSMs) with species-specific acclimation response ratios (ARRs)
	1. [thermal_safety_vs_lat_bylatlon_calculations.r](scripts/thermal_safety_vs_lat_bylatlon_calculations.r) (see Fig. 2a)
16. Tmax, Tb, and Thermal Safety Margin (TSM) peak locations
	1. [tables/peak_locations.csv](tables/peak_locations.csv)
		1. [thermal_safety_vs_lat_bylatlon_models.r](scripts/thermal_safety_vs_lat_bylatlon_models.r) (see Fig. 2a)

