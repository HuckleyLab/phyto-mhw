# Modified from micro_global() in NicheMapR package v1.1.3 April 2018
# Now gets TMIN and TMAX daily anomalies from nearest GHCND station
# Must set timeinterval=365 (daily). Use nyears=20 for mindate='2018-01-01' and maxdate='2018-12-31'
#
# loc: c(lon, lat)
#
# Needs additional arguments:
# ghcnd_stations: output from ghcnd_stations()
# radius: maximum search radius in km for a GHCND station
# mincov: minimum coverage proportion
# mindate: YYYY-MM-DD format string, earliest date. Must be Jan 1
# maxdate: YYYY-MM-DD format string, latest date. Must be Dec 31
#
# Needs this option set to get GHCN data:
# options(noaakey = "KEY_EMAILED_TO_YOU")
#
# Returns nothing if hits an error
#

# initialize variables to a default (useful for debugging)
# load('temp/ghcnd_stations.rdata')
# loc = c(14.19, 55.40); timeinterval = 365; nyears = 20; soiltype = 4; REFL = 0.15; slope = 0; aspect = 0; DEP = c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200); minshade = 0; maxshade = 90; Refhyt = 2; Usrhyt = 0.01; Z01 = 0; Z02 = 0; ZH1 = 0; ZH2 = 0; runshade = 1; clearsky = 0; rungads = 1; write_input = 0; writecsv = 0; ERR = 2; RUF = 0.004; EC = 0.0167238; SLE = 0.95; Thcond = 2.5; Density = 2560; SpecHeat = 870; BulkDensity = 1300; PCTWET = 0; cap = 1; CMH2O = 1; hori = rep(0, 24); TIMAXS = c(1, 1, 0, 0); TIMINS = c(0, 0, 1, 1); timezone = 0; runmoist = 0; PE = rep(1.1, 19); KS = rep(0.0037, 19); BB = rep(4.5, 19); BD = rep(1.3, 19); Clay = 20; maxpool = 10000; rainmult = 1; evenrain = 0; SoilMoist_Init = c(0.1, 0.12, 0.15, 0.2, 0.25, 0.3, 0.3, 0.3, 0.3, 0.3); L = c(0, 0, 8.18990859, 7.991299442, 7.796891252, 7.420411664, 7.059944542, 6.385001059, 5.768074989, 4.816673431, 4.0121088, 1.833554792, 0.946862989, 0.635260544, 0.804575, 0.43525621, 0.366052856, 0, 0) * 10000; R1 = 0.001; RW = 2.5e+10; RL = 2e+06; PC = -1500; SP = 10; IM = 1e-06; MAXCOUNT = 500; LAI = 0.1; snowmodel = 0; snowtemp = 1.5; snowdens = 0.375; densfun = c(0, 0); snowmelt = 0.9; undercatch = 1; rainmelt = 0.0125; rainfrac = 0.5; shore = 0; tides = matrix(data = 0, nrow = 24 * timeinterval * nyears, ncol = 3); lamb = 0; IUV = 0; radius=1000; mincov=0.8; mindate='1986-01-01'; maxdate='2005-12-31'

micro_ghcnd <- function (loc = c(14.19, 55.40), timeinterval = 365, 
    nyears = 20, soiltype = 4, REFL = 0.15, slope = 0, aspect = 0, 
    DEP = c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200), minshade = 0, 
    maxshade = 90, Refhyt = 1.2, Usrhyt = 0.01, Z01 = 0, Z02 = 0, 
    ZH1 = 0, ZH2 = 0, runshade = 1, clearsky = 0, rungads = 1, 
    write_input = 0, writecsv = 0, ERR = 2, RUF = 0.004, EC = 0.0167238, 
    SLE = 0.95, Thcond = 2.5, Density = 2560, SpecHeat = 870, 
    BulkDensity = 1300, PCTWET = 0, cap = 1, CMH2O = 1, hori = rep(0, 
        24), TIMAXS = c(1, 1, 0, 0), TIMINS = c(0, 0, 1, 1), 
    timezone = 0, runmoist = 0, PE = rep(1.1, 19), KS = rep(0.0037, 
        19), BB = rep(4.5, 19), BD = rep(1.3, 19), Clay = 20, 
    maxpool = 10000, rainmult = 1, evenrain = 0, SoilMoist_Init = c(0.1, 
        0.12, 0.15, 0.2, 0.25, 0.3, 0.3, 0.3, 0.3, 0.3), L = c(0, 
        0, 8.18990859, 7.991299442, 7.796891252, 7.420411664, 
        7.059944542, 6.385001059, 5.768074989, 4.816673431, 4.0121088, 
        1.833554792, 0.946862989, 0.635260544, 0.804575, 0.43525621, 
        0.366052856, 0, 0) * 10000, R1 = 0.001, RW = 2.5e+10, 
    RL = 2e+06, PC = -1500, SP = 10, IM = 1e-06, MAXCOUNT = 500, 
    LAI = 0.1, snowmodel = 0, snowtemp = 1.5, snowdens = 0.375, 
    densfun = c(0, 0), snowmelt = 0.9, undercatch = 1, rainmelt = 0.0125, 
    rainfrac = 0.5, shore = 0, tides = matrix(data = 0, nrow = 24 * 
        timeinterval * nyears, ncol = 3), lamb = 0, IUV = 0, ghcnd_stations, radius, mincov, mindate, maxdate) 
{
    SoilMoist = SoilMoist_Init
    errors <- 0
    if (DEP[2] - DEP[1] > 3 | DEP[3] - DEP[2] > 3) {
        message("warning, nodes might be too far apart near the surface, try a different spacing if the program is crashing \n")
    }
    if (timeinterval < 12 | timeinterval > 365) {
        message("ERROR: the variable 'timeinterval' is out of bounds.\n        Please enter a correct value (12 - 365).", 
            "\n")
        errors <- 1
    }
    if (is.numeric(loc[1])) {
        if (loc[1] > 180 | loc[2] > 90) {
            message("ERROR: Latitude or longitude (longlat) is out of bounds.\n        Please enter a correct value.", 
                "\n")
            errors <- 1
        }
    }
    if (timezone %in% c(0, 1) == FALSE) {
        message("ERROR: the variable 'timezone' be either 0 or 1.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    if (rungads %in% c(0, 1) == FALSE) {
        message("ERROR: the variable 'rungads' be either 0 or 1.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    if (Clay < 0) {
        message("ERROR: Clay density value (Clay) is negative.\n      Please input a positive value.", 
            "\n")
        errors <- 1
    }
    if (write_input %in% c(0, 1) == FALSE) {
        message("ERROR: the variable 'write_input' be either 0 or 1.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    if (EC < 0.0034 | EC > 0.058) {
        message("ERROR: the eccentricity variable (EC) is out of bounds.\n        Please enter a correct value (0.0034 - 0.058).", 
            "\n")
        errors <- 1
    }
    if (RUF < 1e-04) {
        message("ERROR: The roughness height (RUF) is too small ( < 0.0001).\n        Please enter a larger value.", 
            "\n")
        errors <- 1
    }
    if (RUF > 2) {
        message("ERROR: The roughness height (RUF) is too large ( > 2).\n        Please enter a smaller value.", 
            "\n")
        errors <- 1
    }
    if (DEP[1] != 0) {
        message("ERROR: First soil node (DEP[1]) must = 0 cm.\n        Please correct", 
            "\n")
        errors <- 1
    }
    if (length(DEP) != 10) {
        message("ERROR: You must enter 10 different soil depths.", 
            "\n")
        errors <- 1
    }
    for (i in 1:9) {
        if (DEP[i + 1] <= DEP[i]) {
            message("ERROR: Soil depth (DEP array) is not in ascending size", 
                "\n")
            errors <- 1
        }
    }
    if (DEP[10] > 500) {
        message("ERROR: Deepest soil depth (DEP array) is too large (<=500 cm)", 
            "\n")
        errors <- 1
    }
    if (Thcond < 0) {
        message("ERROR: Thermal variable conductivity (THCOND) is negative.\n        Please input a positive value.", 
            "\n")
        errors <- 1
    }
    if (Density < 0) {
        message("ERROR: Density variable (Density) is negative.\n        Please input a positive value.", 
            "\n")
        errors <- 1
    }
    if (SpecHeat < 0) {
        message("ERROR: Specific heat variable (SpecHeat) is negative.\n        Please input a positive value.", 
            "\n")
        errors <- 1
    }
    if (min(BulkDensity) < 0) {
        message("ERROR: Bulk density value (BulkDensity) is negative.\n        Please input a positive value.", 
            "\n")
        errors <- 1
    }
    if (REFL < 0 | REFL > 1) {
        message("ERROR: Soil reflectivity value (REFL) is out of bounds.\n        Please input a value between 0 and 1.", 
            "\n")
        errors <- 1
    }
    if (slope < 0 | slope > 90) {
        message("ERROR: Slope value (slope) is out of bounds.\n        Please input a value between 0 and 90.", 
            "\n")
        errors <- 1
    }
    if (aspect < 0 | aspect > 365) {
        message("ERROR: Aspect value (aspect) is out of bounds.\n        Please input a value between 0 and 365.", 
            "\n")
        errors <- 1
    }
    if (max(hori) > 90 | min(hori) < 0) {
        message("ERROR: At least one of your horizon angles (hori) is out of bounds.\n        Please input a value between 0 and 90", 
            "\n")
        errors <- 1
    }
    if (length(hori) != 24) {
        message("ERROR: You must enter 24 horizon angle values.", 
            "\n")
        errors <- 1
    }
    if (SLE < 0.5 | SLE > 1) {
        message("ERROR: Emissivity (SLE) is out of bounds.\n        Please enter a correct value (0.05 - 1.00).", 
            "\n")
        errors <- 1
    }
    if (ERR < 0) {
        message("ERROR: Error bound (ERR) is too small.\n        Please enter a correct value (> 0.00).", 
            "\n")
        errors <- 1
    }
    if (Usrhyt < RUF) {
        message("ERROR: Reference height (Usrhyt) smaller than roughness height (RUF).\n        Please use a larger height above the surface.", 
            "\n")
        errors <- 1
    }
    if (Usrhyt < 0.005 | Usrhyt > Refhyt) {
        message("ERROR: Reference height (Usrhyt) is out of bounds.\n        Please enter a correct value (0.005 - Refhyt).", 
            "\n")
        errors <- 1
    }
    if (CMH2O < 0.5 | CMH2O > 2) {
        message("ERROR: Preciptable water in air column (CMH2O) is out of bounds.\n        Please enter a correct value (0.1 - 2).", 
            "\n")
        errors <- 1
    }
    if (max(TIMAXS) > 24 | min(TIMAXS) < 0) {
        message("ERROR: At least one of your times of weather maxima (TIMAXS) is out of bounds.\n        Please input a value between 0 and 24", 
            "\n")
        errors <- 1
    }
    if (max(TIMINS) > 24 | min(TIMINS) < 0) {
        message("ERROR: At least one of your times of weather minima (TIMINS) is out of bounds.\n        Please input a value between 0 and 24", 
            "\n")
        errors <- 1
    }
    if (minshade > maxshade | minshade == maxshade) {
        message("ERROR: Your value for minimum shade (minshade) is greater than or equal to the maximum shade (maxshade).\n        Please correct this.", 
            "\n")
        errors <- 1
    }
    if (minshade > 100 | minshade < 0) {
        message("ERROR: Your value for minimum shade (minshade) is out of bounds.\n        Please input a value between 0 and 100.", 
            "\n")
        errors <- 1
    }
    if (maxshade > 100 | maxshade < 0) {
        message("ERROR: Your value for maximum shade (maxshade) is out of bounds.\n        Please input a value between 0 and 100.", 
            "\n")
        errors <- 1
    }
    if (soiltype < 0 | soiltype > 11) {
        message("ERROR: the soil type must range between 1 and 11.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    
	# for working with GHCN Daily data
	if (!suppressMessages(library("rgdal", logical.return=TRUE))) {
		message("ERROR: rgdal needed for working with GHCN data. Please load it before calling this function.")
        errors <- 1
	}
	if (!suppressMessages(library("lubridate", logical.return=TRUE))) {
		message("ERROR: lubridate needed for working with GHCN data. Please install it.")
        errors <- 1
	}
	if (!suppressMessages(library("zoo", logical.return=TRUE))) {
		message("ERROR: zoo needed for working with GHCN data. Please install it.")
        errors <- 1
	}
	if(!exists('ghcn_get')){
		if(file.exists('scripts/ghcn_get.r')){
			source('scripts/ghcn_get.r')		
		} else {
			message('ERROR: expected to find scripts/ghcn_get.r, but did not. Please copy it there.')
	        errors <- 1
		}
	}

    
	# make sure nyears and dates are compatible  
    if (month(mindate)!=1 & day(mindate)!=1) {
        message("ERROR: mindate must start on January 1.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    if (month(maxdate)!=12 & day(maxdate)!=31) {
        message("ERROR: maxdate must end on December 31.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    if (nyears != (year(maxdate) - year(mindate)+1)) {
        message("ERROR: nyears must equal the number of years between mindate and maxdate + 1.\n      Please correct.", 
            "\n")
        errors <- 1
    }
    
    # moved location code here so we can try to get GHCN data before going through the rest of the script
    if(errors==0){
		if (is.numeric(loc) == FALSE & length(loc) == 1) {
            if (!requireNamespace("dismo", quietly = TRUE)) {
                stop("dismo needed for the place name geocode function to work. Please install it.", 
                  call. = FALSE)
            }
            if (!requireNamespace("XML", quietly = TRUE)) {
                stop("XML needed for the place name geocode function to work. Please install it.", 
                  call. = FALSE)
            }
            longlat <- dismo::geocode(loc)[3:4]
            if (nrow(longlat > 1)) {
                longlat <- longlat[1, ]
            }
            x <- t(as.matrix(as.numeric(c(longlat[1, 1], longlat[1, 
                2]))))
        } else {
            if (is.numeric(loc) == FALSE) {
                loc = cbind(as.numeric(loc[1]), as.numeric(loc[2]))
            }
            longlat <- loc
            x <- t(as.matrix(as.numeric(c(loc[1], loc[2]))))
        }

		# get GHCND data
		ghcn <- NULL
		ghcn <- ghcn_get(lat=x[2], lon=x[1], ghcnd_stations=ghcnd_stations, mindate=mindate, maxdate=maxdate, radius=radius, mincov=mincov)
		if(is.null(ghcn)){
			message(paste("ERROR: Failed to get GHCN data for lat", x[2], "lon", x[1], "radius", radius, "km.\n      Perhaps there is not a weather station here, or try a wider search radius.", "\n"))
			errors <- 1	
		}
	}    
    
    if (errors == 0) {
        juldays12 <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 
            288, 319, 349)
        juldaysn <- juldays12
        if (nyears > 1 & timeinterval == 365) {
            for (i in 1:(nyears - 1)) {
                juldaysn <- c(juldaysn, (juldays12 + 365 * i))
            }
        }
        if (timeinterval < 365) {
            microdaily <- 0
        } else {
            microdaily <- 1
        }
        daystart <- as.integer(ceiling(365/timeinterval/2))
        if (timeinterval != 12) {
            juldays <- seq(daystart, 365, as.integer(floor(365/timeinterval)))
        } else {
            juldays <- juldaysn
        }
        julnum <- timeinterval * nyears
        julday <- subset(juldays, juldays != 0)
        julday <- rep(julday, nyears)
        idayst <- 1
        ida <- timeinterval * nyears

        if (timezone == 1) {
            if (!require(geonames, quietly = TRUE)) {
                stop("package \"geonames\" is required to do a specific time zone (timezone=1). Please install it.")
            }
            ALREF <- (geonames::GNtimezone(longlat[2], longlat[1])[4]) * 
                -15
        } else {
            ALREF <- abs(trunc(x[1]))
        }
        HEMIS <- ifelse(x[2] < 0, 2, 1)
        ALAT <- abs(trunc(x[2]))
        AMINUT <- (abs(x[2]) - ALAT) * 60
        ALONG <- abs(trunc(x[1]))
        ALMINT <- (abs(x[1]) - ALONG) * 60
        azmuth <- aspect
        hori <- as.matrix(hori)
        VIEWF <- 1 - sum(sin(as.data.frame(hori) * pi/180))/length(hori)
        SLES <- rep(SLE, timeinterval * nyears)
        MAXSHADES <- rep(0, (timeinterval * nyears)) + maxshade
        MINSHADES <- rep(0, (timeinterval * nyears)) + minshade
        if (soiltype == 0) {
            BulkDensity <- Density
            cap = 0
            runmoist <- 0
            PE <- rep(CampNormTbl9_1[1, 4], 19)
            KS <- rep(CampNormTbl9_1[1, 6], 19)
            BB <- rep(CampNormTbl9_1[1, 5], 19)
            BD <- rep(BulkDensity/1000, 19)
        } else {
            if (soiltype < 12) {
                PE <- rep(CampNormTbl9_1[soiltype, 4], 19)
                KS <- rep(CampNormTbl9_1[soiltype, 6], 19)
                BB <- rep(CampNormTbl9_1[soiltype, 5], 19)
                BD <- rep(BulkDensity/1000, 19)
            }
        }
        gcfolder <- paste(.libPaths()[1], "/gcfolder.rda", sep = "")
        if (file.exists(gcfolder) == FALSE) {
            message("You don't appear to have the global climate data set - \n run function get.global.climate(folder = 'folder you want to put it in') .....\n exiting function micro_global")
            opt <- options(show.error.messages = FALSE)
            on.exit(options(opt))
            stop()
        }
        load(gcfolder)
        message("extracting climate data", "\n")
        global_climate <- raster::brick(paste(folder, "/global_climate.nc", 
            sep = ""))
        CLIMATE <- raster::extract(global_climate, x)
        ALTT <- as.numeric(CLIMATE[, 1])
        RAINFALL <- CLIMATE[, 2:13]
        RAINYDAYS <- CLIMATE[, 14:25]/10
        WNMAXX <- CLIMATE[, 26:37]/10
        WNMINN <- WNMAXX * 0.1
        TMINN <- CLIMATE[, 38:49]/10
        TMAXX <- CLIMATE[, 50:61]/10
        ALLMINTEMPS <- TMINN
        ALLMAXTEMPS <- TMAXX
        ALLTEMPS <- cbind(ALLMAXTEMPS, ALLMINTEMPS)
        RHMINN <- CLIMATE[, 62:73]/10
        RHMAXX <- CLIMATE[, 74:85]/10
        CCMINN <- CLIMATE[, 86:97]/10
        if (clearsky == 1) {
            CCMINN = CCMINN * 0
        }
        CCMAXX <- CCMINN
        if (runmoist == 0) {
            soilmoisture <- suppressWarnings(raster::brick(paste(folder, 
                "/soilw.mon.ltm.v2.nc", sep = "")))
            message("extracting soil moisture data", "\n")
            SoilMoist <- raster::extract(soilmoisture, x)/1000
        }
        if (is.na(max(SoilMoist, ALTT, CLIMATE)) == TRUE) {
            message("Sorry, there is no environmental data for this location")
            stop()
        }
        WNMINN <- WNMINN * (1.2/10)^0.15
        WNMAXX <- WNMAXX * (1.2/10)^0.15
        if (timeinterval != 12) {
        	# MODIFICATIONS FROM ORIGINAL SCRIPT ARE HERE        	
        	# calculate climatology for each month from ghcn
        	ghcn$month <- month(ghcn$date)
        	ghcn_bymo <- aggregate(list(tmin_bymo=ghcn$tmin, tmax_bymo=ghcn$tmax), by=list(month=ghcn$month), FUN=mean, na.rm=TRUE)
        		
        	# calculate GHCN anomalies from climatology
			ghcn <- merge(ghcn, ghcn_bymo, by='month')
			ghcn$tmin_anom <- ghcn$tmin - ghcn$tmin_bymo
			ghcn$tmax_anom <- ghcn$tmax - ghcn$tmax_bymo

			# add anomalies to NicheMapR local climatology by month
			ghcn <- merge(ghcn, data.frame(month=1:12, tmax_NM=TMAXX, tmin_NM=TMINN))
			ghcn$tmin_adj <- ghcn$tmin_NM + ghcn$tmin_anom
			ghcn$tmax_adj <- ghcn$tmax_NM + ghcn$tmax_anom
			
			# trim out leap days so all years are 365 days (to match rest of script)
			inds <- ghcn$month==2 & mday(ghcn$date)==29
			ghcn <- ghcn[!inds,]
			
			# order
			ghcn <- ghcn[order(ghcn$date),]
			
			# spline filling for beginning and end (if needed), since won't be filled by na.approx
			TMINN1 <- suppressWarnings(spline(juldays12, TMINN, 
				n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
			ghcn$tmin_NM_spline <- rep(TMINN1$y, nyears)
			TMAXX1 <- suppressWarnings(spline(juldays12, TMAXX, 
				n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
			ghcn$tmax_NM_spline <- rep(TMAXX1$y, nyears)

			# linear interpolation up to 30 days
			ghcn$tmin_fill <- na.approx(ghcn$tmin_adj, na.rm=FALSE, maxgap=30)
			ghcn$tmax_fill <- na.approx(ghcn$tmax_adj, na.rm=FALSE, maxgap=30)

			# use spline fill from average months where still missing (beginning, end, or gaps > 30 days)
			ghcn$tmin_fill[is.na(ghcn$tmin_fill)] <- ghcn$tmin_NM_spline[is.na(ghcn$tmin_fill)]
			ghcn$tmax_fill[is.na(ghcn$tmax_fill)] <- ghcn$tmax_NM_spline[is.na(ghcn$tmax_fill)]

			# fill variables used by NM script
			TMINN <- ghcn$tmin_fill
			TMAXX <- ghcn$tmax_fill
			ALLMINTEMPS <- TMINN
			ALLMAXTEMPS <- TMAXX
			ALLTEMPS <- cbind(ALLMAXTEMPS, ALLMINTEMPS)
        
        	# the GHCN anomaly calculations replaced this spline filling method
#            TMAXX1 <- suppressWarnings(spline(juldays12, TMAXX, 
#                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
#            TMAXX <- rep(TMAXX1$y, nyears)
#            TMINN1 <- suppressWarnings(spline(juldays12, TMINN, 
#                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
#            TMINN <- rep(TMINN1$y, nyears)

            RHMAXX1 <- suppressWarnings(spline(juldays12, RHMAXX, 
                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
            RHMAXX <- rep(RHMAXX1$y, nyears)
            RHMINN1 <- suppressWarnings(spline(juldays12, RHMINN, 
                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
            RHMINN <- rep(RHMINN1$y, nyears)
            CCMAXX1 <- suppressWarnings(spline(juldays12, CCMAXX, 
                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
            CCMAXX <- rep(CCMAXX1$y, nyears)
            CCMINN <- CCMAXX
            WNMAXX1 <- suppressWarnings(spline(juldays12, WNMAXX, 
                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
            WNMAXX <- rep(WNMAXX1$y, nyears)
            WNMINN1 <- suppressWarnings(spline(juldays12, WNMINN, 
                n = timeinterval, xmin = 1, xmax = 365, method = "periodic"))
            WNMINN <- rep(WNMINN1$y, nyears)
            if (runmoist == 0) {
                SoilMoist1 <- suppressWarnings(spline(juldays12, 
                  SoilMoist, n = timeinterval, xmin = 1, xmax = 365, 
                  method = "periodic"))
                SoilMoist <- rep(SoilMoist1$y, nyears)
            }
            
            if(length(TMAXX) != length(RHMAXX)) stop(paste('TMAXX and RHMAXX are not the same length. TMAXX:', length(TMAXX), 'RHMAXX:', length(RHMAXX), 'Error with leap days?'))
            
        }
        if (timeinterval < 365) {
            TMAXX <- rep(TMAXX, nyears)
            TMINN <- rep(TMINN, nyears)
            RHMAXX <- rep(RHMAXX, nyears)
            RHMINN <- rep(RHMINN, nyears)
            CCMAXX <- rep(CCMAXX, nyears)
            CCMINN <- rep(CCMINN, nyears)
            WNMAXX <- rep(WNMAXX, nyears)
            WNMINN <- rep(WNMINN, nyears)
            if (runmoist == 0) {
                SoilMoist <- rep(SoilMoist, nyears)
            }
            RAINFALL <- rep(RAINFALL, nyears)
        }
        avetemp <- (sum(TMAXX) + sum(TMINN))/(length(TMAXX) * 
            2)
        soilinit <- rep(avetemp, 20)
        tannul <- mean(unlist(ALLTEMPS))
        tannulrun <- rep(tannul, julnum)
        daymon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 
            31)
        if (timeinterval == 365) {
            RAINFALL1 <- 1:365
            sort <- matrix(data = 0, nrow = 365, ncol = 2)
            daymon <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
                30, 31)
            m <- 1
            b <- 0
            for (i in 1:12) {
                ndays = daymon[i]
                for (k in 1:ndays) {
                  b <- b + 1
                  sort[m, 1] <- i
                  sort[m, 2] <- b
                  if (k <= RAINYDAYS[i] & rainfrac > 0) {
                    if (k == 1) {
                      RAINFALL1[m] = RAINFALL[i] * rainfrac * 
                        rainmult
                    } else {
                      RAINFALL1[m] = (RAINFALL[i] * (1 - rainfrac) * 
                        rainmult)/RAINYDAYS[i]
                    }
                  } else {
                    if (rainfrac == 0) {
                      RAINFALL1[m] = (RAINFALL[i] * rainmult)/RAINYDAYS[i]
                    } else {
                      RAINFALL1[m] = 0
                    }
                  }
                  m <- m + 1
                  if (b > RAINYDAYS[i]) {
                    b <- 0
                  }
                }
            }
            RAINFALL2 <- as.data.frame(cbind(RAINFALL1, sort))
            RAINFALL <- rep(as.double(RAINFALL2$RAINFALL1), nyears)
            if (TMINN[1] < snowtemp) {
                RAINFALL[1] <- 0
            }
        } else {
            if (timeinterval != 12) {
                RAINFALL <- rep(rep(sum(RAINFALL)/timeinterval, 
                  timeinterval), nyears)
            } else {
                RAINFALL <- RAINFALL/rep(daymon, nyears)
            }
        }
        dim <- length(RAINFALL)
        if (rungads == 1) {
            relhum <- 1
            optdep.summer <- as.data.frame(rungads(longlat[2], 
                longlat[1], relhum, 0))
            optdep.winter <- as.data.frame(rungads(longlat[2], 
                longlat[1], relhum, 1))
            optdep <- cbind(optdep.winter[, 1], rowMeans(cbind(optdep.summer[, 
                2], optdep.winter[, 2])))
            optdep <- as.data.frame(optdep)
            colnames(optdep) <- c("LAMBDA", "OPTDEPTH")
            a <- lm(OPTDEPTH ~ poly(LAMBDA, 6, raw = TRUE), data = optdep)
            LAMBDA <- c(290, 295, 300, 305, 310, 315, 320, 330, 
                340, 350, 360, 370, 380, 390, 400, 420, 440, 
                460, 480, 500, 520, 540, 560, 580, 600, 620, 
                640, 660, 680, 700, 720, 740, 760, 780, 800, 
                820, 840, 860, 880, 900, 920, 940, 960, 980, 
                1000, 1020, 1080, 1100, 1120, 1140, 1160, 1180, 
                1200, 1220, 1240, 1260, 1280, 1300, 1320, 1380, 
                1400, 1420, 1440, 1460, 1480, 1500, 1540, 1580, 
                1600, 1620, 1640, 1660, 1700, 1720, 1780, 1800, 
                1860, 1900, 1950, 2000, 2020, 2050, 2100, 2120, 
                2150, 2200, 2260, 2300, 2320, 2350, 2380, 2400, 
                2420, 2450, 2490, 2500, 2600, 2700, 2800, 2900, 
                3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 
                3800, 3900, 4000)
            TAI <- predict(a, data.frame(LAMBDA))
        } else {
            TAI <- c(0.42, 0.415, 0.412, 0.408, 0.404, 0.4, 0.395, 
                0.388, 0.379, 0.379, 0.379, 0.375, 0.365, 0.345, 
                0.314, 0.3, 0.288, 0.28, 0.273, 0.264, 0.258, 
                0.253, 0.248, 0.243, 0.236, 0.232, 0.227, 0.223, 
                0.217, 0.213, 0.21, 0.208, 0.205, 0.202, 0.201, 
                0.198, 0.195, 0.193, 0.191, 0.19, 0.188, 0.186, 
                0.184, 0.183, 0.182, 0.181, 0.178, 0.177, 0.176, 
                0.175, 0.175, 0.174, 0.173, 0.172, 0.171, 0.17, 
                0.169, 0.168, 0.167, 0.164, 0.163, 0.163, 0.162, 
                0.161, 0.161, 0.16, 0.159, 0.157, 0.156, 0.156, 
                0.155, 0.154, 0.153, 0.152, 0.15, 0.149, 0.146, 
                0.145, 0.142, 0.14, 0.139, 0.137, 0.135, 0.135, 
                0.133, 0.132, 0.131, 0.13, 0.13, 0.129, 0.129, 
                0.128, 0.128, 0.128, 0.127, 0.127, 0.126, 0.125, 
                0.124, 0.123, 0.121, 0.118, 0.117, 0.115, 0.113, 
                0.11, 0.108, 0.107, 0.105, 0.103, 0.1)
        }
        Numtyps <- 2
        Nodes <- matrix(data = 0, nrow = 10, ncol = dim)
        Nodes[1, 1:dim] <- 3
        Nodes[2, 1:dim] <- 9
        REFLS <- rep(REFL, dim)
        PCTWET <- rep(PCTWET, dim)
        Density <- Density/1000
        BulkDensity <- BulkDensity/1000
        if (runmoist == 0) {
            moists2 <- matrix(nrow = 10, ncol = dim, data = 0)
            moists2[1, ] <- SoilMoist
            moists2[2, ] <- moists2[1, ]
            moists <- moists2
        } else {
            moists2 <- matrix(nrow = 10, ncol = dim, data = 0)
            moists2[1:10, ] <- SoilMoist_Init
            moists2[moists2 > (1 - BulkDensity/Density)] <- (1 - 
                BulkDensity/Density)
            moists <- moists2
        }
        soilprops <- matrix(data = 0, nrow = 10, ncol = 6)
        soilprops[1, 1] <- BulkDensity
        soilprops[2, 1] <- BulkDensity
        soilprops[1, 2] <- min(0.26, 1 - BulkDensity/Density)
        soilprops[2, 2] <- min(0.26, 1 - BulkDensity/Density)
        soilprops[1, 3] <- Clay
        soilprops[2, 3] <- Clay
        if (cap == 1) {
            soilprops[1, 4] <- 0.2
        } else {
            soilprops[1, 4] <- Thcond
        }
        soilprops[2, 4] <- Thcond
        if (cap == 1) {
            soilprops[1, 5] <- 1920
        } else {
            soilprops[1, 5] <- SpecHeat
        }
        soilprops[2, 5] <- SpecHeat
        soilprops[1, 6] <- Density
        soilprops[2, 6] <- Density
        Z01 <- 0
        Z02 <- 0
        ZH1 <- 0
        ZH2 <- 0
        hourly = 0
        rainhourly = 0
        TAIRhr = rep(0, 24 * dim)
        RHhr = rep(0, 24 * dim)
        WNhr = rep(0, 24 * dim)
        CLDhr = rep(0, 24 * dim)
        SOLRhr = rep(0, 24 * dim)
        RAINhr = rep(0, 24 * dim)
        ZENhr = rep(-1, 24 * dim)
        microinput <- c(dim, RUF, ERR, Usrhyt, Refhyt, Numtyps, 
            Z01, Z02, ZH1, ZH2, idayst, ida, HEMIS, ALAT, AMINUT, 
            ALONG, ALMINT, ALREF, slope, azmuth, ALTT, CMH2O, 
            microdaily, tannul, EC, VIEWF, snowtemp, snowdens, 
            snowmelt, undercatch, rainmult, runshade, runmoist, 
            maxpool, evenrain, snowmodel, rainmelt, writecsv, 
            densfun, hourly, rainhourly, lamb, IUV, RW, PC, RL, 
            SP, R1, IM, MAXCOUNT)
        julday1 = matrix(data = 0, nrow = dim, ncol = 1)
        SLES1 = matrix(data = 0, nrow = dim, ncol = 1)
        MAXSHADES1 = matrix(data = 0, nrow = dim, ncol = 1)
        MINSHADES1 = matrix(data = 0, nrow = dim, ncol = 1)
        TMAXX1 = matrix(data = 0, nrow = dim, ncol = 1)
        TMINN1 = matrix(data = 0, nrow = dim, ncol = 1)
        CCMAXX1 = matrix(data = 0, nrow = dim, ncol = 1)
        CCMINN1 = matrix(data = 0, nrow = dim, ncol = 1)
        RHMAXX1 = matrix(data = 0, nrow = dim, ncol = 1)
        RHMINN1 = matrix(data = 0, nrow = dim, ncol = 1)
        WNMAXX1 = matrix(data = 0, nrow = dim, ncol = 1)
        WNMINN1 = matrix(data = 0, nrow = dim, ncol = 1)
        REFLS1 = matrix(data = 0, nrow = dim, ncol = 1)
        PCTWET1 = matrix(data = 0, nrow = dim, ncol = 1)
        RAINFALL1 = matrix(data = 0, nrow = dim, ncol = 1)
        tannul1 = matrix(data = 0, nrow = dim, ncol = 1)
        moists1 = matrix(data = 0, nrow = 10, ncol = dim)
        julday1[1:dim] <- julday
        SLES1[1:dim] <- SLES
        MAXSHADES1[1:dim] <- MAXSHADES
        MINSHADES1[1:dim] <- MINSHADES
        TMAXX1[1:dim] <- TMAXX
        TMINN1[1:dim] <- TMINN
        CCMAXX1[1:dim] <- CCMAXX
        CCMINN1[1:dim] <- CCMINN
        RHMAXX1[1:dim] <- RHMAXX
        RHMINN1[1:dim] <- RHMINN
        WNMAXX1[1:dim] <- WNMAXX
        WNMINN1[1:dim] <- WNMINN
        REFLS1[1:dim] <- REFLS
        PCTWET1[1:dim] <- PCTWET
        RAINFALL1[1:dim] <- RAINFALL
        tannul1[1:dim] <- tannul
        moists1[1:10, 1:dim] <- moists
        if (length(LAI) < dim) {
            LAI <- rep(LAI[1], dim)
        }
        if (shore == 0) {
            tides <- matrix(data = 0, nrow = 24 * dim, ncol = 3)
        }
        micro <- list(tides = tides, microinput = microinput, 
            julday = julday, SLES = SLES1, DEP = DEP, Nodes = Nodes, 
            MAXSHADES = MAXSHADES, MINSHADES = MINSHADES, TIMAXS = TIMAXS, 
            TIMINS = TIMINS, TMAXX = TMAXX1, TMINN = TMINN1, 
            RHMAXX = RHMAXX1, RHMINN = RHMINN1, CCMAXX = CCMAXX1, 
            CCMINN = CCMINN1, WNMAXX = WNMAXX1, WNMINN = WNMINN1, 
            TAIRhr = TAIRhr, RHhr = RHhr, WNhr = WNhr, CLDhr = CLDhr, 
            SOLRhr = SOLRhr, RAINhr = RAINhr, ZENhr = ZENhr, 
            REFLS = REFLS1, PCTWET = PCTWET1, soilinit = soilinit, 
            hori = hori, TAI = TAI, soilprops = soilprops, moists = moists1, 
            RAINFALL = RAINFALL1, tannulrun = tannulrun, PE = PE, 
            KS = KS, BB = BB, BD = BD, L = L, LAI = LAI, snowmodel = snowmodel)
        if (write_input == 1) {
            if (dir.exists("micro csv input") == FALSE) {
                dir.create("micro csv input")
            }
            write.table(as.matrix(microinput), file = "micro csv input/microinput.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(julday, file = "micro csv input/julday.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(SLES, file = "micro csv input/SLES.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(DEP, file = "micro csv input/DEP.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(Nodes, file = "micro csv input/Nodes.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(MAXSHADES, file = "micro csv input/Maxshades.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(MINSHADES, file = "micro csv input/Minshades.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TIMAXS, file = "micro csv input/TIMAXS.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TIMINS, file = "micro csv input/TIMINS.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TMAXX, file = "micro csv input/TMAXX.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TMINN, file = "micro csv input/TMINN.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(RHMAXX, file = "micro csv input/RHMAXX.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(RHMINN, file = "micro csv input/RHMINN.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(CCMAXX, file = "micro csv input/CCMAXX.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(CCMINN, file = "micro csv input/CCMINN.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(WNMAXX, file = "micro csv input/WNMAXX.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(WNMINN, file = "micro csv input/WNMINN.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(REFLS, file = "micro csv input/REFLS.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(PCTWET, file = "micro csv input/PCTWET.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(soilinit, file = "micro csv input/soilinit.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(hori, file = "micro csv input/hori.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TAI, file = "micro csv input/TAI.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(soilprops, file = "micro csv input/soilprop.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(moists, file = "micro csv input/moists.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(RAINFALL, file = "micro csv input/rain.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(tannulrun, file = "micro csv input/tannulrun.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(PE, file = "micro csv input/PE.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(BD, file = "micro csv input/BD.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(BB, file = "micro csv input/BB.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(KS, file = "micro csv input/KS.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(L, file = "micro csv input/L.csv", sep = ",", 
                col.names = NA, qmethod = "double")
            write.table(LAI, file = "micro csv input/LAI.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(tides, file = "micro csv input/tides.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(TAIRhr, file = "micro csv input/TAIRhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(RHhr, file = "micro csv input/RHhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(WNhr, file = "micro csv input/WNhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(CLDhr, file = "micro csv input/CLDhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(SOLRhr, file = "micro csv input/SOLRhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(RAINhr, file = "micro csv input/RAINhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
            write.table(ZENhr, file = "micro csv input/ZENhr.csv", 
                sep = ",", col.names = NA, qmethod = "double")
        }
        if (is.numeric(loc[1])) {
            location <- paste("long", loc[1], "lat", loc[2])
        } else {
            location <- loc
        }
        message(paste("running microclimate model for", timeinterval, 
            "days by", nyears, "years at site", location, "\n"))
        ptm <- proc.time()
        microut <- microclimate(micro)
        message(paste0("runtime ", (proc.time() - ptm)[3], " seconds"))
        metout <- microut$metout
        shadmet <- microut$shadmet
        soil <- microut$soil
        shadsoil <- microut$shadsoil
        if (runmoist == 1) {
            soilmoist <- microut$soilmoist
            shadmoist <- microut$shadmoist
            humid <- microut$humid
            shadhumid <- microut$shadhumid
            soilpot <- microut$soilpot
            shadpot <- microut$shadpot
            plant <- microut$plant
            shadplant <- microut$shadplant
        } else {
            soilpot <- soil
            soilmoist <- soil
            shadpot <- soil
            shadmoist <- soil
            humid <- soil
            shadhumid <- soil
            plant <- cbind(soil, soil[, 3:4])
            shadplant <- cbind(soil, soil[, 3:4])
            soilpot[, 3:12] <- 0
            soilmoist[, 3:12] <- 0.5
            shadpot[, 3:12] <- 0
            shadmoist[, 3:12] <- 0.5
            humid[, 3:12] <- 0.99
            shadhumid[, 3:12] <- 0.99
            plant[, 3:14] <- 0
            shadplant[, 3:14] <- 0
        }
        if (lamb == 1) {
            drlam <- as.data.frame(microut$drlam)
            drrlam <- as.data.frame(microut$drrlam)
            srlam <- as.data.frame(microut$srlam)
            return(list(soil = soil, shadsoil = shadsoil, metout = metout, 
                shadmet = shadmet, soilmoist = soilmoist, shadmoist = shadmoist, 
                humid = humid, shadhumid = shadhumid, soilpot = soilpot, 
                shadpot = shadpot, plant = plant, shadplant = shadplant, 
                RAINFALL = RAINFALL, dim = dim, ALTT = ALTT, 
                REFL = REFL[1], MAXSHADES = MAXSHADES, longlat = c(x[1], 
                  x[2]), nyears = nyears, timeinterval = timeinterval, 
                minshade = minshade, maxshade = maxshade, DEP = DEP, 
                drlam = drlam, drrlam = drrlam, srlam = srlam))
        } else {
            return(list(soil = soil, shadsoil = shadsoil, metout = metout, 
                shadmet = shadmet, soilmoist = soilmoist, shadmoist = shadmoist, 
                humid = humid, shadhumid = shadhumid, soilpot = soilpot, 
                shadpot = shadpot, plant = plant, shadplant = shadplant, 
                RAINFALL = RAINFALL, dim = dim, ALTT = ALTT, 
                REFL = REFL[1], MAXSHADES = MAXSHADES, longlat = c(x[1], 
                  x[2]), nyears = nyears, timeinterval = timeinterval, 
                minshade = minshade, maxshade = maxshade, DEP = DEP))
        }
    }
}
