# Trim Nichemapped to species included in analysis

require(data.table)

# Read in data
dat2<-fread(file="output/dataset_1_hotwater_Nichemapped.csv") # with nichemapping
dat1 <- copy(dat2) # for the raw data
dat3<-fread(file="output/dataset_1_hotwater_Nichemapped_95quant.csv") # with 95th percentile nichemapping

# trim out unused columnns
dat1[,c('V1', 'used_for_thermal_breadth_analysis', 'used_for_thermal_safety_margin_analysis', 'tmin_metric', 'tmin_acc', 'tmin_acc_days', 'tmin_acc_type', 'tmin_col_time_if_field_fresh', 'tmin', 'NM_1cm_airshade', 'NM_2m_airshade', 'NM_exposed_bodytemp', 'NM_exposed_air', 'NM_exposed_body_wet', 'NM_shade_body_wet') := NULL]

dat2[,c('V1', 'used_for_thermal_breadth_analysis', 'used_for_thermal_safety_margin_analysis', 'tmin_metric', 'tmin_acc', 'tmin_acc_days', 'tmin_acc_type', 'tmin_col_time_if_field_fresh', 'tmin') := NULL]

dat3[,c('V1', 'used_for_thermal_breadth_analysis', 'used_for_thermal_safety_margin_analysis', 'tmin_metric', 'tmin_acc', 'tmin_acc_days', 'tmin_acc_type', 'tmin_col_time_if_field_fresh', 'tmin') := NULL]


# remove intertidal so not counted as marine
dat1 <- subset(dat1, !habitat_intertidal=='intertidal')
dat2 <- subset(dat2, !habitat_intertidal=='intertidal')
dat3 <- subset(dat3, !habitat_intertidal=='intertidal')
 

# trim to complete Tmax raw
i1 <- complete.cases(dat1[,c('habitat', 'lat', 'lon', 'tmax', 'Phylum', 'Class', 'Order', 'Family', 'Genus')])
sum(i1) # 468
dat1 <- dat1[i1,]

i2 <- complete.cases(dat2[,c('habitat', 'lat', 'lon', 'tmax', 'Phylum', 'Class', 'Order', 'Family', 'Genus')])
sum(i2) # 468
dat2 <- dat2[i2,]

i3 <- complete.cases(dat3[,c('habitat', 'lat', 'lon', 'tmax', 'Phylum', 'Class', 'Order', 'Family', 'Genus')])
sum(i3) # 468
dat3 <- dat3[i3,]

# remove habitat_intertidal column
dat1[,c('habitat_intertidal') := NULL]
dat2[,c('habitat_intertidal') := NULL]
dat3[,c('habitat_intertidal') := NULL]

# write out
write.csv(dat1, file='data/tmax_data/dataset_1_hotwater.csv', row.names=FALSE)
write.csv(dat2, file='output/dataset_1_hotwater_Nichemapped.csv', row.names=FALSE)
write.csv(dat3, file='output/dataset_1_hotwater_Nichemapped_95quant.csv', row.names=FALSE)