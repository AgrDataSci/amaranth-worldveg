
# recall packages
library("ag5Tools")

##call csv data
df<- read.csv("data/New_Amaranth_tricot_time_date_adjusted.csv")

#check farmers GPS. Are there some farmers who do not have GPS points?
# "amaranth_tricot_logitude"                    "amaranth_tricot_latitude"                    

# remove farmers data that do not have GPS cordinate 
df_gps<- df[-which(is.na(df[,c("amaranth_tricot_logitude", "amaranth_tricot_latitude")])),]


# find mminim day and maximum day
min(as.Date(df_gps[,"amaranth_planting_day"]), na.rm = TRUE)                      
max(as.Date(df_gps[,"amaranth_fourthharvest_day"]), na.rm = TRUE)

### Download climate data
#write down variables that I want to get
variables_ag5 <-c("2m_temperature", "precipitation_flux")
statistic <- c("24_hour_maximum","24_hour_minimum","24_hour_mean", "day_time_mean","day_time_maximum", "night_time_mean","night_time_minimum")
ag5_download(variable = variables_ag5,
             statistic = statistic,
             day = "all",
             month = "all",
             year = c(2020,2021,2022,2023),
             path = "D:/Working_projects/Amaranth")

################################################################################
# there are some farmers data saved wrong date data. For example, some farmers does not have any date data,
# also, some farmers have wrong date data that do not rationally fit time sequence...
# (e.g some farmers data :nursery is much faster thanplanting) It does not make sense.  
# Cheak NA data in date data 
day_data<-names(df_gps)[440:446] 
df_gps<- df_gps[-which(is.na(df_gps[,day_data])),]

# #
# ttest<- df[-which(is.na(df[,c("amaranth_tricot_logitude", "amaranth_tricot_latitude")])),]
# str(which(is.na(ttest[,day_data][c(1,2)])))
# str(which(is.na(ttest[,day_data][c(1,3)])))
# str(which(is.na(ttest[,day_data][c(1,4)])))
# str(which(is.na(ttest[,day_data][c(1,5)])))
# str(which(is.na(ttest[,day_data][c(1,6)])))
# str(which(is.na(ttest[,day_data][c(1,7)])))
# #


# few date overlapped...It is orignial data problem. Delete these date 

a0 <- which(df_gps[,day_data][1] >= df_gps[,day_data][2])
a1 <- which(df_gps[,day_data][1] >= df_gps[,day_data][3])
a2 <- which(df_gps[,day_data][1] >= df_gps[,day_data][4])
a3 <- which(df_gps[,day_data][1] >= df_gps[,day_data][5])
a4 <- which(df_gps[,day_data][1] >= df_gps[,day_data][6])
a5 <- which(df_gps[,day_data][1] >= df_gps[,day_data][7])
 
a6 <- which(df_gps[,day_data][2] >= df_gps[,day_data][3])
a7 <- which(df_gps[,day_data][2] >= df_gps[,day_data][4])
a8 <- which(df_gps[,day_data][2] >= df_gps[,day_data][5])
a9 <- which(df_gps[,day_data][2] >= df_gps[,day_data][6])
a10 <- which(df_gps[,day_data][2] >= df_gps[,day_data][7])
 
a11 <- which(df_gps[,day_data][3] >= df_gps[,day_data][4])
a12 <- which(df_gps[,day_data][3] >= df_gps[,day_data][5])
a13 <- which(df_gps[,day_data][3] >= df_gps[,day_data][6])
a14 <- which(df_gps[,day_data][3] >= df_gps[,day_data][7])
 
a15 <- which(df_gps[,day_data][4] >= df_gps[,day_data][5])
a16 <- which(df_gps[,day_data][4] >= df_gps[,day_data][6])
a17 <- which(df_gps[,day_data][4] >= df_gps[,day_data][7])
 
a18 <- which(df_gps[,day_data][5] >= df_gps[,day_data][6])
a19 <- which(df_gps[,day_data][5] >= df_gps[,day_data][7])
 
a20 <- which(df_gps[,day_data][6] >= df_gps[,day_data][7])

 
 exclude_row<- c(a0, a1, a2, a3, a4, a5, a6 , a7, a8, a9, a10, a11, a12, a13
                 ,a14, a15, a16, a17, a18, a19, a20)
 df_gps<- df_gps[-c(unique(exclude_row)),]
 
 

################################################################################
# trans the climate data to refind climate data forms like
# #maximum day temperature, #minimum night temperature, #T90p, #T10p #TR, #CFD
#WSDI, #CSDI # precipitation total sum, mean, and no rain.

# measured trait
measured_traits_neg<- names(df_gps)[grepl("_neg", names(df_gps))]
measured_traits<- gsub("_neg", "", measured_traits_neg)

# term 1 
# from planting to nursery

# term 2 
# from planting to transplanting

# term 3 
# from planting to firstharvest

# term 4 
# from planting to secondharvest

# term 5 
# from planting to thirdharvest

# term 6 
# from planting to fourthharvest

# term 7?  
# from planting to reproductive

################################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_nursery_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_nursery_max_day_temp,file="plant_nursery_max_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_nursery_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_nursery_mean_day_temp,file="plant_nursery_mean_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_nursery_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_nursery_mean_Night_temp,file="plant_nursery_mean_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_nursery_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_nursery_min_Night_temp,file="plant_nursery_min_Night_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_nursery_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_nursery_mean_24_temp,file="plant_nursery_mean_24_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_nursery_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_nursery_d24h_minimum,file="plant_nursery_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 1 (from planting to nursery)
plant_nursery_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_nursery_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_nursery_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_nursery_d24h_max,file="plant_nursery_d24h_max.RData")




################################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)

plant_transplanting_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_max_day_temp,file="plant_transplanting_max_day_temp.RData")




# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_mean_day_temp,file="plant_transplanting_mean_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_transplanting_mean_Night_temp,file="plant_transplanting_mean_Night_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_min_Night_temp,file="plant_transplanting_min_Night_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_mean_24_temp,file="plant_transplanting_mean_24_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_d24h_minimum,file="plant_transplanting_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 2 (from planting to transplanting)
plant_transplanting_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_transplanting_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_transplanting_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_transplanting_d24h_max,file="plant_transplanting_d24h_max.RData")



###############################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)

plant_firstharvest_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_max_day_temp,file="plant_firstharvest_max_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_mean_day_temp,file="plant_firstharvest_mean_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_mean_Night_temp,file="plant_firstharvest_mean_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_min_Night_temp,file="plant_firstharvest_min_Night_temp.RData")




# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_mean_24_temp,file="plant_firstharvest_mean_24_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_firstharvest_d24h_minimum,file="plant_firstharvest_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 3 (from planting to firstharvest)
plant_firstharvest_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_firstharvest_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_firstharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_firstharvest_d24h_max,file="plant_firstharvest_d24h_max.RData")



################################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)



#there is NA in "amaranth_secondharvest_day"..  fill it again.
df_gps[which(is.na(df_gps[,"amaranth_secondharvest_day"])), day_data]
as.Date(df_gps[which(is.na(df_gps[,"amaranth_secondharvest_day"])), day_data[4]]) + 14
df_gps[which(is.na(df_gps[,"amaranth_secondharvest_day"])), day_data[5]] <- "2022-01-03"

plant_secondharvest_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_mean_day_temp,file="plant_secondharvest_mean_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_mean_Night_temp,file="plant_secondharvest_mean_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_min_Night_temp,file="plant_secondharvest_min_Night_temp.RData")

# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_mean_24_temp,file="plant_secondharvest_mean_24_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_d24h_minimum,file="plant_secondharvest_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 4 (from planting to secondharvest)
plant_secondharvest_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_secondharvest_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_secondharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_secondharvest_d24h_max,file="plant_secondharvest_d24h_max.RData")

################################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)



plant_thirdharvest_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

plant_secondharvest_d24h_max[[700]]

save(plant_thirdharvest_max_day_temp,file="plant_thirdharvest_max_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}


save(plant_thirdharvest_mean_day_temp,file="plant_thirdharvest_mean_day_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_thirdharvest_mean_Night_temp,file="plant_thirdharvest_mean_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_thirdharvest_min_Night_temp,file="plant_thirdharvest_min_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_thirdharvest_mean_24_temp,file="plant_thirdharvest_mean_24_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_thirdharvest_d24h_minimum,file="plant_thirdharvest_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 5 (from planting to thirdharvest)
plant_thirdharvest_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_thirdharvest_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_thirdharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_thirdharvest_d24h_max,file="plant_thirdharvest_d24h_max.RData")


################################################################################
# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)

plant_fourthharvest_max_day_temp <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_max_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_fourthharvest_max_day_temp,file="plant_fourthharvest_max_day_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_mean_day_temp  <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_mean_day_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Day-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_fourthharvest_mean_day_temp,file="plant_fourthharvest_mean_day_temp.RData")




# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_mean_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_mean_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}
save(plant_fourthharvest_mean_Night_temp,file="plant_fourthharvest_mean_Night_temp.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_min_Night_temp <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_min_Night_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-Night-Time",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_fourthharvest_min_Night_temp,file="plant_fourthharvest_min_Night_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_mean_24_temp <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_mean_24_temp[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Mean-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_fourthharvest_mean_24_temp,file="plant_fourthharvest_mean_24_temp.RData")



# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_d24h_minimum  <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_d24h_minimum[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Min-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_fourthharvest_d24h_minimum,file="plant_fourthharvest_d24h_minimum.RData")


# extract data from download maximum of day temperature from downloaded files("D:/Working_projects/Amaranth")
# term 6 (from planting to fourthharvest)
plant_fourthharvest_d24h_max  <-list()
for( i in 1:nrow(df_gps)){plant_fourthharvest_d24h_max[[i]] <- ag5_extract(coords = c(
  lon = df_gps[i,"amaranth_tricot_logitude"],
  lat = df_gps[i,"amaranth_tricot_latitude"]),
  dates= c(start_date = 
             df_gps[i,"amaranth_planting_day"],
           end_date = df_gps[i,"amaranth_fourthharvest_day"]),
  variable = "Temperature-Air-2m",
  statistic = "Max-24h",
  time = NULL,
  celsius = TRUE,
  path = "D:/Working_projects/"
)}

save(plant_fourthharvest_d24h_max,file="plant_fourthharvest_d24h_max.RData")

################################################################################

