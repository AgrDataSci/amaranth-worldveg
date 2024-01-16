# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("dplyr")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")


# read file with API key,
key = "1879b555-d5a3-4b6a-905c-bdc087c8021d"
server = "1000farms"

replace_codes = read.csv("data/replacement-pattern-names.csv")

# get the list of projects from the user indicated in the API key
projects = getProjectsCM(key,
                         server = server)

# select by userowner
userowner = c("sognigbendanikou", "AbdulShango")

projects = projects[projects$user_owner %in% userowner, ]

# make a data.frame with project name, userowner, API key and server
# but not save it, to keep the API key confidential
serverdata = projects[,c("project_id", "user_owner", "server")]

# fetch the data from climmob
cmdata = list()

for(i in seq_along(projects$project_id)) {

  d_i = getDataCM(key,
                  project = serverdata$project_id[i],
                  userowner = serverdata$user_owner[i],
                  server = serverdata$server[i],
                  as.data.frame = FALSE)

  cmdata[[i]] = d_i

}


# put the data into data.frame format
dat = lapply(cmdata, function(x){
  x = as.data.frame(x, 
                    tidynames = TRUE,
                    pivot.wider = TRUE)
  
  names(x) = make_clean_names(names(x))
  
  x
  
})

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# fix some codes that diverged over time
# write more code to fix if needed
for (i in seq_along(replace_codes$name)) {
  dat = lapply(dat, function(x){
    names(x) = gsub(replace_codes$name[i], replace_codes$newname[i], names(x))
    x
  })
}



# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

sort(uniquenames)

# put the data together 
dat = rowbind(dat)

dat

pack_index = paste0("package_item_", letters[1:3])

dat[pack_index] = lapply(dat[pack_index], function(x){
  x = gsub("Local Benin", "Local", x)
  x = gsub("Local check [(]AC-NL[)]", "AC-NL", x)
  x = gsub("Local Mali", "Local", x)
  x
})


# check list of varieties tested
items_available = data.frame(items = unlist(dat[pack_index]),
                             country = rep(dat$package_country, times = 3))


items_available = table(items_available$items, items_available$country)

dir.create("data/summaries/", showWarnings = FALSE)

write.csv(items_available, "data/summaries/varieties-tested-country.csv")

# check availability of traits
trait_list = getTraitList(dat, c("_pos", "_neg"))

traits = unlist(lapply(trait_list, function(x){
  x$trait_label
}))

sort(traits)

table(dat$package_country)

trait_available = lapply(trait_list, function(x){
  z = data.frame(N = x$keep, country = dat$package_country)
  table(z$N, z$country)[2,]
})

trait_available = do.call("rbind", trait_available)

trait_available = cbind(trait_available, N = rowSums(trait_available))

trait_available = as.data.frame(trait_available)

trait_available$trait = traits

write.csv(trait_available, "data/summaries/traits-available.csv")

# Coordinates ####
# get the coordinates as an independent data.frame
keep = grepl("_longitude|_latitude", names(dat))

lonlat = dat[, keep]

dat = dat[, !keep]

lon = grep("_longitude", names(lonlat))
lon = lonlat[, lon]

lon = as.vector(apply(lon, 1, function(x){
  # I'll take the reverse as this increases the likelihood of
  # getting the coordinates from the trial, not the point of
  # delivery
  names(x)[rev(which(!is.na(x)))[1]]
}))

lon[is.na(lon)] = "registration_longitude"

lat = gsub("_longitude", "_latitude", lon)

table(lon)
table(lat)

# keep only the selected columns, one per plot
lonlat = data.frame(longitude = lonlat[cbind(1:nrow(lonlat), lon)],
                    latitude = lonlat[cbind(1:nrow(lonlat), lat)])


lonlat[1:2] = lapply(lonlat[1:2], as.numeric)

plot(lonlat)

plot_map(lonlat, c("longitude", "latitude"),
         map_provider = "OpenStreetMap.Mapnik")


dat = cbind(lonlat, dat)


keep = !grepl("gps_precision|elevation", names(dat))

dat = dat[keep]

# check availability of farmers covariates
covar = grep("registration", names(dat))

covar = lapply(dat[covar], function(x){
  z = data.frame(N = is.na(x),
                 country = dat$package_country)
  table(z$N, z$country)[1,]
}) 

covar = do.call("rbind", covar)

write.csv(trait_available, "data/summaries/covar-available.csv")

# save dat as csv file. 
write.table(dat,
            file = "data/amaranth-tricot-data.csv",
            append = FALSE, 
            sep = "," , 
            row.names = FALSE, 
            col.names = TRUE)

# # keep dat for preserving the original data 
# keep_dat <- dat
# 
# #################################################################################
# ## find similar name to integrate!! 
# dat_names<- names(keep_dat)
# 
# # split names and getter first word from names
# 
# first_word<- c()
# for (i in seq_along(dat_names)){
#   first_word[i] <- strsplit(dat_names, "_")[[i]][1]}
# 
# refinded_names<- unique(first_word)
# 
# refinded_names
# 
# ## change French name to English!!
# # Id 
# 
# # Package
# 
# # Registeration
# 
# # Nursery
# # nursery =  pepiniere
# changed_names<- gsub("pepiniere", "nursery", dat_names)
# changed_names
# 
# # Transplantation
# # transplanting =  transplantation  
# changed_names<- gsub("transplantation", "transplanting", changed_names)
# 
# 
# # firstharvest = premiererecolte = performanceagronomique1
# # premiererecolte -> firstharvest
# # performanceagronomique1 -> firstharvest
# changed_names<- gsub("premiererecolte", "firstharvest", changed_names)
# changed_names<- gsub("performanceagronomique1", "firstharvest", changed_names)
# 
# 
# # secondharvest = performanceagronomique2 = deuxiemerecolte
# changed_names<- gsub("deuxiemerecolte", "secondharvest", changed_names)
# changed_names<- gsub("performanceagronomique2", "secondharvest", changed_names)
# 
# 
# # thirdharvest = performanceagronomique3 = troisiemerecolte
# changed_names<- gsub("troisiemerecolte", "thirdharvest", changed_names)
# changed_names<- gsub("performanceagronomique3", "thirdharvest", changed_names)
# 
# # fourthharvest =  quatriemerecolte
# changed_names<- gsub("quatriemerecolte", "fourthharvest", changed_names)
# 
# 
# # reproductive =  floraison
# changed_names<- gsub("floraison", "reproductive", changed_names)
# 
# # socio_economic =  socioeconomic
# changed_names<- gsub("socio_economic", "socioeconomic", changed_names)
# 
# # remove "fr" ,additional of FR, which record french . 
# keep_vector_number<- grep("fr.*$", changed_names)
# 
# changed_names[keep_vector_number] <- gsub("fr","",  changed_names[keep_vector_number])
# 
# 
# # check overlapped names
# table(changed_names)
# 
# # 3 times overlap in all names
# overlapped3 <- table(changed_names)[table(changed_names) == 3]
# overlapped3<- names(overlapped3)
# # 2 times overlap in all names
# overlapped2 <- table(changed_names)[table(changed_names) == 2]
# overlapped2<- names(overlapped2)
# # 1 times overlap in all names
# overlapped1<- table(changed_names)[table(changed_names) == 1]
# overlapped1<- names(overlapped1)
# 
# 
# # find names vector numbers(thoese are same sequence dataframe column number ) 
# # and try to merge overlapped variables as one 
# # three times overlap
# new_name_overlapp_threetimes<- list()
# keep_temp<-list()
# for(i in seq_along(overlapped3)){
#   for(j in 1:3){
#     keep_temp[j]   <- keep_dat[,c(which(changed_names == overlapped3[i]))][j]}
#   new_name_overlapp_threetimes[[i]]<-  coalesce(keep_temp[[1]],keep_temp[[2]],keep_temp[[3]])
#   names(new_name_overlapp_threetimes)[i] <- overlapped3[i]
# } 
# new_name_overlapp_threetimes
# 
# # two times overlapped 
# new_name_overlapp_secondtimes<- list()
# keep_temp<-list()
# for(i in seq_along(overlapped2)){
#   for(j in 1:2){
#     keep_temp[j]   <- keep_dat[,c(which(changed_names == overlapped2[i]))][j]}
#   new_name_overlapp_secondtimes[[i]]<-  coalesce(keep_temp[[1]],keep_temp[[2]])
#   names(new_name_overlapp_secondtimes)[i] <- overlapped2[i]
# } 
# new_name_overlapp_secondtimes
# 
# 
# # no overlapped variables names 
# 
# new_name_no_overlapp<- list()
# for (i in seq_along(overlapped1)){new_name_no_overlapp[[i]] <- keep_dat[,c(which(changed_names == overlapped1[i]))]
# names(new_name_no_overlapp)[i] <- overlapped1[i]
# }
# 
# # changed from list to dataframe
# new_name_overlapp_threetimes<-as.data.frame(new_name_overlapp_threetimes)
# new_name_overlapp_secondtimes<-as.data.frame(new_name_overlapp_secondtimes)
# new_name_no_overlapp<-as.data.frame(new_name_no_overlapp)
# # combine
# a<- cbind(new_name_overlapp_threetimes, new_name_overlapp_secondtimes)
# new_dat<- cbind(a, new_name_no_overlapp)
# # now marged data frame
# new_dat
# 
# write.table(new_dat,file="data/New_Amaranth_tricot.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
# 
# # but we need to sort name as sequence
# names(new_dat)
# sort_name_lisy<- strsplit(names(new_dat), "_")
# 
# t_vector<-c()
# for(i in seq_along(sort_name_lisy)){
#   t_vector[i]  <- sort_name_lisy[[i]][1]}
# variables_names_seq<- unique(t_vector)
# variables_names_seq1 <- c("id", "registration", "package", "nursery", "transplanting","firstharvest" , "secondharvest", "thirdharvest" ,  "fourthharvest", "reproductive",  "socioeconomic")
# variables_names_seq %in% variables_names_seq
# variables_names_seq1 %in% variables_names_seq1
# variables_names_seq <- variables_names_seq1
# 
# vns<- variables_names_seq[-1] # except id becuse there are some variables that have id in their names
# 
# result <- lapply(vns, function(v) names(new_dat[, grep(v, names(new_dat))]))
# new_name_seq <- c(variables_names_seq[1], result)
# 
# unlist(new_name_seq)
# 
# # here is marged version
# new_date_frame<- new_dat[, c(unlist(new_name_seq))]
# 
# # here is marged version but also I cleaned variables all NA (empty)
# new_date_frame_ver2<- new_date_frame[,colSums(is.na(new_date_frame)) != nrow(new_date_frame)]
# #save it
# write.table(new_date_frame_ver2,file="data/Amaranth_tricot_marged_version_deleted_emptycol.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
# 
# # save new_date_frame_ver2 and change name as "df" 
# df <- new_date_frame_ver2
# 
# # lets change ID (ID described as just number)
# 
# #there are "Not observed" data. lets change "Not observed" to NA
# df[df == "Not observed"] <- NA
# 
# # delete unnecessary columns like names and telephone 
# df<- df[,-c(which(names(df) == "registration_telephone"))]  # delete participant telephone
# df<- df[,-c(which(names(df) == "registration_participant_name"))]  # delete participant name
# df<- df[,-c(which(names(df) == "registration_clc_after"))]
# df<- df[,-c(which(names(df) == "registration_clc_before"))]
# 
# 
# # find columns that have french respond  
# loop_result <- c()
# for( i in seq_along(df)){loop_result[i]<- "Oui" %in%  attributes(table(df[,i]))$dimnames[[1]]}
# 
# # deduct 24 and 29 in loop_result because these column were described as French!  
# f_to_e_column<- names(df)[which(loop_result)]
# f_to_e_column<- f_to_e_column[-which(f_to_e_column == "socioeconomic_noprodwhy")]
# f_to_e_column<- f_to_e_column[-which(f_to_e_column == "socioeconomic_prodpract_oth")]
# 
# 
# # lets change  Oui to Yes , Mon to no
# for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("Oui", "Yes",df[[f_to_e_column[i]]])}
# for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("Non", "No",df[[f_to_e_column[i]]])}
# for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("No Applicable", "Not Applicable",df[[f_to_e_column[i]]])}
# 
# table(df[[f_to_e_column[12]]])
# df[,f_to_e_column[12]]<- gsub("1 mois", "1 month", df[,f_to_e_column[12]])
# df[,f_to_e_column[12]]<- gsub("Ouï", "Yes", df[,f_to_e_column[12]])
# 
# 
# # lets change Id first with "package_project_name"
# temp_id <- c()
# for (i in 1:nrow(df)){ 
#   temp_id[i] <- strsplit(df[,"package_project_name"], "-")[[i]][1]}
# 
# df[["id"]]<- paste(df[["id"]], temp_id, sep ="_")
# # check overlapped id
# table(table(df[["id"]]))
# 
# 
# # delete other Id column 
# delete_id_columne<- names(df)[grep("id", names(df))][2:19]
# delete_id_columne
# 
# for (i in seq_along(delete_id_columne)) {
#   df <- df[,-c(which(names(df) == delete_id_columne[i]))]}
# 
# # delete unnecessary columns
# delete_names_from_all <- c("coordinator", "name", "submitted_by", "deviceimei", "clc_after", "socioeconomic_telephone")
# 
# temp_list <- list()
# for (i in seq_along(delete_names_from_all)) {
#   temp_list[[i]] <- grep(delete_names_from_all[[i]], names(df))}
# 
# temp_list<- unlist(temp_list)
# temp_list
# 
# names(df)[c(temp_list)]
# 
# df<-df[,-c(temp_list)]
# keep_df <- df # save it 
# 
# names(df)
# 
# # unify gps data and day data  
# # longitude.. merge all 
# names(df)[grepl("longitude", names(df))]
# amaranth_tricot_logitude<- do.call(coalesce, as.list(df[,c(names(df)[grepl("longitude", names(df))])]))
# #delete logtitude
# # df<- df[,-which(names(df) %in% names(df)[grepl("longitude", names(df))])]
# 
# # latitude.. merge all 
# names(df)[grepl("latitude", names(df))]
# amaranth_tricot_latitude<- do.call(coalesce, as.list(df[,c(names(df)[grepl("latitude", names(df))])]))
# #delete logtitude
# # df<- df[,-which(names(df) %in% names(df)[grepl("latitude", names(df))])]
# 
# 
# amaranth_gps_location<- cbind(amaranth_tricot_logitude,amaranth_tricot_latitude)
# df<- cbind( df, amaranth_gps_location)
# 
# # gps location... by using village name 
# unique(df[,names(df)[grepl("village", names(df))][1]])
# unique(df[,names(df)[grepl("distri", names(df))][1]])
# 
# 
# # date  
# # However, first change all data as date form 
# df[,names(df)[grepl("date", names(df))]]<- lapply(df[,names(df)[grepl("date", names(df))]], as.Date, format ="%Y-%m-%d")
# # done 
# 
# 
# names(df)[grepl("date", names(df))] 
# names(df)[grepl("date", names(df))][1]  # registration. if there are no planting day from nursey.. set planting day as registration day.
# names(df)[grepl("date", names(df))][2:4] # nursery  1 week after registration
# names(df)[grepl("date", names(df))][5:8] # transplanting : 1 week after transplanting
# names(df)[grepl("date", names(df))][9:13] # firstharvest :  3 weeks after transplanting
# names(df)[grepl("date", names(df))][14:18] # secondharvest : 2 weeks after the first harvest
# names(df)[grepl("date", names(df))][19:23] # thirdharvest : 2 weeks after the second harvest
# names(df)[grepl("date", names(df))][24:28] # fourthharvest : 2 weeks after the third harvest
# names(df)[grepl("date", names(df))][29:30] # reproductive
# names(df)[grepl("date", names(df))][31]  # socioeconomic
# 
# 
# 
# 
# # lets be grouping row based on data cirmumstance for easy calculation  
# df_date_row_list <- list()
# df_date_row_number <- c()
# for (i in 1:nrow(df)){x<-  which(!is.na(df[i,c(names(df)[grepl("date", names(df))][1:30])]))
#                       df_date_row_list[[i]] <-x
#                       df_date_row_number[i] <- paste(x,collapse = "-")}
# df_date_row_number
# 
# 
# df_dat_grouped_row_by_date <- list()
# for( i in seq_along(names(table(df_date_row_number)))){df_dat_grouped_row_by_date[[i]]<- which(df_date_row_number %in%  names(table(df_date_row_number))[i])}
# 
# names(df_dat_grouped_row_by_date) <- names(table(df_date_row_number))
# 
# names(df_dat_grouped_row_by_date) 
# 
# 
# names(df)[grepl("date", names(df))][1:31]
# #cheate new columns for time data... 
# # planting day
# amaranth_planting_day <- rep(NA,2063)
# # nursery day 
# amaranth_nursery_day <- rep(NA,2063)
# # transplanting
# amaranth_transplanting_day <- rep(NA,2063)
# # firstharvest
# amaranth_firstharvest_day <- rep(NA,2063)
# # secondharvest
# amaranth_secondharvest_day <- rep(NA,2063)
# # thirdharvest
# amaranth_thirdharvest_day <- rep(NA,2063)
# # fourthharvest
# amaranth_fourthharvest_day <- rep(NA,2063)
# 
# day_data<- data.frame(amaranth_planting_day, amaranth_nursery_day, amaranth_transplanting_day
#            , amaranth_firstharvest_day, amaranth_secondharvest_day, amaranth_thirdharvest_day,
#            amaranth_fourthharvest_day)
# 
# df<- cbind(df, day_data)
# df[,names(day_data)]<- lapply(df[,names(day_data)], as.Date, format ="%Y-%m-%d")
# 
# 
# # only registration day there are no planting day. So, set planting day as registeration day
# # names(df_dat_grouped_row_by_date)[2]
# 
# df[df_dat_grouped_row_by_date[[2]],names(df)[grepl("date", names(df))][1:30]]
# 
# names(day_data)
# # set planting day by using registration day(registration day = planting day)
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[2]],names(df)[grepl("date", names(df))][1:30]][1]
# 
# # set nursery day - 1 week after registration
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[1]]  + 7
# 
# # transplanting day - 2 weeks after nursery 
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[3]] + 21 
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[2]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[2]], names(day_data)[6]] + 14
# 
# #check
# df[df_dat_grouped_row_by_date[[2]],names(day_data)]
# 
# 
# 
# 
# 
# # 1-2-3  names(df_dat_grouped_row_by_date)[3], there are no assesment day in nursry and so calcuate day based on the amaranth tricot
# df[df_dat_grouped_row_by_date[[3]],names(df)[grepl("date", names(df))][1:30]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[3]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[1]]  + 7
# 
# # transplanting day - 2 weeks after nursery 
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[3]] + 21 
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[3]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[3]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[3]],names(day_data)]
# 
# 
# # 1-2-3-29` names(df_dat_grouped_row_by_date)[4], there are no assesment day in nursry and so calcuate day based on the amaranth tricot
# df[df_dat_grouped_row_by_date[[4]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[4]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[1]]  + 7
# 
# # transplanting day - 2 weeks after nursery 
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[3]] + 21 
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[4]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[4]], names(day_data)[6]] + 14
# 
# #check
# df[df_dat_grouped_row_by_date[[4]],names(day_data)]
# 
# 
# # 1-2-3-5-6`names(df_dat_grouped_row_by_date)[5],  there are no assesment day in nursry and so calcuate day based on the amaranth tricot
# df[df_dat_grouped_row_by_date[[5]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[5]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[5]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are transplanting_transplanting day in the data set but it is not transplating ssessmentdate 
# # therefore, use transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[5]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[5]], names(day_data)[3]] + 21 
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[5]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[5]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[5]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[5]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[5]],names(day_data)]
# 
# 
# # "1-2-3-5-6-9"`names(df_dat_grouped_row_by_date)[6]
# names(df_dat_grouped_row_by_date)[6]
# df[df_dat_grouped_row_by_date[[6]], names(df)[grepl("date", names(df))]]
# 
# df[df_dat_grouped_row_by_date[[6]],names(df)[grepl("date", names(df))]][9]
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[6]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day as the protocal 
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[6]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[6]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There  are no first harvest day date..(do not use submittign day)
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[6]], names(day_data)[3]] + 21
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[6]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[6]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[6]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[6]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[6]],names(day_data)]
# 
# 
# # "1-2-3-5-6-9-10-11-12-14-15-16-17"names(df_dat_grouped_row_by_date)[7]
# df[df_dat_grouped_row_by_date[[7]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[7]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[7]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[7]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[7]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[7]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[7]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest
# df[df_dat_grouped_row_by_date[[7]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[7]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[7]],names(day_data)]
# 
# 
# # "1-2-3-5-6-9-10-11-12-14-15-16-17-19-20-21-22""names(df_dat_grouped_row_by_date)[8]
# names(df_dat_grouped_row_by_date)[8]
# df[df_dat_grouped_row_by_date[[8]], names(df)[grepl("date", names(df))]]
# 
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[8]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[8]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[8]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[8]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. There are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[8]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest  There are thirdharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[8]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are No fourthharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[8]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[8]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[8]],names(day_data)]
# 
# 
# 
# #  "1-2-3-5-6-9-10-11-12-14-15-16-17-19-20-21-22-24-25-26-27""names(df_dat_grouped_row_by_date)[9]
# names(df_dat_grouped_row_by_date)[9]
# df[df_dat_grouped_row_by_date[[9]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[9]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[9]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[9]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[9]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. There are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[9]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are thirdharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[9]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest.  There are fourthharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[9]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[9]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[9]],names(day_data)]
# 
# 
# #  "1-2-3-5-6-9-10-11-12-14-15-16-17-19-20-21-22-24-25-26-27-29"names(df_dat_grouped_row_by_date)[10]
# names(df_dat_grouped_row_by_date)[10]
# df[df_dat_grouped_row_by_date[[10]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[10]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[10]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[10]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[10]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. There are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[10]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest, There are thirdharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[10]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are fourthharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[10]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[10]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[10]],names(day_data)]
# 
# 
# #  ""1-2-3-5-6-9-10-11-12-14-15-16-17-19-20-21-22-29"names(df_dat_grouped_row_by_date)[11]
# names(df_dat_grouped_row_by_date)[11]
# df[df_dat_grouped_row_by_date[[11]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[11]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[11]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[11]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. there are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[11]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. there are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[11]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. there are thirdharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[11]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. there are fourthharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[11]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[11]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[11]],names(day_data)]
# 
# 
# #  "1-2-3-5-6-9-10-11-12-14-15-16-17-19-29" names(df_dat_grouped_row_by_date)[12]
# names(df_dat_grouped_row_by_date)[12]
# df[df_dat_grouped_row_by_date[[12]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[12]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[12]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[12]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[12]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. There are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[12]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are No thirdharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[12]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are No fourthharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[12]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[12]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[12]],names(day_data)]
# 
# 
# #  "1-2-3-5-6-9-10-11-12-14-15-16-17-29" names(df_dat_grouped_row_by_date)[13]
# names(df_dat_grouped_row_by_date)[13]
# df[df_dat_grouped_row_by_date[[13]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[13]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[13]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[13]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. there are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[13]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. here are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[13]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are No thirdharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[13]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are No fourthharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[13]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[13]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[13]],names(day_data)]
# 
# 
# 
# #  "1-2-3-5-6-9-10-11-12-14-19-20-21-22-24-25-26-27-29" names(df_dat_grouped_row_by_date)[14]
# names(df_dat_grouped_row_by_date)[14]
# df[df_dat_grouped_row_by_date[[14]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[14]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[14]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[14]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. there are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[14]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. here are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[14]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are No thirdharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[14]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are No fourthharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[14]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[14]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[14]],names(day_data)]
# 
# 
# 
# #  "1-2-3-5-6-9-10-11-12-29"   names(df_dat_grouped_row_by_date)[15]
# names(df_dat_grouped_row_by_date)[15]
# df[df_dat_grouped_row_by_date[[15]], names(df)[grepl("date", names(df))]][10]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[15]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[15]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[15]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are firstharvest_harvestingdate day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[15]],names(df)[grepl("date", names(df))]][10]
# 
# # secondharvest day - 2 weeks after the first harvest. There are No secondharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[15]], names(day_data)[4]] + 14 
# 
# # thirdharvest day - 2 weeks after the second harvest. There are No thirdharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[15]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are No fourthharvest harvest day for ABC. calculate it based on the protocol
# df[df_dat_grouped_row_by_date[[15]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[15]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[15]],names(day_data)]
# 
# 
# 
# #  "1-2-3-5-6-9-14-15-16-17-19-20-21-22-24-25-26-27-29"  names(df_dat_grouped_row_by_date)[16]
# names(df_dat_grouped_row_by_date)[16]
# df[df_dat_grouped_row_by_date[[16]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[16]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[16]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[16]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. there are no firstharvest_harvestingdate day for ABC. caclulate day from second harvest day - 14
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[16]], names(df)[grepl("date", names(df))]][15] -14
# 
# # secondharvest day - 2 weeks after the first harvest. there are secondharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[16]], names(df)[grepl("date", names(df))]][15]
# 
# # thirdharvest day - 2 weeks after the second harvest. there are thirdharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[16]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. there are fourthharvest harvest day for ABC. they are all same
# df[df_dat_grouped_row_by_date[[16]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[16]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[16]],names(day_data)]
# 
# 
# 
# #  "1-2-3-5-6-9-14-19" names(df_dat_grouped_row_by_date)[17]
# names(df_dat_grouped_row_by_date)[17]
# df[df_dat_grouped_row_by_date[[17]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[17]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[17]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[17]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[17]], names(day_data)[3]] + 14
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[17]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[17]], names(day_data)[5]] +14
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[17]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[17]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[17]],names(day_data)]
# 
# 
# 
# #   "1-2-3-5-6-9-29" names(df_dat_grouped_row_by_date)[18]
# names(df_dat_grouped_row_by_date)[18]
# df[df_dat_grouped_row_by_date[[18]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[18]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[18]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[18]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[18]], names(day_data)[3]] + 14
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[18]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[18]], names(day_data)[5]] +14
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[18]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[18]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[18]],names(day_data)]
# 
# 
# 
# #   ""1-2-3-9"names(df_dat_grouped_row_by_date)[19]
# names(df_dat_grouped_row_by_date)[19]
# df[df_dat_grouped_row_by_date[[19]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[19]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[19]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[19]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are  firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[19]],names(df)[grepl("date", names(df))]][9]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[19]], names(day_data)[4]] + 14
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[19]], names(day_data)[5]] +14
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[19]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[19]], names(day_data)[6]] + 14
# #check
# df[df_dat_grouped_row_by_date[[19]],names(day_data)]
# 
# 
# 
# 
# #   1-2-3-9-10-11-12-14-15-16-17-19-20-21-22-24-25-26-27-29 names(df_dat_grouped_row_by_date)[20]
# names(df_dat_grouped_row_by_date)[20]
# df[df_dat_grouped_row_by_date[[20]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[20]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[20]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[20]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are  firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[4]] <-df[df_dat_grouped_row_by_date[[20]], names(df)[grepl("date", names(df))]][9]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[20]], names(df)[grepl("date", names(df))]][16]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[20]], names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[20]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[20]], names(df)[grepl("date", names(df))]][25]
# #check
# df[df_dat_grouped_row_by_date[[20]],names(day_data)]
# 
# 
# 
# 
# #   "1-2-3-9-10-11-12-14-15-16-17-19-20-21-22-29"names(df_dat_grouped_row_by_date)[21]
# names(df_dat_grouped_row_by_date)[21]
# df[df_dat_grouped_row_by_date[[21]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[21]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[21]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[21]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are  firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[4]] <-df[df_dat_grouped_row_by_date[[21]], names(df)[grepl("date", names(df))]][9]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[21]], names(df)[grepl("date", names(df))]][16]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[21]], names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[21]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[21]], names(day_data)[6]] + 14 
# #check
# df[df_dat_grouped_row_by_date[[21]],names(day_data)]
# 
# 
# 
# #   "1-2-3-9-10-11-12-14-15-16-17-24-25-26-27-29"names(df_dat_grouped_row_by_date)[22]
# names(df_dat_grouped_row_by_date)[22]
# df[df_dat_grouped_row_by_date[[22]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[22]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[22]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[22]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are  firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[4]] <-df[df_dat_grouped_row_by_date[[22]], names(df)[grepl("date", names(df))]][9]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[22]], names(df)[grepl("date", names(df))]][16]
# 
# # thirdharvest day - 2 weeks after the second harvest. There are no thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[6]] <-df[df_dat_grouped_row_by_date[[22]], names(df)[grepl("date", names(df))]][16] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are no fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[22]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[22]], names(df)[grepl("date", names(df))]][25]
# df[df_dat_grouped_row_by_date[[22]],names(day_data)]
# 
# 
# 
# 
# # "1-2-3-9-10-11-12-14-19-20-21-22-24-25-26-27-29" "names(df_dat_grouped_row_by_date)[23]
# names(df_dat_grouped_row_by_date)[23]
# df[df_dat_grouped_row_by_date[[23]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[23]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[23]],names(df)[grepl("date", names(df))]][2] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[23]], names(day_data)[2]] +21
# 
# # firstharvest day  -  3 weeks after transplanting. There are  firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[4]] <-df[df_dat_grouped_row_by_date[[23]], names(df)[grepl("date", names(df))]][9]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[23]], names(df)[grepl("date", names(df))]][9] +14
# 
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[6]] <-df[df_dat_grouped_row_by_date[[23]], names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[23]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[23]], names(df)[grepl("date", names(df))]][25]
# df[df_dat_grouped_row_by_date[[23]],names(day_data)]
# 
# 
# #   "1-5-6" "names(df_dat_grouped_row_by_date)[24]
# names(df_dat_grouped_row_by_date)[24]
# df[df_dat_grouped_row_by_date[[24]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[24]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[24]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[3]] <-df[df_dat_grouped_row_by_date[[24]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[24]], names(day_data)[3]] + 14
#   
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[24]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[6]] <-  df[df_dat_grouped_row_by_date[[24]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[24]], names(day_data)[7]] <-  df[df_dat_grouped_row_by_date[[24]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[24]],names(day_data)]
# 
# 
# 
# #   "1-5-6-8-9-10-11-12" "names(df_dat_grouped_row_by_date)[25]
# names(df_dat_grouped_row_by_date)[25]
# df[df_dat_grouped_row_by_date[[25]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[25]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[25]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[3]] <-df[df_dat_grouped_row_by_date[[25]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[25]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[25]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[6]] <-  df[df_dat_grouped_row_by_date[[25]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[25]], names(day_data)[7]] <-  df[df_dat_grouped_row_by_date[[25]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[25]],names(day_data)]
# 
# 
# 
# #   "1-5-6-9-10-11-12-14-15-16-17-19-20-21-22-24-25-26-27-29" "names(df_dat_grouped_row_by_date)[26]
# names(df_dat_grouped_row_by_date)[26]
# df[df_dat_grouped_row_by_date[[26]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[26]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[3]] <-df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][16]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[26]], names(day_data)[7]] <-  df[df_dat_grouped_row_by_date[[26]],names(df)[grepl("date", names(df))]][25]
# df[df_dat_grouped_row_by_date[[26]],names(day_data)]
# 
# 
# 
# #   1-5-6-9-10-11-12-14-15-16-17-19-20-21-22-29" "names(df_dat_grouped_row_by_date)[27]
# names(df_dat_grouped_row_by_date)[27]
# df[df_dat_grouped_row_by_date[[27]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[27]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[27]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[3]] <-df[df_dat_grouped_row_by_date[[27]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[27]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[27]],names(df)[grepl("date", names(df))]][16]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[27]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[27]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[27]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[27]],names(day_data)]
# 
# 
# 
# #  1-5-6-9-10-11-12-29""names(df_dat_grouped_row_by_date)[28]
# names(df_dat_grouped_row_by_date)[28]
# df[df_dat_grouped_row_by_date[[28]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[28]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[28]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[3]] <-df[df_dat_grouped_row_by_date[[28]],names(df)[grepl("date", names(df))]][5]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[28]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[28]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[28]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[28]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[28]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[28]],names(day_data)]
# 
# 
# #  "1-9""names(df_dat_grouped_row_by_date)[29]
# names(df_dat_grouped_row_by_date)[29]
# df[df_dat_grouped_row_by_date[[29]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[29]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[3]] + 21
#   
# # secondharvest day - 2 weeks after the first harvest. There are no secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[29]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[29]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[29]],names(day_data)]
# 
# 
# #  "1-9-10-11-12-14-15-16-17-19-20-21-22-24-25-26-27-29" names(df_dat_grouped_row_by_date)[30]
# names(df_dat_grouped_row_by_date)[30]
# df[df_dat_grouped_row_by_date[[30]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[30]],names(df)[grepl("date", names(df))]][1]
# 
# # set nursery day - 1 week after registration. There are no nursery assessment date. cacluate day based on the protocal 
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[30]], names(day_data)[1]] + 7
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[30]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[30]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[30]],names(df)[grepl("date", names(df))]][17]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[30]],names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[30]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[30]],names(df)[grepl("date", names(df))]][26]
# df[df_dat_grouped_row_by_date[[30]],names(day_data)]
# 
# 
# #  "2-3-4" names(df_dat_grouped_row_by_date)[31]
# names(df_dat_grouped_row_by_date)[31]
# df[df_dat_grouped_row_by_date[[31]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[31]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[31]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are no ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[31]], names(day_data)[2]] + 14
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[31]], names(day_data)[3]] + 21
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[31]], names(day_data)[4]] + 14 
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[31]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[31]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[31]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[31]],names(day_data)]
# 
# 
# #  "2-3-4-5-6-7"names(df_dat_grouped_row_by_date)[32]
# names(df_dat_grouped_row_by_date)[32]
# df[df_dat_grouped_row_by_date[[32]], names(df)[grepl("date", names(df))]]
# 
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[32]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[32]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[32]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[32]], names(day_data)[3]] + 21
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[32]], names(day_data)[4]] + 14 
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[32]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[32]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[32]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[32]],names(day_data)]
# 
# 
# #  "2-3-4-5-6-7-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30""names(df_dat_grouped_row_by_date)[33]
# names(df_dat_grouped_row_by_date)[33]
# df[df_dat_grouped_row_by_date[[33]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[33]], names(day_data)[3]] + 21
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][17]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[33]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[33]],names(df)[grepl("date", names(df))]][25]
# df[df_dat_grouped_row_by_date[[33]],names(day_data)]
# 
# 
# #  "2-3-4-5-6-7-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30" names(df_dat_grouped_row_by_date)[34]
# names(df_dat_grouped_row_by_date)[34]
# df[df_dat_grouped_row_by_date[[34]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][17]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[34]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[34]],names(df)[grepl("date", names(df))]][25]
# df[df_dat_grouped_row_by_date[[34]],names(day_data)]
# 
# 
# 
# #   "2-3-4-5-6-7-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-29-30" names(df_dat_grouped_row_by_date)[35]
# names(df_dat_grouped_row_by_date)[35]
# df[df_dat_grouped_row_by_date[[35]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][17]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[35]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[35]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[35]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[35]],names(day_data)]
# 
# 
# 
# #   ""2-3-4-5-6-7-9-10-11-12-13-14-15-16-17-18-29-30" names(df_dat_grouped_row_by_date)[36]
# names(df_dat_grouped_row_by_date)[36]
# df[df_dat_grouped_row_by_date[[36]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[36]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[36]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[36]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[36]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[36]],names(df)[grepl("date", names(df))]][17]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[36]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[36]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[36]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[36]],names(day_data)]
# 
# 
# # "2-3-4-5-6-7-9-10-11-12-13-19-20-21-22-23-24-25-26-27-28-29-30" names(df_dat_grouped_row_by_date)[37]
# names(df_dat_grouped_row_by_date)[37]
# df[df_dat_grouped_row_by_date[[37]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][20] - 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][20]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[37]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[37]],names(df)[grepl("date", names(df))]][26]
# df[df_dat_grouped_row_by_date[[37]],names(day_data)]
# 
# 
# 
# # "2-3-4-5-6-7-9-10-11-12-13-29-30" names(df_dat_grouped_row_by_date)[38]
# names(df_dat_grouped_row_by_date)[38]
# df[df_dat_grouped_row_by_date[[38]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[38]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[38]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are ransplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[38]],names(df)[grepl("date", names(df))]][7]
# 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[38]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[38]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[38]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[38]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[38]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[38]],names(day_data)]
# 
# 
# 
# 
# # "2-3-4-9-10-11-12-13-14-15-16-17-18"names(df_dat_grouped_row_by_date)[39]
# names(df_dat_grouped_row_by_date)[39]
# df[df_dat_grouped_row_by_date[[39]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[39]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[39]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[39]], names(day_data)[2]] + 14
#  # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[39]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[39]],names(df)[grepl("date", names(df))]][16]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[39]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[39]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[39]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[39]],names(day_data)]
# 
# 
# 
# 
# # "2-3-4-9-10-11-12-13-14-15-16-17-18-19-20-21-22-23-24-25-26-27-28-29-30"names(df_dat_grouped_row_by_date)[40]
# names(df_dat_grouped_row_by_date)[40]
# df[df_dat_grouped_row_by_date[[40]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][2]
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][4]
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[40]], names(day_data)[2]] + 14
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][16]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[40]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[40]],names(df)[grepl("date", names(df))]][26]
# df[df_dat_grouped_row_by_date[[40]],names(day_data)]
# 
# 
# 
# # "5-6-8" names(df_dat_grouped_row_by_date)[41]
# names(df_dat_grouped_row_by_date)[41]
# df[df_dat_grouped_row_by_date[[41]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[41]],names(df)[grepl("date", names(df))]][5] -21
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[41]],names(df)[grepl("date", names(df))]][5] - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[41]],names(df)[grepl("date", names(df))]][5]
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[41]], names(day_data)[3]] + 21
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[5]] <-df[df_dat_grouped_row_by_date[[41]], names(day_data)[4]] + 14 
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[41]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[41]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[41]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[41]],names(day_data)]
# 
# 
# # 5-6-8-9-10-11-12" names(df_dat_grouped_row_by_date)[42]
# names(df_dat_grouped_row_by_date)[42]
# df[df_dat_grouped_row_by_date[[42]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[42]],names(df)[grepl("date", names(df))]][5] -21
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[42]],names(df)[grepl("date", names(df))]][5] - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[42]],names(df)[grepl("date", names(df))]][5]
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[42]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[5]] <-df[df_dat_grouped_row_by_date[[42]], names(day_data)[4]] + 14 
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[42]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[42]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[42]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[42]],names(day_data)]
# 
# 
# #"5-6-8-9-10-11-12-14-15-16-17"names(df_dat_grouped_row_by_date)[43]
# names(df_dat_grouped_row_by_date)[43]
# df[df_dat_grouped_row_by_date[[43]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[43]],names(df)[grepl("date", names(df))]][5] -21
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[43]],names(df)[grepl("date", names(df))]][5] - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[43]],names(df)[grepl("date", names(df))]][5]
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[43]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[5]] <-df[df_dat_grouped_row_by_date[[43]],names(df)[grepl("date", names(df))]][15]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[43]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[43]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[43]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[43]],names(day_data)]
# 
# 
# 
# #"5-6-8-9-10-11-12-14-15-16-17-19-20-21-22""names(df_dat_grouped_row_by_date)[44]
# names(df_dat_grouped_row_by_date)[44]
# df[df_dat_grouped_row_by_date[[44]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[1]] <- df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][5] -21
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[2]] <- df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][5] - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][5]
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[5]] <-df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][15]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[44]],names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[44]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[44]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[44]],names(day_data)]
# 
# 
# 
# #" "9-10-11-12"""names(df_dat_grouped_row_by_date)[45]
# names(df_dat_grouped_row_by_date)[45]
# df[df_dat_grouped_row_by_date[[45]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[1]] <-  df[df_dat_grouped_row_by_date[[45]],names(df)[grepl("date", names(df))]][11] -21 - 14 - 7
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[2]] <-  df[df_dat_grouped_row_by_date[[45]],names(df)[grepl("date", names(df))]][11] -21 - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[45]],names(df)[grepl("date", names(df))]][11] - 21 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[45]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[45]], names(day_data)[4]] + 14
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[45]], names(day_data)[5]] + 14
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[45]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[45]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[45]],names(day_data)]
# 
# 
# 
# #" "9-10-11-12-14-15-16-17-19-20-21-22"""names(df_dat_grouped_row_by_date)[46]
# names(df_dat_grouped_row_by_date)[46]
# df[df_dat_grouped_row_by_date[[46]], names(df)[grepl("date", names(df))]]
# 
# # set planting day but this case they have planting day.
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[1]] <-  df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][11] -21 - 14 - 7
# 
# # set nursery day - 1 week after registration.  
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[2]] <-  df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][11] -21 - 14
# 
# # transplanting day - 2 weeks after nursery. There are no transplanting_transplantingdate
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[3]] <- df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][11] - 21 
# # firstharvest day  -  3 weeks after transplanting. There are no firstharvest_submitted_date
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[4]] <- df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][11]
# 
# # secondharvest day - 2 weeks after the first harvest. There are  secondharvest_submitted_date
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[5]] <- df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][16]
# # thirdharvest day - 2 weeks after the second harvest. There are  thirdharvest_submitted_date
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[6]] <- df[df_dat_grouped_row_by_date[[46]],names(df)[grepl("date", names(df))]][21]
# # fourthharvest day - 2 weeks after the third harvest. There are  fourtharvest_submitted_date
# df[df_dat_grouped_row_by_date[[46]], names(day_data)[7]] <- df[df_dat_grouped_row_by_date[[46]], names(day_data)[6]] + 14
# df[df_dat_grouped_row_by_date[[46]],names(day_data)]
# 
# 
# #### Done to adjust tricot evaluation day data
# write.table(df,file="data/New_Amaranth_tricot_time_date_adjusted.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
# # 
# # # check adjusting data was correct.
# # # few date overlapped...It is orignial data problem. Delete these date 
# # 
# # a0<- which(df[,names(day_data)][1] == df[,names(day_data)][2])
# # a1<-which(df[,names(day_data)][1] == df[,names(day_data)][3])
# # a2<-which(df[,names(day_data)][1] == df[,names(day_data)][4])
# # a3<-which(df[,names(day_data)][1] == df[,names(day_data)][5])
# # # which(df[,names(day_data)][1] == df[,names(day_data)][6])
# # # which(df[,names(day_data)][1] == df[,names(day_data)][7])
# # 
# # a4<-which(df[,names(day_data)][2] == df[,names(day_data)][3])
# # a5<- which(df[,names(day_data)][2] == df[,names(day_data)][4])
# # a6<-which(df[,names(day_data)][2] == df[,names(day_data)][5])
# # # which(df[,names(day_data)][2] == df[,names(day_data)][6])
# # # which(df[,names(day_data)][2] == df[,names(day_data)][7])
# # 
# # a7<- which(df[,names(day_data)][3] == df[,names(day_data)][4])
# # a8<- which(df[,names(day_data)][3] == df[,names(day_data)][5])
# # a9<- which(df[,names(day_data)][3] == df[,names(day_data)][6])
# # # which(df[,names(day_data)][3] == df[,names(day_data)][7])
# # 
# # a10<- which(df[,names(day_data)][4] == df[,names(day_data)][5])
# # a11<-which(df[,names(day_data)][4] == df[,names(day_data)][6])
# # # which(df[,names(day_data)][4] == df[,names(day_data)][7])
# # 
# # a12<-which(df[,names(day_data)][5] == df[,names(day_data)][6])
# # # which(df[,names(day_data)][5] == df[,names(day_data)][7])
# # 
# # a13<- which(df[,names(day_data)][6] == df[,names(day_data)][7])
# 
# # 
# # exclude_row<- c(a0, a1, a2, a3, a4, a5, a6 , a7, a8, a9, a10, a11, a12, a13)
# # df<- df[-c(unique(exclude_row)),]
# # 
# # 
# # #### Done to adjust tricot evaluation day data
# # write.table(df,file="data/New_Amaranth_tricot_time_date_adjusted.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
