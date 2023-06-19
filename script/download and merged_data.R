# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")
library("dplyr")


# read file with API key,
key = "1879b555-d5a3-4b6a-905c-bdc087c8021d"
server = "1000farms"

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

# fix some codes that diverged over time
# write more code to fix if needed
dat = lapply(dat, function(x){
  names(x)[names(x) == "performanceagronomique1_"] = "firstharvest_"
  names(x)[names(x) == "premiererecolte_"] = "firstharvest_"
  names(x)[names(x) == "pepiniere_"] = "nursery_"
  names(x)[names(x) == "registration_gender1"] = "registration_gender"
  names(x)[names(x) == "registration_biogender"] = "registration_gender"
  names(x)[names(x) == "registration_bioage"] = "registration_age"
  names(x)[names(x) == "registration_kebele"] = "registration_village"
  names(x)[names(x) == "registration_location"] = "registration_village"
  names(x) = gsub("overallchar2", "overall", names(x))
  x
})

# get unique col names across the data.frames
uniquenames = lapply(dat, function(x){
  names(x)
})

uniquenames = unique(unlist(uniquenames))

uniquenames

# put the data together 
dat = rowbind(dat)

# save dat as csv file. 
write.table(dat,file="data/Amaranth_tricot.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
# keep dat for preserving the original data 
keep_dat <- dat


#################################################################################
## find similar name to integrate!! 
dat_names<- names(keep_dat)

# split names and getter first word from names

first_word<- c()
for (i in seq_along(dat_names)){
  first_word[i] <- strsplit(dat_names, "_")[[i]][1]}

refinded_names<- unique(first_word)

# names we should take
refinded_names
# [1] "id"                      "package"                 "registration"            "troisiemerecolte"        "transplantation"        
# [6] "socioeconomic"           "quatriemerecolte"        "premiererecolte"         "deuxiemerecolte"         "transplanting"          
# [11] "thirdharvest"            "socio"                   "secondharvest"           "reproductive"            "nursery"                
# [16] "fourthharvest"           "firstharvest"            "performanceagronomique3" "performanceagronomique2" "performanceagronomique1"
# [21] "pepiniere"               "floraison"(reproductive)  

# "id", "package", "registration", "troisiemerecolte"(thirdharvest), "transplantation", ,"thirdharvest", "socioeconomic",
#  "quatriemerecolte"(fourthharvest), "premiererecolte" (firstharvest), "deuxiemerecolte"(secondharvest), "transplanting"
# "thirdharvest", "socio"(socioeconomic),  "secondharvest"  , "reproductive", "nursery", "fourthharvest"     
# "firstharvest", "performanceagronomique3"(thirdharvest) , "performanceagronomique2(secondharvest)" "performanceagronomique1"(firstharvest)
#  "pepiniere"(nursery),  "floraison"(reproductive) 

# change french name to english!!  3
# firstharvest = premiererecolte = performanceagronomique1
# premiererecolte -> firstharvest
# performanceagronomique1 -> firstharvest
changed_names<- gsub("premiererecolte", "firstharvest", dat_names)
changed_names<- gsub("performanceagronomique1", "firstharvest", changed_names)

# change french name to english!!  3 
# secondharvest = performanceagronomique2 = deuxiemerecolte
changed_names<- gsub("deuxiemerecolte", "secondharvest", changed_names)
changed_names<- gsub("performanceagronomique2", "secondharvest", changed_names)

# change french name to english!!  3 
# thirdharvest = performanceagronomique3 = troisiemerecolte
changed_names<- gsub("troisiemerecolte", "thirdharvest", changed_names)
changed_names<- gsub("performanceagronomique3", "thirdharvest", changed_names)

# change french name to english!! 2 
# fourthharvest =  quatriemerecolte
changed_names<- gsub("quatriemerecolte", "fourthharvest", changed_names)

# change french name to english!!    
# transplanting =  transplantation 2 
changed_names<- gsub("transplantation", "transplanting", changed_names)

# change french name to english!!    
# transplanting =  transplantation
changed_names<- gsub("transplantation", "transplanting", changed_names)

# change french name to english!!    
# nursery =  pepiniere
changed_names<- gsub("pepiniere", "nursery", changed_names)

# change french name to english!!    
# reproductive =  floraison
changed_names<- gsub("floraison", "reproductive", changed_names)

# change french name to english!!    
# socio_economic =  socioeconomic
changed_names<- gsub("socio_economic", "socioeconomic", changed_names)

# remove "fr" ,additional of FR, which record french . 
keep_vector_number<- grep("fr.*$", changed_names)

changed_names[keep_vector_number] <- gsub("fr","",  changed_names[keep_vector_number])

#check the result 
grep("fr.*$", changed_names)

# check overlapped names
table(changed_names)
# 3 times overlap in all names
overlapped3 <- table(changed_names)[table(changed_names) == 3]
overlapped3<- names(overlapped3)
# 2 times overlap in all names
overlapped2 <- table(changed_names)[table(changed_names) == 2]
overlapped2<- names(overlapped2)
# 1 times overlap in all names
overlapped1<- table(changed_names)[table(changed_names) == 1]
overlapped1<- names(overlapped1)

# find names vector numbers(thoese are same sequence dataframe column number ) 
# and try to merge overlapped variables as one 
# three times overlap
new_name_overlapp_threetimes<- list()
keep_temp<-list()
for(i in seq_along(overlapped3)){
 for(j in 1:3){
   keep_temp[j]   <- keep_dat[,c(which(changed_names == overlapped3[i]))][j]}
  new_name_overlapp_threetimes[[i]]<-  coalesce(keep_temp[[1]],keep_temp[[2]],keep_temp[[3]])
names(new_name_overlapp_threetimes)[i] <- overlapped3[i]
  } 
new_name_overlapp_threetimes

# two times overlapped 
new_name_overlapp_secondtimes<- list()
keep_temp<-list()
for(i in seq_along(overlapped2)){
  for(j in 1:2){
    keep_temp[j]   <- keep_dat[,c(which(changed_names == overlapped2[i]))][j]}
  new_name_overlapp_secondtimes[[i]]<-  coalesce(keep_temp[[1]],keep_temp[[2]])
  names(new_name_overlapp_secondtimes)[i] <- overlapped2[i]
} 
new_name_overlapp_secondtimes


# no overlapped variables names 

new_name_no_overlapp<- list()
for (i in seq_along(overlapped1)){new_name_no_overlapp[[i]] <- keep_dat[,c(which(changed_names == overlapped1[i]))]
names(new_name_no_overlapp)[i] <- overlapped1[i]
}

# changed from list to dataframe
new_name_overlapp_threetimes<-as.data.frame(new_name_overlapp_threetimes)
new_name_overlapp_secondtimes<-as.data.frame(new_name_overlapp_secondtimes)
new_name_no_overlapp<-as.data.frame(new_name_no_overlapp)
# combine
a<- cbind(new_name_overlapp_threetimes, new_name_overlapp_secondtimes)
new_dat<- cbind(a, new_name_no_overlapp)
# now marged data frame
new_dat

write.table(new_dat,file="data/New_Amaranth_tricot.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

# but we need to sort name as sequence
names(new_dat)
sort_name_lisy<- strsplit(names(new_dat), "_")

t_vector<-c()
for(i in seq_along(sort_name_lisy)){
  t_vector[i]  <- sort_name_lisy[[i]][1]}
variables_names_seq<- unique(t_vector)
variables_names_seq1 <- c("id", "registration", "package", "nursery", "reproductive", "transplanting","firstharvest" , "secondharvest", "thirdharvest" ,  "fourthharvest",  "socioeconomic")
variables_names_seq %in% variables_names_seq
variables_names_seq1 %in% variables_names_seq1
variables_names_seq <- variables_names_seq1

vns<- variables_names_seq[-1] # except id becuse there are some variables that have id in their names

result <- lapply(vns, function(v) names(new_dat[, grep(v, names(new_dat))]))
new_name_seq <- c(variables_names_seq[1], result)

unlist(new_name_seq)

# here is marged version
new_date_frame<- new_dat[, c(unlist(new_name_seq))]
write.table(new_date_frame,file="data/Amaranth_tricot_marged_version.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

# here is marged version but also I cleaned variables all NA (empty)
new_date_frame_ver2<- new_date_frame[,colSums(is.na(new_date_frame)) != nrow(new_date_frame)]
write.table(new_date_frame_ver2,file="data/Amaranth_tricot_marged_version_deleted_emptycol.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

