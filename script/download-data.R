# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")
library("gosset")
library("janitor")

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
