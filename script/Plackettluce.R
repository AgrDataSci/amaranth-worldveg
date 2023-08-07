### load packages
library("PlackettLuce")
library("gosset")
library("climatrends")


# call csv data to make data.frame
# df<- read.csv("data/Amaranth_tricot_marged_version.csv")
df<- read.csv("data/Amaranth_tricot_marged_version_deleted_emptycol.csv")

#there are "Not observed" data. lets change "Not observed" to NA
df[df == "Not observed"] <- NA

# package varieties 

colnames(df)[80] <- "package_item_A"
colnames(df)[81] <- "package_item_B"
colnames(df)[82] <- "package_item_C"
variety <-  c("package_item_A", "package_item_B" ,"package_item_C")

# delete unnecessary columns like names and telephone 
df<- df[,-c(which(names(df) == "registration_telephone"))]  # delete participant telephone
df<- df[,-c(which(names(df) == "registration_participant_name"))]  # delete participant name
df<- df[,-c(which(names(df) == "registration_clc_after"))]
df<- df[,-c(which(names(df) == "registration_clc_before"))]
df<- df[,-c(which(names(df) == "registration_prodpract"))]


# find columns that have french respond  
loop_result <- c()
for( i in seq_along(df)){loop_result[i]<- "Oui" %in%  attributes(table(df[,i]))$dimnames[[1]]}

# deduct 24 and 29 in loop_result because these column were described as French!  
f_to_e_column<- names(df)[which(loop_result)]
f_to_e_column<- f_to_e_column[-which(f_to_e_column == "socioeconomic_noprodwhy")]
f_to_e_column<- f_to_e_column[-which(f_to_e_column == "socioeconomic_prodpract_oth")]


# lets change  Oui to Yes , Mon to no
for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("Oui", "Yes",df[[f_to_e_column[i]]])}
for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("Non", "No",df[[f_to_e_column[i]]])}
for (i in seq_along(f_to_e_column)){df[[f_to_e_column[i]]]<- gsub("No Applicable", "Not Applicable",df[[f_to_e_column[i]]])}

table(df[[f_to_e_column[12]]])
df[,f_to_e_column[12]]<- gsub("1 mois", "1 month", df[,f_to_e_column[12]])
df[,f_to_e_column[12]]<- gsub("Ouï", "Yes", df[,f_to_e_column[12]])


# lets change Id first with "package_project_name"
temp_id <- c()
for (i in 1:nrow(df)){ 
temp_id[i] <- strsplit(df[,"package_project_name"], "-")[[i]][1]}

df[["id"]]<- paste(df[["id"]], temp_id, sep ="_")
# check overlapped id
table(table(df[["id"]]))


# delete other Id column 
delete_id_columne<- names(df)[grep("id", names(df))][2:19]
delete_id_columne

for (i in seq_along(delete_id_columne)) {
  df <- df[,-c(which(names(df) == delete_id_columne[i]))]}

delete_names_from_all <- c("coordinator","_country", "name", "package_technology", "submitted_by", "deviceimei", "clc_after", "socioeconomic_telephone")

temp_list <- list()
for (i in seq_along(delete_names_from_all)) {
  temp_list[[i]] <- grep(delete_names_from_all[[i]], names(df))}

temp_list<- unlist(temp_list)
temp_list

df<-df[,-c(temp_list)]

###############################################################################
#################organize date, try to fill NA column of date##################
###################its necessary for climate covariate#########################

date_names<- names(df[,grep("date", names(df))])

date_names
table(df[,date_names[1]])
table(df[,date_names[2]])

# need to check date data circumstances
df[1,date_names]
df[2,date_names]
df[3,date_names]


################################################################################
# tricot traits 
trait_neg<- names(df[,grep("neg", names(df))])
trait_pos<- names(df[,grep("pos", names(df))])

traits_list <- list()
for (i in seq_along(trait_neg)){
  traits_list[[i]] <- c(trait_neg[i], trait_pos[i])
}
# allocate trait name into trait list
splittedname<- strsplit(trait_neg, "_")

for ( i in seq_along(splittedname))
names(traits_list)[[i]]<- paste0(splittedname[[i]][1],"_", splittedname[[i]][2])

rank_tricot(data = df, 
            items = variety,
            input =c(unlist(traits_list[1])),
            validation.rankings = TRUE)

# list of ground nut ranking
R <- vector(mode = "list", length = length(traits_list))

for (i in seq_along(traits_list)) {
  
  R[[i]] <-rank_tricot(data = df, 
                items = variety,
                input =c(unlist(traits_list[i])),
                validation.rankings = TRUE)}
  

mod = lapply(R, PlackettLuce)
worth_map(mod, labels = names(traits_list)) 


png(filename = "output/worthmap_amaranth.png", width = 30, height = 30, units = "cm", res = 400)
worth_map(mod, labels = names(traits_list)) 
dev.off()
################################################################################
################################other approach################################
d_i<- list()
for(i in 1:length(traits_list)){
  d_i_t <- cbind(df[, c("package_item_A", "package_item_B", "package_item_C")], df[, c(traits_list[[i]])])
  # remove all A = B =C case 
  d_i_t <- d_i_t[-which(is.na(d_i_t[,4]) & is.na(d_i_t[,5])),]
  d_i[[i]]  <- d_i_t
}
d_i

# lapply rank_tricot
R_list_keep<- lapply(d_i, function(x)  rank_tricot(data = x, 
                                     items = names(x[,c(1:3)]),
                                     input = names(x[,c(4:5)]),
                                     validation.rankings = TRUE))

# lapply PlackettLuce
mod_list_keep <- lapply(R, PlackettLuce)
worth_map(mod_list_keep, labels = names(traits_list)) 

# make worth 
png(filename = "output/heatmap_amaranth_no_equalcase.png", width = 30, height = 30, units = "cm", res = 500)
worth_map(mod_list_keep, labels = names(traits_list)) 
dev.off()



################################################################################
##########################social covariate######################################
# socioeconmic and registration share common information..
# lets see.
names(df)
names(df)[grep("socio", names(df))]
social_names<- names(df)[grep("socio", names(df))]
registration_names<- names(df)[grep("registration", names(df))]


df[,"socioeconomic_telephone"]
strsplit(social_names, "_")


registration_names

# subset data frame of listed socieconomic 
# socioeconomic_age
# socioeconomic_district
# socioeconomic_educationyears
# socioeconomic_irrigationshare
# socioeconomic_landown
# socioeconomic_landrights
# socioeconomic_gender

# check socioeconomy state
table(df[,c("socioeconomic_age","socioeconomic_district","socioeconomic_educationyears",
  "socioeconomic_irrigationshare","socioeconomic_landown","socioeconomic_landrights","socioeconomic_gender")][1])
#
table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_district"])))
table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_educationyears"])))
table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_irrigationshare"])))
# above three share row that do not have data 

table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_landown"])))
table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_landrights"])))
table(which(is.na(df[,"socioeconomic_age"])) == which(is.na(df[,"socioeconomic_gender"])))
# they are different.

# I think it is not possible to forward selection approach... 
# then lets put variable into PLT. one by one.

# for example, socioeconomic_age have different formet.. so let's unify as one formet 

names(table(df[,"socioeconomic_age"]))
wrongformet_age<- c("1967" ,   "1969"  ,  "1970" ,   "1975"  ,  "1980" ,   "1984"  ,  "1985"   , "1986"    ,"1987" ,   "1988"  ,  "1990" ,   "1992",    "1993" ,  
"1995"  ,  "1996" ,   "1997"  ,  "1999" ,   "2000"  ,  "8941067")


names(df)[grep("date", names(df))]

df[c(which(df[,"socioeconomic_age"]== wrongformet_age[1])),"socioeconomic_age"] 
df[c(which(df[,"socioeconomic_age"]== wrongformet_age[1])),] 

corrected_name<- as.numeric(strsplit(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[1])),"registration_survey_end"], "-")[[1]][1]) - as.numeric(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[1])),"socioeconomic_age"])  + 1
corrected_name<- as.numeric(strsplit(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[5])),"registration_survey_end"], "-")[[1]][1]) - as.numeric(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[5])),"socioeconomic_age"])  + 1
corrected_name

df[c(which(df[,"socioeconomic_age"]== wrongformet_age[2])),"socioeconomic_age"]
df[c(which(df[,"socioeconomic_age"]== wrongformet_age[3])),"socioeconomic_age"]
df[c(which(df[,"socioeconomic_age"]== wrongformet_age[4])),"socioeconomic_age"]
df[c(which(df[,"socioeconomic_age"]== wrongformet_age[5])),"socioeconomic_age"]
df[c(which(df[,"socioeconomic_age"]== wrongformet_age[6])),"socioeconomic_age"]

# unify age as one formet except last wrongformet_age "8941067"
for( i in 1:18){
  corrected_name<- (as.numeric(strsplit(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[i])),"registration_survey_end"], "-")[[1]][1]) - as.numeric(df[c(which(df[,"socioeconomic_age"]== wrongformet_age[i])),"socioeconomic_age"])  + 1)
  df[c(which(df[,"socioeconomic_age"]== wrongformet_age[i])),"socioeconomic_age"] <- corrected_name}

# change age 8941067... as NA....   
df[which(df[,"socioeconomic_age"] == wrongformet_age[19]),"socioeconomic_age"] <- NA

# district 
table(df[,"socioeconomic_district"])

# gender
table(df[,"socioeconomic_gender"])


social_names<- names(df)[grep("socio", names(df))]



# example.... socioeconomic_age only

d_i<- list()
for(i in 1:length(traits_list)){
  d_i_t <- cbind(df[, c("package_item_A", "package_item_B", "package_item_C")], df[, c(traits_list[[i]], "socioeconomic_age")])
  # remove all A = B =C case 
  d_i_t <- d_i_t[-which(is.na(d_i_t[,4]) & is.na(d_i_t[,5])),]
  d_i_t[-which(is.na(d_i_t[,4]) & is.na(d_i_t[,5])),]
  d_i_t<- d_i_t[-which(is.na(d_i_t[,6])),]
  d_i[[i]]  <- d_i_t
  }
d_i

names(d_i) <- names(traits_list)
# check d_i 
str(d_i)

# Remove the empty data frames from the list
filtered_list <- Filter(function(df) is.data.frame(df) && nrow(df) > 0, d_i)
filtered_list

str(filtered_list)


tree <- list()
for( i in seq_along(filtered_list)){
# use grop = true.
G <-  rank_tricot(filtered_list[[i]],   
                  items = 1:3, input = 4:5,
                  group = TRUE,
                  validate.rankings = T)

pld<- cbind(as.data.frame(G), filtered_list[[i]][,6])

names(pld) <- c("G", "socioeconomic_age")

tree[[i]]<- pltree(G ~ socioeconomic_age,
               data = pld,
               minsize = 25, 
               alpha = 0.1, 
               npseudo = 0.8,
               gamma = TRUE,
               bonferroni = TRUE)}

names(tree) <- names(filtered_list)

plot(tree[[1]])
plot(tree[[2]])
plot(tree[[3]])
plot(tree[[4]])
plot(tree[[5]])
plot(tree[[6]])
plot(tree[[7]])
plot(tree[[8]])
plot(tree[[9]])
plot(tree[[10]])
plot(tree[[11]])
plot(tree[[12]])
names(tree[12])



################################################################################
G<- rankTricot(data = df, 
           items = variety,
           input = c(unlist(traits_list[1])),
           validation.rankings = TRUE,
           group = TRUE)


covar<- df_noNA[,c("socioeconomic_age", "socioeconomic_district", "socioeconomic_educationyears")]
pld <- cbind(G,covar)
#cut data set that have NA
which(!is.na(pld$socioeconomic_age))
pld <- pld[which(!is.na(pld$socioeconomic_age)),]

#


# set the baseline as a deviance from a Plackett-Luce model without covariates
baseline <- deviance(PlackettLuce(G))
# vector to keep best explanatory variables
covar_keep <- character(0L)
# keep running if TRUE
best <- TRUE
# number of runs
counter <- 1
# a list to keep the goodness-of-fit coefficients from each step
modelcoeffs <- list()
# get the names of explanatory and response variables
covarnames <- names(covar)
siglevel <- 0.1
minsize <- 25
npseudo <- 0.8
bonferroni <- TRUE
gamma <- TRUE

# keep running until the model get its best performance
while(best){
  
  message("Run " , counter, "\n")
  
  fs <- length(covarnames)
  
  args <- list(formula = NULL,
               data = pld,
               alpha = siglevel,
               npseudo = npseudo,
               bonferroni = bonferroni,
               gamma = gamma,
               minsize = minsize)
  
  deviances <- numeric()
  
  for(i in seq_len(fs)) {
    f_i <- formula(paste0("G ~ ", paste(c(covar_keep, covarnames[i]), collapse = " + ")))
    args$formula <- f_i
    pl_i <- do.call("pltree", args)
    deviances <- c(deviances, deviance(pl_i))
  }
  
  best_dv <- which.min(deviances)
  
  value_best <- min(deviances)
  
  # take the name of best variable
  best_model <- covarnames[best_dv]
  
  # compare if the best value is better than the baseline
  best <- value_best < baseline
  
  # refresh baseline
  baseline <- value_best
  
  # model calls to add into list of parameters
  call_m <- paste0("G ~ ", paste(paste(covar_keep, collapse = " "), covarnames))
  call_m <- data.frame(cbind(call = call_m, deviance = deviances))
  call_m[2:ncol(call_m)] <- lapply(call_m[2:ncol(call_m)], as.numeric)
  
  # take outputs from this run and add it to the list of parameters
  modelcoeffs[[counter]] <- call_m
  
  if(best){
    
    # remove best variable for the next run
    covarnames <- covarnames[!covarnames %in% best_model]
    
    # keep this model for the next run
    covar_keep <- c(covar_keep, best_model)
    
    message("Best model found: ",
            paste0("G ~ ", paste(covar_keep, collapse = " + ")), "\n")
    
  }
  
  # update counter (number of runs in 'while')
  counter <- counter + 1
  
  # prevent while loop to broke if the model fits with all variables
  if (length(covarnames) == 0) {
    best <- FALSE
  }
  
}


covar_keep

f <- formula(paste0("G ~ ", paste(covar_keep, collapse = " + ")))

f

tree <- pltree(f, 
               data = pld, 
               minsize = minsize, 
               alpha = 0.1, 
               npseudo = npseudo,
               gamma = TRUE,
               bonferroni = bonferroni)

tree

plot(tree)
#
#
#
#
#
#
#
#
#

table(df[["socioeconomic_extensionservice"]])

################################################################################
########################## get download climate################################# 
# check GPS coordinate  
# latitude and logitude name
names(df)[grep("latitude", names(df))]
names(df)[grep("longitude", names(df))]

# lets assume all coordinate are same... 
# even though row data that represent  one farmer's info  have several different GPS location info, 
# just merge them as one.  

coalesce_list <- lapply(grep("latitude", names(df)), function(i) df[, i])
latitude_result <- do.call(coalesce, coalesce_list)

coalesce_list <- lapply(grep("longitude", names(df)), function(i) df[, i])
longitude_result <- do.call(coalesce, coalesce_list)

# combine df, latitude_result and longitude_result
result<- cbind(latitude_result, longitude_result)
df<- cbind(df, result)

# download climate data 
names(df)[grep("date", names(df))]

# get number of rows that have planting date
rw1<- which(!is.na(df[,"registration_submitted_date"]))
rw2<-which(!is.na(df[,"nursery_plantingdate"]))
rw3<-which(!is.na(df[,"nursery_submitted_date"]))
rw4<-which(!is.na(df[,"nursery_assessmentdate"]))

# bind vector. If there are overlapped 3 times, they have different date. 
rw <- c(rw1, rw2, rw3, rw4)
table(rw)[table(rw)  ==2] # no 2 
rw_c<- c(names(table(rw)[table(rw) == 1]), names(table(rw)[table(rw) == 3]))
# if the table shows 1 or other numbers,  1 means the row has data of registration_submitted_date  
#and 3 means the row has data of nursery_plantingdate, nursery_submitted_date, and nursery_assessmentdate
table(rw_c)

#check  those data frame..... 
df[c(names(table(rw)[table(rw) == 3])),c("registration_submitted_date", "nursery_plantingdate", "nursery_submitted_date", "nursery_assessmentdate")]
df[c(names(table(rw)[table(rw) == 1])),c("registration_submitted_date", "nursery_plantingdate", "nursery_submitted_date", "nursery_assessmentdate")]

coalesce(df[,"registration_submitted_date"], df[,"nursery_plantingdate"])

# first harvest 
table(df[,c("firstharvest_submitted_date", "firstharvest_harvestingdate_a", "firstharvest_harvestingdate_b", 
      "firstharvest_harvestingdate_c", "firstharvest_assessmentdate")][1])


table(df[,c("firstharvest_submitted_date", "firstharvest_harvestingdate_a", "firstharvest_harvestingdate_b", 
            "firstharvest_harvestingdate_c", "firstharvest_assessmentdate")][2])
table(df[,c("firstharvest_submitted_date", "firstharvest_harvestingdate_a", "firstharvest_harvestingdate_b", 
            "firstharvest_harvestingdate_c", "firstharvest_assessmentdate")][3])



