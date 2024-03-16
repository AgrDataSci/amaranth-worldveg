# .......................................
# .......................................
library("cluster")
library("gosset")
library("PlackettLuce")
library("summarytools")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")

dat = read.csv("data/amaranth-tricot-data.csv")

# .......................................
# .......................................
# work with some covariates ####
covar = read.csv("data/summaries/covar-available.csv")
covar = covar[covar$sel == "x", ]

covar = dat[covar$covar]

names(covar) = gsub("registration_", "", names(covar))

covar$country = dat$package_country

pdf("output/boxplot-farmer-age.pdf",
    width = 12,
    height = 8)
capture.output()
boxplot(covar$age ~ covar$country:covar$gender,
        xlab = "Country x Gender",
        ylab = "Age")
dev.off()

pdf("output/boxplot-farmer-experience.pdf",
    width = 12,
    height = 8)
capture.output()
boxplot(covar$experiencecrop ~ covar$country:covar$gender,
        xlab = "Country x Gender",
        ylab = "Experience growing amaranth (years)")
dev.off()

pdf("output/boxplot-distance-to-market.pdf",
    width = 12,
    height = 8)
capture.output()
boxplot(covar$marketdistance ~ covar$country:covar$gender,
        xlab = "Country x Gender",
        ylab = "Distance to market (km)")
dev.off()

pdf("output/boxplot-income-share-from-hh-amaranth-sales.pdf",
    width = 12,
    height = 8)
capture.output()
boxplot(covar$incomecropshare~ covar$country:covar$gender,
        xlab = "Country x Gender",
        ylab = "HH income share from amaranth sale (%)")
dev.off()

#prop.table(table(covar$country, covar$occupation))
#prop.table(table(covar$country, covar$typeofhh))

names(covar)

# identify the characters 
chars = which(unlist(lapply(covar[1:15], is.character)))

for (i in seq_along(chars)) {
  
  x = ctable(covar$country, covar[,chars[i]], useNA = "no", chisq = TRUE)
  
  y = stby(list(x = covar$country, 
                y = covar[,chars[i]]),
           INDICES = covar$gender, 
           FUN = ctable,    
           useNA = "no", 
           chisq = TRUE)
  
  capture.output(c(x, y), 
                 file = paste0("output/descriptive-statistics-covariates-", 
                               names(chars[i]),".txt"))
  
}


covar[chars] = lapply(covar[chars], function(x){
  ifelse(is.na(x), "Unknown", x)
})


# input values 
numbers = which(unlist(lapply(covar[1:15], is.numeric)))

category = paste0(covar$gender, covar$country)
unicats = unique(category)

for (i in seq_along(numbers)) {
  
  for (j in seq_along(unicats)) {
    
    covar[, numbers[i]] = ifelse(is.na(covar[, numbers[i]]) & 
                                   category == unicats[j],
                                 median(covar[category == unicats[j], numbers[i]], na.rm = TRUE),
                                 covar[, numbers[i]])
    
    covar[, numbers[i]] = as.integer(covar[, numbers[i]])
    
    
  }
}

boxplot(covar[,numbers[1]])

boxplot(covar[,numbers[2]])

hist(covar[,numbers[3]])

boxplot(covar[,numbers[4]])

quant = covar[numbers]

chars

groups = covar[c('gender', 'whocontrolsell')]

head(groups)

str(groups)

groups[1:ncol(groups)] = lapply(groups[1:ncol(groups)], as.factor)

distance_groups = daisy(groups)

clust1 = cutree(hclust(distance_groups), 2)

# do it for the numeric
quant = covar[numbers]
quant = quant[,c("age", "incomecropshare", "experiencecrop")]
quant = scale(quant)

distance = dist(quant)

clust = hclust(distance)

clust2 = cutree(clust, 3)

plot(clust)

clust_dat = data.frame(c1 = clust1, c2 = clust2)

clust_dat[1:ncol(clust_dat)] = lapply(clust_dat[1:ncol(clust_dat)], as.factor)

dists = daisy(clust_dat)

clust3 = hclust(dists)

clust3 = cutree(clust3, 4)

table(clust3)

covar$Cluster = clust3

covar$Cluster[covar$Cluster == 4] = 3

table(covar$Cluster)

covar_clust = covar[c("Cluster", 'gender', 'whocontrolsell', "age", 
                      "incomecropshare", "experiencecrop")]

# .....................................
# .....................................
# again, trying to split cluster 1

chars
groups = covar[covar_clust$Cluster == 1, c('gender', 'whocontrolsell', 'whocontrolprod')]

head(groups)

str(groups)

groups[1:ncol(groups)] = lapply(groups[1:ncol(groups)], as.factor)

distance_groups = daisy(groups)

clust1 = hclust(distance_groups)

plot(clust1)

clust1 = cutree(clust1, 6)

table(clust1)

# do it for the numeric
quant = covar[covar_clust$Cluster == 1,c("age", "incomecropshare", "experiencecrop")]
quant = scale(quant)

distance = dist(quant)

clust = hclust(distance)
plot(clust)
clust2 = cutree(clust, 6)

table(clust2)

clust_dat = data.frame(c1 = clust1, c2 = clust2)

clust_dat[1:ncol(clust_dat)] = lapply(clust_dat[1:ncol(clust_dat)], as.factor)

dists = daisy(clust_dat)

clust3 = hclust(dists)

plot(clust3)

clust3 = cutree(clust3, 5)

table(clust3)

clust3[clust3 > 2] = 2

clust3[clust3 > 1] = 4

covar_clust = covar[c("Cluster", 'country', 'age', 'gender', 'whocontrolsell', 'whocontrolprod', 
                      "incomecropshare", "experiencecrop")]

table(covar_clust$Cluster)

covar_clust[covar_clust$Cluster == 1, "Cluster"] = clust3

table(covar_clust$Cluster)

chars = which(unlist(lapply(covar_clust[1:ncol(covar_clust)], is.character)))

covar_clust$Cluster = as.factor(covar_clust$Cluster)

boxplot(covar$age ~ covar_clust$Cluster)

boxplot(covar$experiencecrop ~ covar_clust$Cluster)

boxplot(covar$incomecropshare ~ covar_clust$Cluster)

summary(lm(incomecropshare ~ Cluster, data = covar_clust))

summary(lm(experiencecrop ~ Cluster, data = covar_clust))

summary(lm(age ~ Cluster, data = covar_clust))

table(covar_clust$Cluster, covar_clust$country)

table(covar_clust$Cluster, covar_clust$whocontrolsell)

table(covar_clust$Cluster, covar_clust$whocontrolprod)

# ..............................
# ..............................
# labels to clusters from chatGPT
clust_labs = c("Empowered Women Farmers",
               "Experienced Men Farmers",
               "Rising Men Entrepreneurs",
               "Innovative Women Farmers")

covar_clust$clust = covar_clust$Cluster

covar_clust$Cluster = factor(covar_clust$clust, labels = clust_labs)

covar_clust$Cluster = factor(covar_clust$Cluster, levels = c("Empowered Women Farmers",
                                                             "Innovative Women Farmers",
                                                             "Experienced Men Farmers",
                                                             "Rising Men Entrepreneurs"))

table(covar_clust$Cluster)

x = cbind(dat[c("id", "package_project_name")], covar_clust)

write.csv(x, "data/covariates-cluster.csv", row.names = FALSE)
