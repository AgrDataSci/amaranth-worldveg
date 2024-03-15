# .......................................
# .......................................

library("cluster")
library("tidyverse")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("summarytools")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")

dat = read.csv("data/amaranth-tricot-data.csv")

# get the list of traits
trait_list = getTraitList(dat, pattern = c("_pos", "_neg"))

traits = lapply(trait_list, function(x){
  x$trait_label
})

traits = unlist(traits)

traitlabels = fix_trait_labels(traits)

traitlabels

trait_list = getTraitList(dat,
                          pattern = c("_pos", "_neg"),
                          trait.labels = traitlabels)

pack_index = paste0("package_item_", letters[1:3])

itemnames = sort(unique(unlist(dat[, pack_index])))

itemnames

# .......................................
# .......................................
# select traits ####
# select traits to include in this analysis based on backward selection
cor_backward = read.csv("output/kendall-partial-correlation-backward-selection.csv")

cor_backward = cor_backward[order(cor_backward$cor), ]

cor_backward$trait = factor(cor_backward$trait,
                            levels = unique(cor_backward$trait))

head(cor_backward)

traits = traitlabels

traitlabels

traits = c("Fourth Harvest - Overall Performance",
           "Fourth Harvest - Preference For Replanting",
           "First Harvest - Marketability",
           "Second Harvest - Marketability",
           "Third Harvest - Marketability",
           "Fourth Harvest - Marketability",
           traits[traits %in% cor_backward$trait])

keep = traitlabels %in% traits

trait_list = trait_list[keep]

do.call("rbind", lapply(trait_list, function(x) cbind(x$trait_label, sum(x$keep))))

traitlabels = unlist(lapply(trait_list, function(x) c(x$trait_label)))

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

covar_clust = covar[c("Cluster", 'gender', 'whocontrolsell', "age", "incomecropshare", "experiencecrop")]

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

x = cbind(dat[c("id", "package_project_name")], covar_clust)

write.csv(x, "data/covariates-cluster.csv", row.names = FALSE)


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

# ........................................
# .......................................
# Worth map  ####
itemnames

# add name of features
traitlabels

labels = c("Plant survival 3rd harv", "Yield 3rd harv",
           "Marketability 3rd harv", "Yield 4th harv",
           "Leaf size 4th harv", "Taste 4th harv",
           "Marketability 4th harv","Overall performance",
           "Marketability 1st harv",  "Marketability 2nd harv",
           "Plant survival after transplanting", "Preference for replanting")

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})


# test likelihood ratio
llr = lapply(R, function(x){
  likelihood_ratio(x, split = covar$Cluster)
})

llr = do.call("rbind", llr)

llr$trait = labels

write.csv(llr, "output/likelihood-ratio-clusters.csv")

mod = lapply(R, PlackettLuce)

coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1424, n1 = 150)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

# pc_labs = as.data.frame(pc$scores[,c(1,2)])
# pc_labs$item = coeffs$item
# pc_labs = pc_labs[!duplicated(pc_labs$item), ]

pcplot = 
  autoplot(pc, 
         data = coeffs, 
         color = "item",
         loadings.label = TRUE, 
         loadings = TRUE, 
         loadings.colour = 'grey20',
         loadings.label.size = 5,
         loadings.label.color = "grey20") +
  # geom_text(data = pc_labs, aes(x = Comp.1, y = Comp.2,
  #                               label = item)) +
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank())

pcplot

ggsave("output/biplot-performance-all-traits.pdf",
       plot = pcplot,
       height = 25,
       width = 25,
       units = "cm")

# ...........................................
# ...........................................
# PCA segmented by gender and regions ####
# first fit a PL tree to see if gender influences 
# variety performance
gender_class = table(covar_clust$Cluster)
gender_class
plots_gender = list()

for (i in seq_along(gender_class)) {
  
  R_subset = lapply(R, function(x){
    x = x[covar_clust$Cluster == names(gender_class[i])]
    na.omit(x)
  })
  
  R_subset
  
  mod = lapply(R_subset, PlackettLuce)
  
  # get probabilities
  coeffs = lapply(mod, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = 150)
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = labels

  pc = princomp(coeffs[, -1], cor = TRUE)
  
  pcplot = 
    autoplot(pc, 
             data = coeffs, 
             color = "item",
             loadings.label = TRUE, 
             loadings = TRUE, 
             loadings.colour = 'grey20',
             loadings.label.size = 5,
             loadings.label.color = "grey20") +
    theme_bw() +
    theme(legend.position = "right", #if(i != 4) "none" else "right",
          legend.title = element_blank()) +
    labs(title = paste0(names(gender_class[i]),
                        ", n = (",
                        as.integer(gender_class[i]), ")"))
  
  plots_gender[[i]] = pcplot
  
}

p = plots_gender[[1]] + plots_gender[[2]] + plots_gender[[3]] + plots_gender[[4]]

p

ggsave("output/biplot-trait-performance-gender.pdf",
       plot = p,
       height = 20,
       width = 50,
       units = "cm")

# ...........................................
# ...........................................
# Marketability ####
traitsel = grep("Marketability", traitlabels)

do.call("rbind", lapply(trait_list, function(x) cbind(x$trait_label, 
                                                      sum(x$keep))))

R_sel = R[traitsel]

pld = data.frame()

M = matrix()

for(i in seq_along(R_sel)) {
  
  r = R_sel[[i]]
  
  keep = !is.na(r)
  
  group_i = covar[keep,]
  
  r = r[keep, ]
  
  d = cbind(group_i, Harvest = traitlabels[traitsel[i]])
  
  pld = rbind(pld, d)

  M = rbind(M, r)  
}

M = M[-1, ]

G = group(M, index = 1:length(M))

pld$G = G

head(pld)

pld$Harvest = gsub(" Harvest - Marketability", "", pld$Harvest)

#pld$Harvest = factor(pld$Harvest, levels = c("First", "Second", "Third", "Fourth"))

chars = which(unlist(lapply(pld[1:ncol(pld)], is.character)))

pld[chars] = lapply(pld[chars], as.factor)


# mod = forward_selection(pld, minsize = 50, gamma = TRUE)


tree = pltree(G ~ age + country + gender + Harvest, 
              data = pld, 
              gamma = TRUE, 
              minsize = 100)


tree

deviance(tree)

nodes_tree = predict(tree, type = "node")
node_id_tree = sort(unique(nodes_tree))

length(nodes_tree) == length(pld$G)

tree_mod = list()
nobs_tree = integer()

for (i in seq_along(node_id_tree)) {
  
  Gi = pld$G[nodes_tree == node_id_tree[i]]
  
  tree_mod[[i]] = PlackettLuce(Gi)
  
  nobs_tree = c(nobs_tree, length(Gi))
  
  rm(Gi)
}

tree_branch = gosset:::build_tree_branches(tree)

tree_nodes = gosset:::build_tree_nodes(tree_mod, 
                                       log = FALSE,
                                       node.ids = node_id_tree,
                                       n.obs = nobs_tree,
                                       ci.level = 0.1)

ptree = tree_branch / tree_nodes + plot_layout(heights =  c(1, 1))

ptree

ggsave(filename = "output/pltree-marketability-covars.pdf",
       plot = ptree,
       units = "cm",
       height = 20,
       width = 20)


ggsave("output/pltree-marketability-worth.pdf",
       plot = ptree,
       width = 20,
       height = 20,
       units = "cm")


# ...........................................
# ...........................................
# Overall preference ####
overall_trait = getTraitList(dat, c("_pos", "_neg"))

overall = lapply(overall_trait, function(x){
  x$trait_label
})

overall = unlist(overall)

sel = grepl("overallperformance|cropstageperformance", overall)

overall = overall[sel]

overall_trait = overall_trait[sel]

overall = fix_trait_labels(overall)

do.call("rbind", lapply(overall_trait, function(x) cbind(x$trait_label, sum(x$keep))))


R_overall = lapply(overall_trait, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})

pld = data.frame()

O = matrix()

for(i in seq_along(R_overall)) {
  
  r = R_overall[[i]]
  
  keep = !is.na(r)
  
  group_i = dat$gender[keep]
  
  r = r[keep, ]
  
  d = data.frame(Gender = group_i, Harvest = overall[i])
  
  pld = rbind(pld, d)
  
  O = rbind(O, r)  
}

O = O[-1, ]

G = group(O, index = 1:length(O))

pld$G = G

head(pld)

pld$Gender = as.factor(pld$Gender)

pld$Harvest = gsub(" Harvest - Overall Performance", "", pld$Harvest)

pld$Harvest = gsub(" Harvest - Crop Stage Performance", "", pld$Harvest)

pld$Harvest = factor(pld$Harvest, levels = c("First", "Second", "Third", "Fourth"))

tree = pltree(G ~ ., data = pld, minsize = 100, gamma = TRUE)

tree

ptree = plot(tree, ci.level = 0.1)

ggsave("output/pltree-marketability.pdf",
       plot = ptree,
       width = 20,
       height = 20,
       units = "cm")


