# .......................................
# .......................................
library("ggrepel")
library("tidyverse")
library("patchwork")
library("magrittr")
library("ggfortify")
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
source("script/helper-01-function.R")

# .......................................
# .......................................
# Organize the traits by data collection moment ####
# read the file 
list.files("data")

dat = read.csv("data/amaranth-tricot-data.csv")

covar = read.csv("data/covariates-cluster.csv")

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

t(table(rep(dat$package_country, times = 3), unlist(dat[pack_index])))

net = dat[, pack_index]

net$best = "A"
net$worst = "C"

r = rank_tricot(net, pack_index, c("best", "worst"))

plot(network(r))

pdf(file = "output/trial-network.pdf",
    width = 9,
    height = 9)
plot(network(r))
dev.off()

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
           traits[traits %in% cor_backward$trait])

keep = traitlabels %in% traits

trait_list = trait_list[keep]

do.call("rbind", lapply(trait_list, function(x) cbind(x$trait_label, sum(x$keep))))

traitlabels = unlist(lapply(trait_list, function(x) c(x$trait_label)))

# ..............................
# ..............................
# labels to clusters from chatGPT
clust_labs = c("Empowered Women Farmers",
               "Gender Balanced Farming",
               "Rising Men Entrepreneurs",
               "Innovative Women Farmers")

covar$clust = covar$Cluster

covar$Cluster = factor(covar$clust, labels = clust_labs)

covar$Cluster = factor(covar$Cluster, levels = c("Empowered Women Farmers",
                                                 "Innovative Women Farmers",
                                                 "Gender Balanced Farming",
                                                 "Rising Men Entrepreneurs"))
table(covar$clust)
table(covar$Cluster)
names(covar)

drop = which(names(covar) %in% c("id", "package_project_name", "clust"))

covar = covar[, -drop]

# ........................................
# .......................................
# PCA with log-worth  ####
itemnames

# add name of features
traitlabels

labels = c("Plant survival 3rd harv", "Yield 3rd harv",
           "Marketability 3rd harv", "Yield 4th harv",
           "Leaf size 4th harv", "Taste 4th harv",
           "Marketability 4th harv", "Overall performance",
           "Marketability 1st harv", "Marketability 2nd harv", 
           "Plant survival after transplanting", "Preference for replanting")

R = lapply(trait_list, function(x){
  rank_tricot(data = dat,
              items = pack_index,
              input = x$string,
              validate.rankings = TRUE)
})


# test likelihood ratio
# this will check whether the ranks are significantly distinct 
# from each group
llr = lapply(R, function(x){
  likelihood_ratio(x, split = covar$Cluster)
})

llr = do.call("rbind", llr)

llr$trait = labels

write.csv(llr, "output/likelihood-ratio-clusters.csv")


# PL model
mod = lapply(R, PlackettLuce)

coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1424, n1 = 200)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc)

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
gender_class = table(covar$Cluster)
gender_class
plots_gender = list()

for (i in seq_along(gender_class)) {
  
  R_subset = lapply(R, function(x){
    x = x[covar$Cluster == names(gender_class[i])]
    na.omit(x)
  })
  
  R_subset
  
  mod = lapply(R_subset, PlackettLuce)
  
  # get probabilities
  coeffs = lapply(mod, function(x){
    resample(x, bootstrap = TRUE, seed = 1424, n1 = 200)
  })
  
  coeffs = do.call(cbind, coeffs)
  
  rmv = which(names(coeffs) == "item")[-1]
  
  coeffs = coeffs[-rmv]
  
  names(coeffs)[-1] = labels

  pc = princomp(coeffs[, -1], cor = TRUE)
  
  pcplot = plot_pca(pc, scale = 6) + 
    labs(title = paste0(names(gender_class[i]),
                        ", n = (",
                        as.integer(gender_class[i]), ")"))
  
  pcplot
  
  plots_gender[[i]] = pcplot
  
}

p = plots_gender[[1]] + plots_gender[[2]] + plots_gender[[3]] + plots_gender[[4]]

ggsave("output/biplot-trait-performance-gender.pdf",
       plot = p,
       height = 35,
       width = 35,
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


