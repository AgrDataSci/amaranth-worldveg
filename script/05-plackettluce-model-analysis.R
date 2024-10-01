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
#source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")
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

items_available = table(unlist(dat[pack_index]), rep(dat$package_country, times = 3))

table(unlist(dat[pack_index]), rep(covar$gender, times = 3))

write.csv(items_available, "output/table-01-variety-summary.csv")

# create a representation of the trial network
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
clust_labs = c("Older Women Generalists",
               "Older Men Generalists",
               "Young Men Specialists",
               "Young Women Specialists")

covar$clust = covar$Cluster

covar$Cluster = factor(covar$clust, labels = clust_labs)

covar$Cluster = factor(covar$Cluster, levels = c("Older Women Generalists",
                                                 "Young Women Specialists",
                                                 "Older Men Generalists",
                                                 "Young Men Specialists"))
table(covar$clust)
table(covar$Cluster)
names(covar)

drop = which(names(covar) %in% c("id", "package_project_name", "clust"))

covar = covar[, -drop]

table(covar$Cluster, covar$whocontrolprod)

table(covar$Cluster, covar$whocontrolsell)

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
llr
write.csv(llr, "output/likelihood-ratio-clusters.csv")

# PL model
mod = lapply(R, PlackettLuce)

# co = lapply(mod, coefficients)
# 
# co = do.call(cbind, co)
# co = as.data.frame(co)
# names(co) = labels
# 
# co$item = rownames(co)
# 
# coeffs = co

coeffs = lapply(mod, function(x){
  resample(x, bootstrap = TRUE, seed = 1424, n1 = 3)
})

coeffs = do.call(cbind, coeffs)

rmv = which(names(coeffs) == "item")[-1]

coeffs = coeffs[-rmv]

names(coeffs)[-1] = labels

# add name of features
#names(winprobs) = labels

pc = princomp(coeffs[-1], cor = FALSE)

pcplot = plot_pca(pc)

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
    labs(title = paste0("(", LETTERS[i], ") ", 
                        names(gender_class[i]),
                        ", n = ",
                        as.integer(gender_class[i])))
  
  pcplot
  
  plots_gender[[i]] = pcplot
  
}

p = plots_gender[[1]] + plots_gender[[2]] + 
  plots_gender[[3]] + plots_gender[[4]]

p

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

pld$Harvest = factor(pld$Harvest, levels = c("First", "Second", "Third", "Fourth"))

chars = which(unlist(lapply(pld[1:ncol(pld)], is.character)))

pld[chars] = lapply(pld[chars], as.factor)

head(pld)

str(pld)

tree = pltree(G ~ Cluster + Harvest,
              data = pld,
              gamma = TRUE,
              minsize = 100)


tree

ptree = plot(tree, log = TRUE, ci.level = 0.05, ref = "Local")
ptree
ggsave(filename = "output/pltree-marketability-covars.pdf",
       plot = ptree,
       units = "cm",
       height = 30,
       width = 40)


# .........................................
# .........................................
# get pairwise comps matrix using worth ####
lvls = rev(itemnames)

plots = list()

models = list()

for(k in seq_along(gender_class)) {
  
  g = pld[pld$Cluster == names(gender_class[k]), "G"]
  
  mod = PlackettLuce(g)
  
  models[[k]] = mod
  
  pair_worth = matrix(0, 
                      nrow = length(lvls), 
                      ncol = length(lvls),
                      dimnames = list(rev(lvls), rev(lvls)))
  
  worths = coef(mod, log = FALSE)
  
  for(i in seq_len(ncol(pair_worth))) {
    
    for(j in seq_len(nrow(pair_worth))) {
      
      col_i = dimnames(pair_worth)[[1]][i]
      row_j = dimnames(pair_worth)[[2]][j]
      
      if(col_i == row_j) next
      
      print(c(col_i, row_j))
      
      w = worths[col_i] / (worths[col_i] + worths[row_j])
      
      w = round(w, 2)
      
      pair_worth[row_j, col_i] = w
      
    }
    
  }
  
  pair_worth = pair_worth - 0.5
  pair_worth[pair_worth == -0.5] = NA
  
  #pair_worth[upper.tri(pair_worth)] = NA
  
  pair_dat = data.frame(player1 = rep(rev(lvls), each = length(lvls)), 
                        player2 = rep(rev(lvls), times = length(lvls)),
                        worth = as.vector(pair_worth))
  
  pair_dat$player1 = factor(pair_dat$player1, levels = lvls)
  
  pair_dat$player2 = factor(pair_dat$player2, levels = rev(lvls))
  
  pair_dat$worth = round(pair_dat$worth, 2)
  
  p = ggplot(pair_dat, 
             aes(x = player2, 
                 y = player1,
                 fill = worth,
                 label = worth)) +
    geom_tile() + 
    geom_text() +
    scale_fill_gradient2(low = "#d53e4f", 
                         high = "#3288bd", 
                         mid = "white",
                         na.value = "white",
                         midpoint = 0) +
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(axis.text = element_text(color = "grey10"),
          strip.text.x = element_text(color = "grey10"),
          axis.text.x = element_text(angle = 90, hjust = 0),
          panel.grid = element_blank(),
          legend.position = "none") +
    labs(x = "", 
         y = "",
         title = paste0("(", LETTERS[k], ") ", 
                        names(gender_class)[k]),
         fill = "")
  p
  plots[[k]] = p
  
}

p = plots[[1]] + plots[[2]] + 
  plots[[3]] + plots[[4]]
p

ggsave("output/pairwise-probabilities-marketability.pdf",
       plot = p,
       width = 35,
       height = 35,
       units = "cm",
       dpi = 600)
 
# Perform regret analysis 
r = regret(models, n1 = 1000, normalize = T)

write.csv(r, "output/regret-table-four-groups.csv")


