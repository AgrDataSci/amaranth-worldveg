### load packages
library("PlackettLuce")
library("gosset")
library("climatrends")
library("ggplot2")
library("igraph")
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/b842039ab35583df9961025dde9ec1dcc83d5095/R/functions.R")
###



# call csv data to make data.frame
# df<- read.csv("data/Amaranth_tricot_marged_version.csv")
df<- read.csv("data/New_Amaranth_tricot_time_date_adjusted.csv")

#there are "Not observed" data. lets change "Not observed" to NA
df[df == "Not observed"] <- NA

# package varieties 
names(df)[grepl("package_item", names(df))][1] <- "package_item_A"
names(df)[grepl("package_item", names(df))][2] <- "package_item_B"
names(df)[grepl("package_item", names(df))][3] <- "package_item_C"
variety <-  c("package_item_A", "package_item_B" ,"package_item_C")


# change varieties name ( A2004 -> Local Mali)
table(df$package_item_A)

df$package_item_A[df$package_item_A == "A2004"] <- "Local Mali"
df$package_item_B[df$package_item_B == "A2004"] <- "Local Mali"
df$package_item_C[df$package_item_C == "A2004"] <- "Local Mali"

# subset df dataframe based on countries "package_country"

countries <- unique(df[,"package_country"])
table(df[,"package_country"])
countries
# [1] "ML" "BJ" "TZ"
# Mali = Ml, Bénin = BJ,  Tanzania  = TZ
mali_df<- subset(df, package_country == countries[1])
benin_df<-  subset(df, package_country == countries[2])
tanzania_df<- subset(df, package_country == countries[3])


table(mali_df$package_item_C)
table(benin_df$package_item_C)
table(tanzania_df$package_item_C)


table(benin_df$package_item_A)
table(benin_df$package_item_B)
table(benin_df$package_item_C)


################################################################################
################################Mali tricot#####################################
#tricot traits 
trait_neg<- names(mali_df[,grep("neg", names(mali_df))])
trait_pos<- names(mali_df[,grep("pos", names(mali_df))])

mali_traits_list <- list()
for (i in seq_along(trait_neg)){
  mali_traits_list[[i]] <- c(trait_neg[i], trait_pos[i])}

# allocate trait name into trait list
splittedname<- strsplit(trait_neg, "_")

for (i in seq_along(splittedname)){
  names(mali_traits_list)[[i]]<- paste0(splittedname[[i]][1],"_", splittedname[[i]][2])}


# list of ground nut ranking
R <- vector(mode = "list", length = length(mali_traits_list))

for (i in seq_along(mali_traits_list)) {
  
  R[[i]] <-rank_tricot(data = mali_df, 
                       items = variety,
                       input =c(unlist(mali_traits_list[i])),
                       validation.rankings = FALSE)}  # do not cosider evencase.


mod = lapply(R, PlackettLuce)

worth_map(mod, labels = names(mali_traits_list)) 


#### this codes are for summary because above case cannot recall summary. 
# Prepare list of rankings for different traits
# I may ignore A = B = C cases
d_i<- list()

for(i in 1:length(mali_traits_list)){
  d_i_t <- cbind(mali_df[, c("package_item_A", "package_item_B", "package_item_C")], mali_df[, mali_traits_list[[i]]])
  d_i_t <- d_i_t[-which(is.na(d_i_t[,4]) & is.na(d_i_t[,5])),]
  d_i[[i]]  <- d_i_t
}
d_i
names(d_i) <- names(mali_traits_list)

str(d_i)

# Identify empty vectors
empty_indices <- sapply(d_i, function(vec) nrow(vec) == 0)
empty_indices

# Remove empty vectors
d_i <- d_i[!empty_indices]

# Prepare list of rankings for different traits
R_r <- list()

for(i in seq_along(mali_traits_list)){
  
  # put in the matrix
  R_r[[i]] <- rank_tricot(d_i[[i]], items = 1:3, input = 4:5)
  
}

mod_r = lapply(R_r, PlackettLuce)

mod_r[[1]]
mod[[1]]

PlackettLuce(R_r[[1]], ref = "Local Mali")
PlackettLuce(R[[1]], ref = "Local Mali")


###



##
# save worth map plots as png file 
png(filename = "output/worthmap_amaranth_mali_no_even.png", width = 30, height = 45, units = "cm", res = 400)
worth_map(mod, labels = names(mali_traits_list)) 
dev.off()

# reference existed

png(filename = "output/worthmap_amaranth_mali_no_even_reference_is_local_mali.png", width = 30, height = 15, units = "cm", res = 400)
worth_map(mod, labels = names(mali_traits_list), ref = "Local Mali") 
dev.off()

##
# save worth  plots as png file 
png(filename = paste0("output/", names(mali_traits_list)[1], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[1]])
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[2], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[2]])
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[3], "_mali_worth_plot.png"), width = 30, height = 30, units = "cm", res = 400)
plot_worth(mod[[3]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[4], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[4]])
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[5], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[5]])
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[6], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[6]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[7], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[7]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[8], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[8]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[9], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[9]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[10], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[10]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[11], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[11]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[12], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[12]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[13], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[13]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[14], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[14]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[15], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[15]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[16], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[16]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[17], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[17]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[18], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[18]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[19], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[19]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[20], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[20]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[21], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[21]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[22], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[22]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[23], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[23]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[24], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[24]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[25], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[25]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[26], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[26]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[27], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[27]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[28], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[28]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[29], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[29]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[30], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[30]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[31], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[31]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[32], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[32]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[33], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[33]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[34], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[34]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[35], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[35]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[36], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[36]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[37], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[37]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[38], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[38]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[39], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[39]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[40], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[40]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[41], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[41]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[42], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[42]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[43], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[43]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[44], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[44]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[45], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[45]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[46], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[46]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[47], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[47]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[48], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[48]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[49], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[49]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[50], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[50]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[51], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[51]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[52], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[52]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[53], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[53]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[54], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[54]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[55], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[55]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[56], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[56]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[57], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[57]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[58], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[58]]) 
dev.off()

png(filename = paste0("output/", names(mali_traits_list)[59], "_mali_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[59]]) 
dev.off()



# it's work but there is nothing why?  
# for (i in seq_along(mod)) {
#   
#   plot_save<- plot_worth(mod[[i]])
#   
#   png(filename = paste0("output/", names(mali_traits_list)[i], "_mali_worth_plot.png"),
#       width = 30, height = 45, units = "cm", res = 400)
#   plot_save
#   dev.off()
# }


### Mali Kendal tau 

#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r <- matrix(NA, nrow=59, ncol=59)

#Prepare the effective N matrix
p_value <- matrix(NA, nrow=59, ncol=59)

for(i in 1:59){
  for (j in 1:59){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R[[i]], R[[j]], null.rm = TRUE, na.omit =TRUE) 
    r[i,j] <- as.numeric(rNij[1])
    p_value[i,j] <- as.numeric(rNij[4])}}

mali_r <- r
mali_p_value<- p_value

write.table(mali_r,file="data/mali_kendall_tau.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(mali_p_value,file="data/mali_kendall_tau_pvalue.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


################################################################################
################################Benin tricot#####################################
#tricot traits 
trait_neg<- names(benin_df[,grep("neg", names(benin_df))])
trait_pos<- names(benin_df[,grep("pos", names(benin_df))])

benin_traits_list <- list()
for (i in seq_along(trait_neg)){
  benin_traits_list[[i]] <- c(trait_neg[i], trait_pos[i])}

# allocate trait name into trait list
splittedname<- strsplit(trait_neg, "_")

for (i in seq_along(splittedname)){
  names(benin_traits_list)[[i]]<- paste0(splittedname[[i]][1],"_", splittedname[[i]][2])}


# list of ground nut ranking
R <- vector(mode = "list", length = length(benin_traits_list))

for (i in seq_along(benin_traits_list)) {
  
  R[[i]] <-rank_tricot(data = benin_df, 
                       items = variety,
                       input =c(unlist(benin_traits_list[i])),
                       validation.rankings = FALSE)}  # do not cosider evencase.


mod = lapply(R, PlackettLuce)
worth_map(mod, labels = names(benin_traits_list)) 
worth_map(mod, labels = names(benin_traits_list), ref = "Local Benin") 

##
# save worth_map as png
png(filename = "output/worthmap_amaranth_benin_no_even.png", width = 30, height = 45, units = "cm", res = 400)
worth_map(mod, labels = names(benin_traits_list)) 
dev.off()


png(filename = "output/worthmap_amaranth_benin_no_even_reference_is_local_benin.png", width = 30, height = 45, units = "cm", res = 400)
worth_map(mod, labels = names(benin_traits_list), ref = "Local Benin") 
dev.off()

##
# save worth  plots as png file 
png(filename = paste0("output/", names(benin_traits_list)[1], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[1]])
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[2], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[2]])
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[3], "_benin_worth_plot.png"), width = 30, height = 30, units = "cm", res = 400)
plot_worth(mod[[3]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[4], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[4]])
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[5], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[5]])
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[6], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[6]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[7], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[7]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[8], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[8]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[9], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[9]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[10], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[10]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[11], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[11]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[12], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[12]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[13], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[13]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[14], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[14]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[15], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[15]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[16], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[16]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[17], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[17]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[18], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[18]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[19], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[19]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[20], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[20]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[21], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[21]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[22], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[22]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[23], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[23]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[24], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[24]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[25], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[25]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[26], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[26]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[27], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[27]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[28], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[28]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[29], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[29]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[30], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[30]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[31], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[31]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[32], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[32]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[33], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[33]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[34], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[34]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[35], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[35]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[36], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[36]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[37], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[37]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[38], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[38]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[39], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[39]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[40], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[40]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[41], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[41]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[42], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[42]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[43], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[43]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[44], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[44]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[45], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[45]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[46], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[46]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[47], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[47]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[48], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[48]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[49], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[49]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[50], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[50]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[51], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[51]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[52], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[52]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[53], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[53]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[54], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[54]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[55], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[55]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[56], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[56]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[57], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[57]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[58], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[58]]) 
dev.off()

png(filename = paste0("output/", names(benin_traits_list)[59], "_benin_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[59]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()


### benin Kendal tau 

#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r <- matrix(NA, nrow=59, ncol=59)

#Prepare the effective N matrix
p_value <- matrix(NA, nrow=59, ncol=59)

for(i in 1:59){
  for (j in 1:59){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R[[i]], R[[j]], null.rm = TRUE, na.omit =TRUE) 
    r[i,j] <- as.numeric(rNij[1])
    p_value[i,j] <- as.numeric(rNij[4])}}

benin_r <- r
benin_p_value<- p_value


write.table(benin_r,file="data/benin_kendall_tau.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(benin_p_value,file="data/benin_kendall_tau_pvalue.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)


################################################################################
################################tanzania tricot#################################
#tricot traits 
trait_neg<- names(tanzania_df[,grep("neg", names(tanzania_df))])
trait_pos<- names(tanzania_df[,grep("pos", names(tanzania_df))])

tanzania_traits_list <- list()
for (i in seq_along(trait_neg)){
  tanzania_traits_list[[i]] <- c(trait_neg[i], trait_pos[i])}

# allocate trait name into trait list
splittedname<- strsplit(trait_neg, "_")

for (i in seq_along(splittedname)){
  names(tanzania_traits_list)[[i]]<- paste0(splittedname[[i]][1],"_", splittedname[[i]][2])}


# list of ground nut ranking
R <- vector(mode = "list", length = length(tanzania_traits_list))

for (i in seq_along(tanzania_traits_list)) {
  
  R[[i]] <-rank_tricot(data = tanzania_df, 
                       items = variety,
                       input =c(unlist(tanzania_traits_list[i])),
                       validation.rankings = FALSE)}  # do not cosider evencase.


mod = lapply(R, PlackettLuce)
worth_map(mod, labels = names(tanzania_traits_list)) 
worth_map(mod, labels = names(tanzania_traits_list), ref ="Local check (AC-NL)") 

# save wort plot as png file. 
png(filename = "output/worthmap_amaranth_tanzania_no_even.png", width = 30, height = 45, units = "cm", res = 400)
worth_map(mod, labels = names(tanzania_traits_list)) 
dev.off()

# reference is  Local check (AC-NL)
png(filename = "output/worthmap_amaranth_tanzania_no_even.png", width = 30, height = 45, units = "cm", res = 400)
worth_map(mod, labels = names(tanzania_traits_list), ref ="Local check (AC-NL)") 
dev.off()

##
# save worth  plots as png file 
png(filename = paste0("output/", names(tanzania_traits_list)[1], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[1]])
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[2], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[2]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[3], "_tanzania_worth_plot.png"), width = 30, height = 30, units = "cm", res = 400)
plot_worth(mod[[3]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[4], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[4]])
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[5], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[5]])
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[6], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[6]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[7], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[7]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[8], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[8]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[9], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[9]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[10], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[10]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[11], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[11]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[12], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[12]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[13], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[13]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[14], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[14]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[15], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[15]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[16], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[16]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[17], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[17]])
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[18], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[18]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[19], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[19]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[20], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[20]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[21], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[21]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[22], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[22]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[23], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[23]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[24], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[24]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[25], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[25]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[26], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[26]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[27], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[27]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[28], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[28]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[29], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[29]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[30], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[30]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[31], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[31]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[32], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[32]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[33], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[33]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[34], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[34]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[35], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[35]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[36], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[36]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[37], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[37]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[38], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[38]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[39], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[39]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[40], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[40]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[41], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[41]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[42], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[42]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[43], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[43]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[44], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[44]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[45], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[45]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[46], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[46]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[47], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[47]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[48], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[48]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[49], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[49]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[50], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[50]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[51], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[51]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[52], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[52]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[53], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[53]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[54], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[54]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[55], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[55]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[56], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[56]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[57], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[57]]) 
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[58], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[58]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(tanzania_traits_list)[59], "_tanzania_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[59]])
dev.off()





### tanzania Kendal tau 

#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r <- matrix(NA, nrow=59, ncol=59)

#Prepare the effective N matrix
p_value <- matrix(NA, nrow=59, ncol=59)

for(i in 1:59){
  for (j in 1:59){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R[[i]], R[[j]], null.rm = TRUE, na.omit =TRUE) 
    r[i,j] <- as.numeric(rNij[1])
    p_value[i,j] <- as.numeric(rNij[4])}}

tanzania_r <- r
tanzania_p_value<- p_value

write.table(tanzania_r,file="data/tanzania_kendall_tau.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(tanzania_p_value,file="data/tanzania_kendall_tau_pvalue.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)



################################################################################
################################all three countries#############################

# make temperal df for making worth_map of all three countries 
three_df<- df

# check all varieties that tested across three countries 
varieties_mali <- unique(mali_df$package_item_A)
varieties_benin <- unique(benin_df$package_item_A)
varieties_tanzania <- unique(tanzania_df$package_item_A)

all_varieties<- unique(df$package_item_A)
overlapped<- intersect(intersect(varieties_mali, varieties_benin), varieties_tanzania)
overlapped

non_overlapped<- all_varieties[!all_varieties %in% overlapped]


all_row_number_a <- list()
for(i in seq_along(non_overlapped)){all_row_number_a[[i]] <- which(df[,variety[1]] %in% non_overlapped[i])}

all_row_number_b<- list()
for(i in seq_along(non_overlapped)){all_row_number_b[[i]] <- which(df[,variety[2]] %in% non_overlapped[i])}

all_row_number_c<- list()
for(i in seq_along(non_overlapped)){all_row_number_c[[i]] <- which(df[,variety[3]] %in% non_overlapped[i])}

all_row_number_a <- unlist(all_row_number_a)
all_row_number_b <- unlist(all_row_number_b)
all_row_number_c <- unlist(all_row_number_c)

all_row_number <- c(all_row_number_a, all_row_number_b, all_row_number_c)

all_row_number<- unique(all_row_number)

all_row_number<- sort(all_row_number)

#exclude farmers who test non- overlapped.
three_df_excluded<-three_df[-c(all_row_number),]

#check it 
unique(three_df_excluded$package_item_A)



#tricot traits 
trait_neg<- names(three_df_excluded[,grep("neg", names(three_df_excluded))])
trait_pos<- names(three_df_excluded[,grep("pos", names(three_df_excluded))])

three_conturies_traits_list <- list()
for (i in seq_along(trait_neg)){
  three_conturies_traits_list[[i]] <- c(trait_neg[i], trait_pos[i])}

# allocate trait name into trait list
splittedname<- strsplit(trait_neg, "_")

for (i in seq_along(splittedname)){
  names(three_conturies_traits_list)[[i]]<- paste0(splittedname[[i]][1],"_", splittedname[[i]][2])}


# list of ground nut ranking
R <- vector(mode = "list", length = length(three_conturies_traits_list))

for (i in seq_along(three_conturies_traits_list)) {
  
  R[[i]] <-rank_tricot(data = three_df_excluded, 
                       items = variety,
                       input =c(unlist(three_conturies_traits_list[i])),
                       validation.rankings = FALSE)}  # do not cosider evencase.


mod = lapply(R, PlackettLuce)
worth_map(mod, labels = names(three_conturies_traits_list)) 

##
# save worth_plot as png file 
png(filename = "output/worthmap_amaranth_tree_countires_no_even.png", width = 40, height = 50, units = "cm", res = 400)
worth_map(mod, labels = names(three_conturies_traits_list)) 
dev.off()

# reference is poli
png(filename = "output/worthmap_amaranth_tree_countires_no_even_reference_is_poli.png", width = 40, height = 50, units = "cm", res = 400)
worth_map(mod, labels = names(three_conturies_traits_list), ref ="Poli") 
dev.off()



##
# save worth  plots as png file 
png(filename = paste0("output/", names(three_conturies_traits_list)[1], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[1]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[2], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[2]]) + scale_x_continuous(limits=c(0,0.5))
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[3], "_three_conturies_worth_plot.png"), width = 30, height = 30, units = "cm", res = 400)
plot_worth(mod[[3]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[4], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[4]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[5], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[5]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[6], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[6]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[7], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[7]]) + scale_x_continuous(limits=c(0,0.5))
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[8], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[8]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[9], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[9]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[10], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[10]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[11], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[11]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[12], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[12]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[13], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[13]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[14], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[14]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[15], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[15]]) + scale_x_continuous(limits=c(0,0.5))
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[16], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[16]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[17], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[17]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[18], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[18]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[19], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[19]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[20], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[20]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[21], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[21]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[22], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[22]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[23], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[23]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[24], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[24]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[25], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[25]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[26], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[26]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[27], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[27]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[28], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[28]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[29], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[29]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[30], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[30]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[31], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[31]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[32], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[32]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[33], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[33]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[34], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[34]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[35], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[35]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[36], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[36]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[37], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[37]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[38], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[38]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[39], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[39]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[40], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[40]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[41], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[41]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[42], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[42]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[43], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[43]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[44], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[44]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[45], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[45]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[46], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[46]]) + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[47], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[47]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[48], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[48]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[49], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[49]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[50], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[50]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[51], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[51]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[52], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[52]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[53], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[53]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[54], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[54]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[55], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[55]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[56], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[56]]) 
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[57], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[57]])  + scale_x_continuous(limits=c(0,0.3))
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[58], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[58]])
dev.off()

png(filename = paste0("output/", names(three_conturies_traits_list)[59], "_three_conturies_worth_plot.png"), width = 30, height = 15, units = "cm", res = 400)
plot_worth(mod[[59]])
dev.off()




### all Kendal tau 

#Calculate triats' kendell tau correlations ship
#Prepare the correlation matrix to be filled with values
r <- matrix(NA, nrow=59, ncol=59)

#Prepare the effective N matrix
p_value <- matrix(NA, nrow=59, ncol=59)

for(i in 1:59){
  for (j in 1:59){ #Fills lower triangle only, to avoid calculating the same value twice
    
    rNij <- kendallTau(R[[i]], R[[j]], null.rm = TRUE, na.omit =TRUE) 
    r[i,j] <- as.numeric(rNij[1])
    p_value[i,j] <- as.numeric(rNij[4])}}

all_r <- r
all_p_value<- p_value

all_r
all_p_value

write.table(all_r,file="data/all_kendall_tau.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)
write.table(all_p_value,file="data/all_kendall_tau_pvalue.csv",  append=FALSE, sep= "," , row.names = FALSE, col.names=TRUE)

################################################################################
################################################################################


