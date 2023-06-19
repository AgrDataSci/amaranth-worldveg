### load packages
library("PlackettLuce")
library("gosset")

#call csv data to make data.frame
#df<- read.csv("data/Amaranth_tricot_marged_version.csv")
df<- read.csv("data/Amaranth_tricot_marged_version_deleted_emptycol.csv")



# pcakage varieties 

colnames(df)[80] <- "package_item_A"
colnames(df)[81] <- "package_item_B"
colnames(df)[82] <- "package_item_C"
variety <-  c("package_item_A", "package_item_B" ,"package_item_C")



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
