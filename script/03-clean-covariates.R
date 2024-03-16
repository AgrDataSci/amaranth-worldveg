# load packages
library("tidyverse")

covar = read.csv("data/summaries/covar-available.csv")

sel = covar$sel == "x"

covar = covar[sel, ]

dat = read.csv("data/amaranth-tricot-data.csv")

sel = names(dat) %in% covar$covar 

index = which(sel)

# get unique values from character 
values = data.frame() 

for(i in seq_along(index)) {
  if (is.character(dat[,index[i]])) {
    d = data.frame(covar = names(dat)[index[i]],
                   name = unique(dat[,index[i]]),
                   new_name = "")
    
    values = rbind(values, d)
    
  }
}


# write_excel_csv(values, "data/summaries/covar-replacement-values.csv")

values = read.csv("data/summaries/covar-replacement-values.csv", na.strings = "")

values = na.omit(values)

index = unique(values$covar)

for(i in seq_along(index)) {
  
  repl = values[values$covar == index[i], ]
  
  for (j in seq_len(nrow(repl))) {
    
    dat[,index[i]] = ifelse(dat[,index[i]] == repl[j, "name"], 
                            repl[j, "new_name"], 
                            dat[,index[i]])
    
  }
  
}

dat[index]


# age 
dat$registration_age = ifelse(dat$registration_age > 1000, 
                 2023 - dat$registration_age, 
                 dat$registration_age)

dat$registration_age = ifelse(dat$registration_age <= 15, NA, dat$registration_age)

plot(density(dat$registration_age, na.rm = T))

dat$registration_experiencecrop = ifelse(dat$registration_experiencecrop > dat$registration_age,
                                         NA, 
                                         dat$registration_experiencecrop)

plot(density(dat$registration_experiencecrop, na.rm = T))

dat$registration_incomecropshare[dat$registration_incomecropshare > 100] = NA 

write_excel_csv(dat, file = "data/amaranth-tricot-data.csv")




