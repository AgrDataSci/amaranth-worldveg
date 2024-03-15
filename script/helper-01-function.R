#Define new function to obtain a Kendall tau correlation matrix and effective N matrix
kendallTauCorMatrix = function(L){
  
  n_i = length(L)
  
  # Check if the list is longer than 3 and check if it contains rankings
  stopifnot(n_i>2, inherits(L[[1]], "rankings"), inherits(L[[2]], "rankings"))
  
  #Prepare the correlation matrix to be filled with values
  r = matrix(NA, nrow=n_i, ncol=n_i)
  rownames(r) = names(L)
  colnames(r) = names(L)
  
  #Prepare the effective N matrix
  N = matrix(NA, nrow=n_i, ncol=n_i)
  rownames(N) = names(L)
  colnames(N) = names(L)
  
  #Fill the matrix
  for(i in 1:n_i){
    for (j in 1:i){ #Fills lower triangle only, to avoid calculating the same value twice
      
      rNij = kendallTau(L[[i]], L[[j]]) #TODO deal with NA values
      r[i,j] = as.numeric(rNij[1])
      N[i,j] = as.numeric(rNij[2]) 
      
    }
    
  }
  
  # Make matrices symmetric    
  r[upper.tri(r)] = t(r)[upper.tri(r)]
  N[upper.tri(N)] = t(N)[upper.tri(N)]
  
  # Wrap result
  results = list(Kendall_tau_corr = r, Effective_N = N)
  
  return(results)
  
}

# function set to work only with this data set, since the process repeats in 
# different scripts
fix_trait_labels = function(x){
  
  x = gsub("_qst_|_|tricot", " ", x)
  x = gsub("nursery", "nursery -", x)
  x = gsub("transplanting", "transplanting -", x)
  x = gsub("reproductive", "reproductive -", x)
  x = gsub("firstharvest", "first harvest -", x)
  x = gsub("secondharvest", "second harvest -", x)
  x = gsub("thirdharvest", "third harvest -", x)
  x = gsub("fourthharvest", "fourth harvest -", x)
  x = gsub("plantsurvival", "plant survival", x)
  x = gsub("pestresistance", "pest resistance", x)
  x = gsub("floodtolerance", "flood tolerance", x)
  x = gsub("diseasesresistance", "diseases resistance", x)
  x = gsub("plantheight", "plant height", x)
  x = gsub("leafsize", "leaf size", x)
  x = gsub("droughttolerance", "drought tolerance", x)
  x = gsub("overallperformance", "overall performance", x)
  x = gsub("grownext", "preference for replanting", x)
  x = gsub("cropstageperformance", "crop stage performance", x)
  
  x = ClimMobTools:::.title_case(x)
  
  x
}


