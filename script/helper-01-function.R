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


# plot pca
plot_pca = function(object, scale = 3){

  sums = summary(pc)
  
  vars = sums$sdev^2
  
  vars = round(vars/sum(vars) * 100, 1)
  
  pcd = as.data.frame(pc$scores[,1:2])
  names(pcd) = c("dim1", "dim2")
  pcd$item = coeffs$item
  
  pcd_text = split(pcd, pcd$item)
  
  pcd_text = lapply(pcd_text, function(x){
    data.frame(item = x$item[1],
               dim1_m = mean(x$dim1),
               dim2_m = mean(x$dim2))
  })
 
  pcd_text = do.call("rbind", pcd_text)
  
  loadings = as.data.frame(pc$loadings[1:length(traitlabels), ])
  names(loadings) = paste0("dim", 1:ncol(loadings))
  loadings$traits = rownames(loadings)
  
  pcplot = 
    ggplot(pcd) +
    geom_point(aes(x = dim1, y = dim2, color = item)) +
    #coord_fixed() +
    geom_segment(data = loadings, aes(x = 0, 
                                      y = 0, 
                                      xend = dim1 * scale,
                                      yend = dim2 * scale),
                 arrow = arrow(length = unit(0.3, "cm"), 
                               type = "open", angle = 25),
                 linewidth = 0.7, color = "grey20") +
    geom_label_repel(data = pcd_text, aes(x = dim1_m,
                                          y = dim2_m,
                                          label = item,
                                          color = item)) +
    geom_text_repel(data = loadings,
                    aes(label = traits,
                        x = dim1 * scale,
                        y = dim2 * scale),
                    box.padding = 0.2,
                    point.padding = 0.3,
                    size = 3.5,
                    color = "grey20", 
                    arrow = arrow(length = unit(0.3, "cm"), 
                                  type = "closed",
                                  angle = 25),
                    force = 4) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = paste0("Comp.1 (", vars[1], "%)"),
         y = paste0("Comp.2 (", vars[2], "%)"))
  
  pcplot
}


