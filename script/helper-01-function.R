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
  x = ClimMobTools:::.title_case(x)
  x = gsub("Finalevaluationoverallimpression", "Overall", x)
  x = gsub("Overallimpression", "Overall", x)
  x = gsub("overallimpression", "Overall", x)
  x = gsub("Storage", "Storage ", x)
  x = gsub("Preparation", "Preparation ", x)
  x = gsub("resistance", "Resistance", x)
  x = gsub("tolerance", "Tolerance", x)
  x = gsub("colour", "Colour", x)
  x = gsub("yield", " Yield", x)
  x = gsub("content", "Content", x)
  x = gsub("Weedcompetitivenesscanopyformation", "Weed Competitiveness", x)
  x = gsub("Survivalsproutingnrofplantsleft", "Survival Sprouting", x)
  x = gsub("Survivalsprouting", "Survival Sprouting", x)
  x = gsub("Branchinghabitarchitectureoftheplant", "Branching Habit", x)
  x = gsub("ebaqualitymouldability", "Mouldability", x)
  x = gsub("notstickinesstohand Mouth", "Not Stickiness", x)
  x = gsub("swellingofgari", "Swelling", x)
  x = gsub("notstickiness", "Not Stickiness", x)
  x = gsub("Woodyfilament", "Woody Filament", x)
  x = gsub("Gariqualitydryness", "Gari Dryness", x)
  x = gsub("Waterreleaseduringpressing", "Water Release", x)
  x = gsub("Mashpulpdiscolor", "MashPulpDiscolor", x)
  x = gsub("Gariprocessingeaseofpeeling", "Easy Peeling", x)
  x = gsub("Suitabilitytosoilandenvironment", "Suitability to Soil", x)
  x = gsub("Germinationsprouting", "Sprouting", x)
  x = gsub("DensitywaterContent", "Water Content", x)
  x = gsub("Swellingofmash", "Swelling Mash", x)
  x = gsub("Choiceforreplant", "Choice for Replant", x)
  x = gsub("Plantgrowth", "Plant Growth", x)
  x = gsub("DiseaseResistance", "Disease Resistance", x)
  x = gsub("GariYield", "Gari Yield", x)
  x = gsub("Rootsize", "Root Size", x)
  x = gsub("RootColour", "Root color", x)
  x = gsub("Rootshape", "Root shape", x)
  x = gsub("MashPulpDiscolor", "Mash Pulp Discolor", x)
  x = gsub("Freshroots", "Fresh roots", x)
  x = gsub("RootsYield", "Roots Yield", x)
  x = gsub("Rootrot", "Root rot", x)
  x = ClimMobTools:::.title_case(x)
  x = gsub("  ", " ", x)
  x = gsub("Harvesting Fresh Roots ", 
           "Harvesting ", x)
  x = gsub("Harvesting Harvesting Yield", "Harvesting Fresh Roots Yield", x)
  
  x = gsub("First Month ", "First Month - ", x)
  x = gsub("Third Month ", "Third Month - ", x)
  x = gsub("Sixth Month ", "Sixth Month - ", x)
  x = gsub("Ninth Month ", "Ninth Month - ", x)
  x = gsub("Harvesting ", "Harvesting - ", x)
  x = gsub("Gari Processing ", "Gari Processing - ", x)
  x = gsub("Gari Quality ", "Gari Quality - ", x)
  x = gsub("Eba Quality After Preparation ", "Eba Quality After Preparation - ", x)
  x = gsub("Eba Quality After Storage ", "Eba Quality After Storage - ", x)
  x = gsub("Final Evaluation ", "Final Evaluation - ", x)
  
  x
}


