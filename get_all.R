#Get all IDs
growth <- readLines("growth.html")
maturity <- readLines("maturity.html")
spawning <- readLines("spawning.html")
fecundity <- readLines("fecundity.html")

#Clean up function
clean_up <- function(trait,pattern){
  URLs <- NA
  lines <- grep("php\\?ID\\=",trait)  
  
    for(i in 1:length(lines)){
      peas <- unlist(strsplit(species.list[lines[i]],split="\""))
      http <- peas[grep(pattern,peas)]
      URLs[i] <- http
    }

  return(URLs)  
}

#Get reference IDs

#Prioritize by number of species, observations, and traits

#Print out nicely formatted bibliography