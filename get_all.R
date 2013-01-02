#Get all IDs
growth <- readLines("growth.html")
maturity <- readLines("maturity.html")
spawning <- readLines("spawning.html")
fecundity <- readLines("fecundity.html")

#Clean up function
clean_up <- function(trait){
  URLs <- NA
  lines <- grep("php\\?ID\\=",trait)  
  
    for(i in 1:length(lines)){
      peas <- unlist(strsplit(trait[lines[i]],split="\""))
       http <- peas[grep("php\\?ID\\=",peas)]
      http <- sub("..","http://fishbase.org",http)
      URLs[i] <- http
    }

  return(URLs)  
}

#Perform clean-up
growth <- clean_up(growth)
maturity <- clean_up(maturity)
spawning <- clean_up(spawning)
fecundity <- clean_up(fecundity)

#Get reference IDs
get_refs <- function(trait){
  record.url <- c()
  reference.url <- c()
    
  for(i in 1:length(trait)){
    record.html <- readLines(trait[i])  
    record.lines <- grep("php\\?ID\\=",record.html)
    for(j in 1:length(record.lines)){
      peas <- unlist(strsplit(record.html[record.lines[j]],split="\""))
      http <- peas[grep("php\\?ID\\=",peas)]
      http <- sub("/","http://fishbase.org/",http)
      record.url <- append(record.url,http)    
    }
  }
  for(i in 1:length(record.url)){
    reference.html <- readLines(record.url[i])
    reference.lines <- grep("FBRefSummary",reference.html)
    for(j in 1:length(reference.lines)){
      peas <- unlist(strsplit(reference.html[reference.lines[j]],split="\""))
      http <- peas[grep("References",peas)]
      http <- sub("..","http://fishbase.org",http)
      reference.url <- append(reference.url,http)    
    }  
  }  
}


#Prioritize by number of species, observations, and traits

#Print out nicely formatted bibliography