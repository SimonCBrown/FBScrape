#Growth url path to references
#Species page
http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=22903
#To many trait pages
http://www.fishbase.org/PopDyn/FishPopGrowthSummary.php?ID=18177
#To main ref page
http://www.fishbase.org/References/FBRefSummary.php?ID=75334

#read in html list
species.list <- readLines(file.choose())

#Get the lines with the species page URLs
lines <- grep("PopGrowthList",species.list)

#Clean it up
URLs <- NA
for(i in 1:length(lines)){
  peas <- unlist(strsplit(species.list[lines[i]],split="\""))
  http <- peas[grep("PopDyn",peas)]
  URLs[i] <- http
}

