#Get everything we can from FishBase and then burn it to the ground!
#Load up libraries we may need
library(XML)
library(RCurl)

#Read in the HTML fecundity list file we generated from FishBase earlier
htmlList <- readLines(file.choose())

#Generate our list of species URLs
#Which lines of HTML have our species URLs?
lines <- grep("FecundityList",htmlList)

#Clean the lines of HTML up so we just get the URL

URLs <- NA
for(i in 1:length(lines)){
  peas <- unlist(strsplit(htmlList[lines[i]],split="\""))
  http <- peas[grep("http",peas)]
  URLs[i] <- http
}

#Fuck yeah!  It works!  Now on to the hard part of extracting the HTML tables into a data frame.  We have to physically go and check the dimensions of the HTML tables we are extracting since they are formed really crappily.  

#The fecundity tables are 9 columns + the fishbase url
Country <- c()
Locality <- c()
AbsFecMin <- c()
AbsFecMax <- c()
RelFecMin <- c()
RelFecMean <- c()
RelFecMax <- c()
FecLenA <- c()
FecLenB <- c()
FishBaseURL <- c()


for(i in 1:length(URLs)){
  before <- length(Country)
  tbl <- readHTMLTable(URLs[i])[2]
  #Stupid unstructured table!
  tbl <- tbl$`NULL`[-c(1:2), ]
  #we have to clean up blanks
  blanks <- tbl == ""
  tbl <- replace(tbl,blanks,NA)
  
  #Now we do the painful extracting
    Country <- append(Country,as.character(tbl[,1]))
    Locality <- append(Locality,as.character(tbl[,2]))
    AbsFecMin <- append(AbsFecMin,as.character(tbl[,3]))
    AbsFecMax <- append(AbsFecMax,as.character(tbl[,4]))
    RelFecMin <- append(RelFecMin,as.character(tbl[,5]))
    RelFecMean <- append(RelFecMean,as.character(tbl[,6]))
    RelFecMax <- append(RelFecMax,as.character(tbl[,7]))
    FecLenA <- append(FecLenA,as.character(tbl[,8]))
    FecLenB <- append(FecLenB,as.character(tbl[,9]))
    FishBaseURL[(before+1):length(Country)] <- URLs[i]
    print(i)
}

#Bind it all togethor
fecundity <- cbind(Locality,Country,AbsFecMin,AbsFecMax,RelFecMin,RelFecMean,RelFecMax,FecLenA,FecLenB,FishBaseURL)

f <- data.frame(fecundity) 

#Number of species with 5 or more records
t <- table(fecundity[,ncol(fecundity)])
t <- data.frame(t)

#Get the data for species with 3 or more observations

t3 <- t[t$Freq >= 3, ]
match.index <- match(f$FishBaseURL,t3$Var1)
f3 <- f[match.index > 0, ]

#Get the references 
references <- c()
for(i in 1682:length(URLs)){
print(i)  
page <- readLines(URLs[i])
refs.ind <- grep("FishFecunditySummary",page)

  ref.urls <- NA
  for(j in 1:length(refs.ind)){
    peas <- unlist(strsplit(page[refs.ind[j]],split="\""))
    http <- peas[grep("FishFecundity",peas)]
    ref.urls[j] <- http
    ref.urls[j] <- sub("..","http://www.fishbase.org",ref.urls[j])
      subpage <- readLines(URLencode(ref.urls[j]))
      Encoding(subpage) <- "latin1"
      subpage.ind <- grep("FBRefSummary",subpage)[1]    
      carrots <- unlist(strsplit(subpage[subpage.ind],split="\'"))[2]
      ref.ID <- paste("http://www.fishbase.org",carrots,sep="") 
      ref.page <- readLines(ref.ID) 
      ref <- ref.page[grep("Citation",ref.page)+2]
      if(length(ref) == 0) ref <- "NO REFERENCE"
    references <- append(references,ref)
  }
}

#Clean the references up
references <- gsub("\t\t\t\t<b>","",references)
references <- gsub("\t\t\t</td>","",references)

#Add the references to the table and export
results <- cbind(fecundity,references)
Encoding(results) <- "latin1"
write.csv(results,"/Users/simonbrown/Desktop/FishBase_Fecundity.csv")














