## Piping functionality
installed=require(magrittr)
if(!installed){install.packages("magrittr")}

library(magrittr)


### File to read in
dataFile = "./data/NEON MeOH 2019 Report.csv"

### Where to save the CSVs out
saveDir = "./data/"

### The report type we're looking at (goes into the saved file name)
reportType = "MeOH"


#### THE SCRIPT STARTS ####
# Read in the file. This will likely take a while
rawData = read.csv(dataFile, stringsAsFactors = FALSE)

# Figure out what columns are sample IDs
sampleIDs= colnames(rawData)[grep(pattern = "[A-Z]{4}_", x = colnames(rawData))]

#reportType = unlist(strsplit(sampleIDs[1], split = "_"))[length(unlist(strsplit(sampleIDs[1], split = "_")))]

# Figure out what columns are measurements/key info
keyColumns=colnames(rawData)[!colnames(rawData) %in% sampleIDs]

# Filter to recors where class and a signal exists (and name them)
lapply(sampleIDs, function(id) rawData[which(rawData$Class!="None"&rawData[,id]!=0),c(keyColumns, id)]) %>% 
  `names<-`(sampleIDs) -> filteredData

# Define a function to generate a data frame of NOSC values on a El_comp - Class basis
generateStats=function(sampleDF){
  groupingTable=unique(sampleDF[,c("El_comp","Class")])
  subsetSampleDF=lapply(
    seq(nrow(groupingTable)), 
    function(i) sampleDF[(sampleDF$El_comp==groupingTable$El_comp[i]&sampleDF$Class==groupingTable$Class[i]),]
  )
  sampleDFOut=data.frame()
  for(i in seq(length(subsetSampleDF))){
    tempDF=subsetSampleDF[[i]]
    tempDF$NOSC=-(((tempDF$C*4)+tempDF$H-(3*tempDF$N)-(2*tempDF$O)+(5*tempDF$P)-2*tempDF$S)/tempDF$C)+4
    
    sample=colnames(tempDF)[grep(pattern = "[A-Z]{4}_", x = colnames(tempDF))]
    El_comp=unique(tempDF$El_comp)
    Class=unique(tempDF$Class)
    StdDev=stats::sd(tempDF$NOSC)
    Mean_NOSC=mean(tempDF$NOSC)
    Min_NOSC=min(tempDF$NOSC)
    Max_NOSC=max(tempDF$NOSC)
    statDF=data.frame(Sample=sample, Class=Class, El_comp=El_comp, Mean_NOSC=Mean_NOSC, Min_NOSC=Min_NOSC, Max_NOSC=Max_NOSC, StDev_NOSC=StdDev)
    sampleDFOut=rbind(sampleDFOut, statDF)
  }
  return(sampleDFOut)  
}

# apply the function to each data frame in filtered data
granularStats=lapply(filteredData, generateStats)

# stack the stats into one data table
output1=as.data.frame(do.call(rbind, granularStats))

# Make the "high level" stats - just sample level summaries
output2=data.frame()
for(id in sampleIDs){
  tempDF=output1[output1$Sample==id,]
  sample=id
  Mean_NOSC=mean(tempDF$Mean_NOSC)
  Min_NOSC=min(tempDF$Min_NOSC)
  Max_NOSC=max(tempDF$Max_NOSC)
  statDF=data.frame(Sample=sample, Mean_NOSC=Mean_NOSC, Min_NOSC=Min_NOSC, Max_NOSC=Max_NOSC)
  output2=rbind(output2, statDF)
}

# Save our files out
write.csv(x=output1, file = paste0(saveDir, "/", reportType, "_granularStats.csv"), row.names = FALSE)
write.csv(x=output2, file = paste0(saveDir, "/", reportType, "_sampleStats.csv"), row.names = FALSE)

