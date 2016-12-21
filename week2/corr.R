corr <- function(directory, threshold = 0) {
  # Full list of files in our data directory
  files <- list.files(directory)
  
  # Empty list for the csv files to be read into
  csvList <- list()
  
  # Loop through files that match "id" and read them in
  # Then, pop them all in the list we initialised earlier
  for(f in files) {
    csv <- read.csv(paste(getwd(), "/", directory, "/", f, sep = ""),
                    na.strings = "NA")
    csvList[[f]] <- csv
    
  }
  
  # Take our list of dataframes and munge them into one, big dataframe
  dataFrame <- data.frame(Reduce(rbind, csvList))
  
  # Boolean Vector of complete rows
  completeCases <- complete.cases(dataFrame)
  
  # Subset of dataFrame with only complete rows
  completes <- dataFrame[completeCases,]
  
  completeCounts <- complete("specdata")
  
  overThreshold <- completeCounts[completeCounts$nobs > threshold, 1]
  
  overBool <- completes[completes$ID %in% overThreshold,]
  
  print(overBool)
}