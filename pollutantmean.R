pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Full list of files in our data directory
  files <- list.files(directory)
  
  # Empty list for the csv files to be read into
  csvList <- list()
  
  # Don't think I actually need this
  colNames <- c("Date","sulfate","nitrate","ID")
  
  # Loop through files that match "id" and read them in
  # Then, pop them all in the list we initialised earlier
  for(f in files[id]) {
    csv <- read.csv(paste(getwd(), "/", directory, "/", f, sep = ""),
                    na.strings = "NA")
    csvList[[f]] <- csv
    
  }
  
  
  # Take our list of dataframes and munge them into one, big dataframe
  dataFrame <- data.frame(Reduce(rbind, csvList))
  
  # Extract the column we're interested in
  pollutantVec <- dataFrame[pollutant]
  
  # Compute the mean, disregarding NAs
  mu <- colMeans(pollutantVec, na.rm = TRUE)
  print(mu)
  
}

complete <- function(directory, id = 1:332) {
  # Full list of files in our data directory
  files <- list.files(directory)
  
  # Empty list for the csv files to be read into
  csvList <- list()
  
  # Don't think I actually need this
  colNames <- c("Date","sulfate","nitrate","ID")
  
  # Loop through files that match "id" and read them in
  # Then, pop them all in the list we initialised earlier
  for(f in files[id]) {
    csv <- read.csv(paste(getwd(), "/", directory, "/", f, sep = ""),
                    na.strings = "NA")
    csvList[[f]] <- csv
    
  }
  
  
  # Take our list of dataframes and munge them into one, big dataframe
  dataFrame <- data.frame(Reduce(rbind, csvList))
  
  completeCases <- complete.cases(dataFrame)
  
  completes <- dataFrame[completeCases]
  
  print(completes)
}


