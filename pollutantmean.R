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
  
  # Boolean Vector of complete rows
  completeCases <- complete.cases(dataFrame)
  
  # Subset of dataFrame with only complete rows
  completes <- dataFrame[completeCases,]
  
  groupList <- list()

  for(i in id) {
    grouped <- completes[completes$ID == i, ]

    groupList[[i]] <- c(i, nrow(grouped))
  }
  
  groupFrame <- data.frame(Reduce(rbind, groupList),
                           row.names=NULL)
  colnames(groupFrame) <- c("ID", "nobs")
  
  print(groupFrame)
}


