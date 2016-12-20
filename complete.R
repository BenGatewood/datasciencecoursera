complete <- function(directory, id = 1:332) {
  
  # Empty vector for results
  results <- list(sulfate=numeric(), nitrate=numeric(), id=numeric)
  
  # Initialise start and end at 0
  start.sulfate <- start.nitrate <- start.id <- 
    end.sulfate <- end.nitrate <- end.id <- 0
  
  # First section: Read in the selected csv files and make a list of dataframes
  # Full list of files in our data directory
  files <- list.files(directory)
  
  # Subset of files we're interested in
  selectedFiles <- files[id]
  
  # Empty list for our dataframes
  frames <- list()
  
  for(index in seq_along(selectedFiles)) {
    
    csv <- read.csv(paste(getwd(), "/",
                          directory, "/", 
                          selectedFiles[[index]], 
                          sep = ""),
                    na.strings = "NA")
    
    frames[[index]] <- csv
    
  }
  
  # Second section: Rip out individual series from the frames and make a big
  # Dataframe from them
  for(index in seq_along(frames)) {
    
    values <- frames[[index]]
    size.sulfate <- length(values$sulfate)
    size.nitrate <- length(values$nitrate)
    size.id <- length(values$ID)
    
    if(size.sulfate > 0) {
      start.sulfate <- end.sulfate + 1
      end.sulfate <- start.sulfate + size.sulfate - 1
      results$sulfate[start.sulfate:end.sulfate] <- values$sulfate
    }
    if(size.nitrate > 0) {
      start.nitrate <- end.nitrate + 1
      end.nitrate <- start.nitrate + size.nitrate - 1
      results$nitrate[start.nitrate:end.nitrate] <- values$nitrate
    }
    if(size.id > 0) {
      start.id <- end.id + 1
      end.id <- start.id + size.id - 1
      results$ID[start.id:end.id] <- values$ID
    }
  }
  
  results <- data.frame(sulfate=unlist(results$sulfate), 
                        nitrate=unlist(results$nitrate),
                        id=unlist(results$ID))
  
  # Boolean vector where TRUE = complete set of results:
  completeBool <- complete.cases(results)
  
  completeCases <- results[completeBool, ]
  
  obsCount <- list()
  
  for(i in id) {
    
    obsCount$id[i:i] <- i
    obsCount$nobs[i:i] <-  nrow(completeCases[completeCases$id == i, ])
  }
  
  completeTally <- data.frame(id=unlist(obsCount$id),
                              nobs=unlist(obsCount$nobs))
  
  completeTally
}

