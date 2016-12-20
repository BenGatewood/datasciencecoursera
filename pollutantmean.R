pollutantmean <- function(directory, pollutant, id = 1:332) {

  # Empty vector for results
  results <- list(sulfate=numeric(), nitrate=numeric())
  
  # Initialise start and end at 0
  start.sulfate <- start.nitrate <- end.sulfate <- end.nitrate <- 0
  
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
  }
  
  results <- data.frame(sulfate=unlist(results$sulfate), nitrate=unlist(results$nitrate))
  
  mu <- colMeans(results, na.rm = TRUE)
  
  mu[pollutant]

}

