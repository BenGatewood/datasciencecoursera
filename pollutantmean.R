pollutantmean <- function(directory, pollutant, id = 1:332) {
  
        files <- list.files(directory)
        csvList <- list()
        colNames <- c("Date","sulfate","nitrate","ID")
        
        for(f in files[id]) {
          csv <- read.csv(paste(getwd(), "/", directory, "/", f, sep = ""),
                          na.strings = "NA")
          csvList[[f]] <- csv

        }
        
         
    dataFrame <- data.frame(Reduce(rbind, csvList))
    
    print(dataFrame)
  
}