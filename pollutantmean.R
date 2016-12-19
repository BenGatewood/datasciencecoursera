pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        for(i in id) {
          csvFile <- paste(getwd(), "/", directory, "/", i, ".csv", sep = "")
          print(csvFile)
        }
        
}