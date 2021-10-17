library(readr)
library(plyr)


pollutantmean <- function(directory, pollutant, id=1:332) {
  ## directory is the location where the csv files are stored on disk
  ## pollutant : {sulfate, nitrate}
  ## id is an integer vector indicating the monitor ID numbers to be used
  print(directory)
  print(pollutant)
  print(id)
  
  myfiles = list.files(path=directory, pattern="*.csv", full.names=TRUE)
  data_csv = ldply(myfiles[id], read_csv)
  mean(data_csv[[pollutant]], na.rm=TRUE)
  
  ## return the mean of the pollutant across all monitors list in the id vector
}
