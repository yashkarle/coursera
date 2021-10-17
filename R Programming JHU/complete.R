complete <- function(directory, id=1:332) {
  myfiles <- list.files(directory, full.names=TRUE)
  
  counts <- sapply(id, function(i) {
    data <- read.csv(myfiles[i])
    sum(complete.cases(data))
  })
  
  data.frame(id=id, nobs=counts)
}
