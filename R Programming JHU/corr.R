corr <- function(directory, threshold=0) {
  myfiles <- list.files(directory, full.names=TRUE)
  
  id = 1:332
  counts <- sapply(id, function(i) {
    data <- read.csv(myfiles[i])
    comp_cases <- sum(complete.cases(data))
    
    #print(comp_cases)
    
    if (comp_cases > threshold) {
      cor(data[['sulfate']], data[['nitrate']], use="complete.obs")
    }
  })
  
  unlist(counts)
}
