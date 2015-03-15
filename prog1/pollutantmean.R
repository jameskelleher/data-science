pollutantmean <- function(directory, pollutant, id=1:332) {
  files <- list.files(directory)
  sum <- 0
  num <- 0
  for (file in files[id]) {
    relpath <- paste(directory, file, sep='/')
    data <- read.csv(relpath)
    poldata <- data[pollutant]
    poldata_clean <- poldata[!is.na(poldata)]
    sum <- sum + sum(poldata_clean)
    num <- num + length(poldata_clean)
  }
  sum/num
}