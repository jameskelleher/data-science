corr <- function(directory, threshold=0) {
  files <- list.files(directory)
  cor_vec <- c()
  data_complete <- complete(directory)
  nobs <- data_complete$nobs
  ids <- data_complete$id[nobs > threshold]
  for (file in files[ids]) {
    relpath <- paste(directory, file, sep='/')
    data <- read.csv(relpath)
    sulf <- data$sulfate
    nit <- data$nitrate
    if (length(sulf) != length(nit))  {
      print("Error with id:")
      print(data$id[1])
      break
    }
    sulfmask <- !is.na(sulf)
    nitmask <- !is.na(nit)
    mask <- sulfmask & nitmask
    cor_result <- cor(sulf[mask], nit[mask])
    cor_vec <- c(cor_vec, cor_result)
  }
  cor_vec
}