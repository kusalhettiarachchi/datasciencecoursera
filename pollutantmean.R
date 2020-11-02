pollutantmean <- function(directory, pollutant, id = 1:332) {
  df <- data.frame()
  for (i in id) {
    if (i < 10) {
      i <- paste("00", i, sep = "")
    } else if (i < 100) {
      i <- paste("0", i, sep = "")
    }
    file <- paste(i, ".csv", sep="")
    path <- paste(directory, file, sep="/")
    df <- rbind(df, read.csv(path))
  }  
  mean(df[[pollutant]], na.rm=TRUE)
}