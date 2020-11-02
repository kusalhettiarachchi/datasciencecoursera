complete <- function(directory, id = 1:332) {
  obs = data.frame()
  for (i in id) {
    j <- i
    if (i < 10) {
      i <- paste("00", i, sep = "")
    } else if (i < 100) {
      i <- paste("0", i, sep = "")
    }
    file <- paste(i, ".csv", sep="")
    path <- paste(directory, file, sep="/")
    df <- read.csv(path)
    both <-!is.na(df$nitrate) & !is.na(df$sulfate)
    d <- c(j,length(both[both == TRUE]))
    obs <- rbind(obs, d)
  }
  colnames(obs) <- c("id", "nobs")
  obs
}