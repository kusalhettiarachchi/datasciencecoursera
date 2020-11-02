corr <- function(directory, threshold = 0) {
  comps <- complete(directory, 1:332)
  corrs <- c()
  calc_corr <- function(i) {
    if (i < 10) {
      i <- paste("00", i, sep = "")
    } else if (i < 100) {
      i <- paste("0", i, sep = "")
    }
    file <- paste(i, ".csv", sep="")
    path <- paste(directory, file, sep="/")
    df <- read.csv(path)
    cor(df$sulfate, df$nitrate, use="complete.obs")
  }
  for (set in rownames(comps)) {
    if (comps[set, 2] > threshold) {
      corrs <- c(corrs, calc_corr(comps[set, 1]))
    }
  }
  corrs
}