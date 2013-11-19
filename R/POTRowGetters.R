SARows <- function(cumCont, sortCol, cutoff) {
  #
  # Args:
  #
  #
  # Returns:
  #
  
  saRows <- c()
  for (i in length(1:cumCont)) {
    if (cumCont[i] < cutoff) {
      saRows[i] <- i
    } else if (cumCont[i] > cutoff & cumCont[i-1] < cutoff) {
      saRows[i] <- i
    } else if (sortCol[i] == sortCol[i-1]) {
      saRows[i] <- i
    } else {
      break
    }
  }
  return(saRows)
}

GetZipsByCounty <- function() {
  #
  # Args:
  # 
  #
  # Returns:
  #
  #
}