GroupSystem <- function(castedDischarge, hosps, sysName) {
  #
  # Takes in discharge data and a vector of hospital ids and returns the
  # hospital discharge fields and a system total as a data frame
  #
  # Args:
  #   castedDischarge: the result of the CastDischarge function, or, more
  #     generally, a raw (unformatted) table of discharges by zip code
  #     (vertical) and hospital (horizontal)
  #   hosps: vector of ids of hospitals in the system
  #
  # Returns:
  #   a data frame of the hospitals  
  
  sys <- castedDischarge[hosps]
  total <- apply(as.matrix(sys), 1, sum, na.rm = FALSE)
  result <- cbind(sys, total)
  names(result) <- c(names(sys), sysName)
  return(result)
}

GenSorter <- function(castedDischarge, sortCol) {
  #
  #
  #
  #
  # Args:
  #
  #
  # Returns:
  #
  
  sorter <- order(sortCol, decreasing=TRUE)
  return(sorter)
}

GenPotCols <- function(sortedSortCol, sortedTotal){
  #
  # 
  #
  # Args:
  #
  #
  # Returns:
  #
  
  # generate cols
  zipCont <- apply(as.matrix(sortedSortCol), 1, sum, na.rm=TRUE) / sum(sortedSortCol)
  zipCumCont <- cumsum(zipCont)
  zipShare <- sortedSortCol / sortedTotal
  zipCumShare <- cumsum(zipShare) / cumsum(sortedTotal)
  
  # bind together cols and return the result
  result <- data.frame(zip_cont = zipCont,
                       zip_cum_cont = zipCumCont,
                       zip_share = zipShare,
                       zip_cum_share = zipCumShare)
  return(result)
}

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

GenPotSummary <- function(data, sa75Rows, sa90Rows) {
  #
  #
  #
  #
  # Args: 
  #
  #
  # Returns:
  #
  
  
}