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

GenSaRows <- function(castedDischarge, serviceArea, cutoff) {
  #
  #
  #
  # Args:
  #
  #
  # Returns:
  #
  
  # generate vertical sorter
  sorter <- GenVertSorter(castedDischarge, serviceArea)

  # get sorted service area
  sa_hosps <- castedDischarge[, serviceArea]
  sa_sorted <- apply(as.matrix(sa_hosps), 1, sum)[sorter]

  # get sorted service area and cumulative contribution vectors
  total <- sum(sa_sorted)
  cumCont <- cumsum(sa_sorted)/total

  # find service area
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

  # subset the sorter and return the result
  result <- sorter[sorter%in%saRows]
  return(result)
}

GenPartyZips <- function(castedDischarge, parties) {
  #
  #
  #
  # Args
  #
  #
  # Returns:
  #

  # subset and total across parties
  subset <- castedDischarge[, parties]
  parties_total <- apply(as.matrix(subset), 1, sum)

  # generate vector of index numbers
  party_zips <- which(parites_total>0 & !is.na(parties_total))

  #return
  return(party_zips)
}

GenVertSorter <- function(castedDischarge, serviceArea, shareGroup, partyZips){
  #
  #
  #
  # Args:
  #   castedDischarge: output of the CastDischarges function
  #   serviceArea: vector of the hosp_ids to be included in the service area
  #
  # Returns:
  #

  # subset and total service area of casted dataset
  sa_hosps <- castedDischarge[, serviceArea]
  sa_total <- apply(as.matrix(sa_hosps), 1, sum)

  # subset and total share group of casted dataset
  share_hosps <- castedDischarge[, shareGroup]
  share_total <- apply(as.matrix(share_hosps, 1, sum))

  # subset zip codes
  zips <- castedDischarge[, 1]

  # generate sorter
  sorter <- order(sa_total, share_total, zips)
  result <- sorter[sorter%in%partyZips]

  #return
  return(result)
}

GenHorSorter <- function(castedDischarge, groupedIds, saRows, partyZips){
  #
  #
  #
  # Args:
  #   castedDischarge: output of the CastDischarges function
  #   groupedIds: list of vectors, each containing the ids of a system of
  #     hospitals, ordered as they would appear (left to right) in the patient
  #     origin table
  #   saRows: a vector of index numbers of rows that make up the service area
  #     to be used in the horizontal sort 
  #   partyZips: a vector of index numbers of rows that make up zip codes in
  #     which the merging parties have discharges.
  #
  # Returns:
  #

  # generate horizontal sorter
  result <- list(names(castedDischarge[, 1]))
  for(sys in groupedIds){

    # service area totals
    sa_subset <- castedDischarges[saRows, sys]
    sa_total <- apply(as.matrix(sa_subset), 2, sum)

    # all discharges totals
    party_zip_subset <- castedDischarges[partyZips, sys]
    party_zip_total <- apply(as.matrix(party_zip_subset), 2, sum)

    # names
    hosp_names <- names(castedDischarge[, sys])

    # generate sorter
    sorter <- order(sa_total, party_zip_total, hosp_names)

    # append to result
    result <- c(result, names(sys)[sorter])
  }
  
  #return
  return(result)
}

SplitAndSortCast <- function(castedDischarge, horSorter, vertSorter){
  #
  #
  #
  # Args:
  #
  #
  # Returns: 
  #

  # initialize the result
  result <- list()

  # loop through horSorter and append sorted subset to result
  for (i in 1:length(horSorter)) {
    subset <- castedDischarge[vertSorter, horSorter[[i]]]
    result[i] <- list(subset)
  }

  # return
  return(result)
}

AddHorSummary <- function(formattedCast, horSummaryKey){
  #
  #
  #
  # Args:
  #   foramttedCast: 
  #   horSummaryKey: a list corresponding in length to the groupedIds vector
  #   
  #

  # initialize result
  result <- foramttedCast[[1]]

  # looping through column groupings to generate summary and bind to result
  for(i in 2:length(formattedCast){

    if(is.list(horSummaryKey[[i]]){
      for(j in 1:length(horSummaryKey[[i]])){

      }
    } else {

    }
  }

  #return
  return(result)

}