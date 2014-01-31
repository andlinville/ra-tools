GenOverlapKey <- function(moltenDischarge,
                          overlapIds=list(),
                          overlapDefinition) {
  #
  # 
  #
  # Args:
  #   moltenDischarge: the output of MeltDischarge function
  #   overlapIds: a list of vectors of hospital ids, one vector for each
  #     system to be included in the overlap
  #   overlapDefinition: the function used to define the overlap
  #
  # Returns:
  #   
  
  # subset molten discharge to relevant columns
  subset <- moltenDischarge[, c('discharge_id', 'msdrg', 'hosp_id', 'variable', 'value')]
  
  # cast to get discharges by msdrg and hosp
  casted <- dcast(subset, msdrg ~ hosp_id, sum)
  
  # total across systems and return
  total <- casted$msdrg
  for(i in overlapIds){
    subset <-data.frame(casted[, i])
    total <- data.frame(total, s=apply(as.matrix(subset), 1, FUN=function(x) as.numeric(sum(x))))
  }
  total <- total[, 2:ncol(total)]
  
  # generate overlap dummy
  overlap <- apply(total, 1, FUN=OverlapStandard)
  
  # bind to msdrgs and return
  msdrg <- casted$msdrg
  overlapKey <- cbind(msdrg, overlap)
  return(overlapKey)
}

GenPayerKey <- function(moltenDischarge, commercialPayers) {
  #
  # Returns a key of commercial payers, to be used as input for the
  # CastDischarge function
  #
  # Args:
  #   moltenDischarge: the molten discharge data, output of MeltDischarge
  #   commercialPayers: a vector commercial payers, specified by the payer_d
  #     field
  #
  # Returns:
  #   a data frame containing the payer descriptions and a dummy flagging
  #   commercial payers
  
  uniquePayers <- unique(moltenDischarge['payer'])
  commercialDummy <- lapply(uniquePayers, function(x){
                              as.numeric(x %in% commercialPayers)
                            })
  names(commercialDummy) <- c('commercial')
  result <- data.frame(uniquePayers, commercialDummy)
  return(result)
}

GenHospKey <- function(moltenDischarge) {
  #
  #
  #
  # Args:
  #
  #
  # Returns:
  
  # subsetting molten data
  subset <- moltenDischarge[, c('hosp_id', 'hosp_name')]
  
  # dropping duplicates
  result <- unique(subset)
  result <- result[order(result$hosp_name), ]
  
  # checking if ids or names are duplicated
  uniqueIds <- unique(moltenDischarge$hosp_id)
  uniqueNames <- unique(moltenDischarge$hosp_name)
  if (nrow(result) > length(uniqueIds)) {
    warning('The hospital key contains more rows than the number of unique hospital ids.')
  }
  if (nrow(result) > length(uniqueNames)) {
    warning('The hospital key contains more rows than the number of unique hospital names.')
  }  
  
  # returning the result
  return(result)
}