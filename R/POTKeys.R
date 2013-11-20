GenOverlapKey <- function(dischargeRaw) {
  
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