MeltDischarge <- function(dischargeRaw, fieldNames, dischargeId = FALSE, rowIsDischarge = TRUE) {
  # Extracts relevant fields from discharge data and melts it.
  #
  # Args:
  #   dischargeRaw: the discharge dataset as a data frame
  #   fieldNames: a vector of the names of the following fields:
  #       0. patient zip code
  #       1. msdrg
  #       2. hospital id
  #       3. hospital name
  #       4. payer description
  #       5. discharge id
  #       6. discharge count
  #     If either discharge_id or discharge_count are implied in the data, their
  #     names should be excluded from the fieldNames argument and the dischargeId
  #     and rowIsDischarge arguments should be set appropriately (see below).
  #   dischargeId: should be FALSE (default) if a field containing a unique
  #     identifier for each discharge is included in the fieldNames vector.
  #     otherwise the field dischargeId should be TRUE.
  #     rowIsDischarge: should be TRUE if each row in the data is considered a
  #     unique discharge. Otherwise this argument should be TRUE and a field
  #     containing a count of discharges for each row should be included in the
  #     fieldNames vector.
  #
  # Returns:
  #   a molten data frame of only those fields from the discharge data that are
  #   relevant for a POT
  
  require(plyr)
  require(reshape2)

  print(class(dischargeRaw))
  # check to see if fieldNames has been properly specified
  if (rowIsDischarge & dischargeId) {
    if (length(fieldNames) != 6) {
      len = length(fieldNames)
      message = paste('Expected 6 values in the fieldnames vector, found', as.character(len), 'values', sep = " ")
      stop(message)
    }
  } else if (rowIsDischarge & !dischargeId) {
    if (length(fieldNames) != 5) {
      len = length(fieldNames)
      message = paste('Expected 5 values in the fieldnames vector, found', as.character(len), 'values', sep = " ")
      stop(message)
    }
  } else if (!rowIsDischarge & !dischargeId) {
    if (length(fieldNames) != 6) {
      len = length(fieldNames)
      message = paste('Expected 6 values in the fieldnames vector, found', as.character(len), 'values', sep = " ")
      stop(message)
    }
  } else if (!rowIsDischarge & dischargeId) {
    if (length(fieldNames) != 7) {
      len = length(fieldNames)
      message = paste('Expected 7 values in the fieldnames vector, found', as.character(len), 'values', sep = " ")
      stop(message)
    }
  }
  
  # store new field names in a vector
  updatedFields <- c('patient_zip', 'msdrg', 'hosp_id', 'hosp_name', 'payer', 'discharge_id', 'discharge_count')
  
  # subset the raw data and update field names
  dataSlice <- dischargeRaw[, fieldNames]
  
  # attach discharge id and discharge count fields
  if (!dischargeId) {
    dataSlice$discharge_id <- as.character(c(1:nrow(dataSlice)))
  }
  if (rowIsDischarge) {
    dataSlice$discharge_count <- c(rep(1, nrow(dataSlice)))
  }
  
  # melt the subsetted data and return the result
  names(dataSlice) <- updatedFields
  result <- melt(dataSlice,
                 id = c('discharge_id', 'hosp_id', 'hosp_name', 'patient_zip', 'msdrg', 'payer'),
                 measured = c('discharge_count'))
  return(result)
}

CastDischarge <- function(moltenDischarge, payerKey, overlapKey, ...) {
  # Takes in the output of the Discharge Extract function, and reshapes for 
  # discharges by zip code (vertical) and hospital (horizontal).
  #   
  # Args:
  #   dischargeExtract: a data.frame whose observations are discharges and 
  #   whose fields are those extracted in the DischargeExtract function. Field
  #   names must be the same as those returned by the DischargeExtract 
  #   function.
  #   msdrgSubset: one a few options to specify the subset of msdrgs to be
  #   included in the cast.
  #
  # Returns:
  #   a data.frame of discharges by zip code and hospital, for the specified
  #   subset of msdrgs.
  
  require(plyr)
  require(reshape2)
  
  # load and merge in zip code key and msdrg key
  load("data/msdrgKey.Rdata")
  load("data/zipKey.Rdata")
  
  dataWithZipKey <- merge(moltenDischarge,
                          zipKey,
                          by.x = c("patient_zip"),
                          by.y = c("zip_code"),
                          sort = FALSE,
                          all.x = TRUE) 
  dataWithMsdrg <- merge(dataWithZipKey,
                        msdrgKey,
                        by = c("msdrg"),
                        sort = FALSE,
                        all.x = TRUE)
  dataWithPayerCodes <- merge(dataWithMsdrg,
                        payerKey,
                        by= c('payer'),
                        sort = FALSE,
                        all.x = TRUE,)
  dataWithOverlapCode <- merge(dataWithPayerCodes,
                    overlapKey,
                    by = c('msdrg'),
                    sort = FALSE,
                    all.x = TRUE)
  completeData <- dataWithOverlapCode
  
  # subset the data
  View(completeData)
  attach(completeData)
  subset <- completeData[..., ]
  detach(completeData)
  
  # cast the subset and return the result
  result <- dcast(subset, patient_zip ~ hosp_id, sum)
  return(result)
}