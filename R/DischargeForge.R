MeltDischarge <- function(dischargeRaw, fieldNames, dischargeId = FALSE, rowIsDischarge = TRUE) {
  # Extracts relevant fields from discharge data and melts it.
  #
  # Args:
  #   dischargeRaw: the discharge dataset as a data frame
  #   fieldNames: a vector of the names of the following fields:
  #     0. patient_zip
  #     1. msdrg
  #     2. hospital_id
  #     3. discharge_id
  #     4. discharge_count
  #   If either discharge_id or discharge_count are implied in the data, their
  #   names should be excluded from the fieldNames argument and the dischargeId
  #   and rowIsDischarge arguments should be set appropriately (see below).
  #   dischargeId: should be FALSE (default) if a field containing a unique
  #   identifier for each discharge is included in the fieldNames vector.
  #   otherwise the field dischargeId should be TRUE.
  #   rowIsDischarge: should be TRUE if each row in the data is considered a
  #   unique discharge. Otherwise this argument should be TRUE and a field
  #   containing a count of discharges for each row should be included in the
  #   fieldNames vector.
  #
  # Returns:
  #   a molten data frame of only those fields from the discharge data that are
  #   relevant for a POT
  
  require(plyr)
  require(reshape2)
  
  # check to see if fieldNames has been properly specified
  if (rowIsDischarge & dischargeId) {
    if (length(fieldNames) != 4) {
      len = length(fieldNames)
      message = 'Expected 4 values in the fieldnames vector, found ' + len + ' values'
      stop(message)
    }
  } else if (rowIsDischarge & !dischargeId) {
    if (length(fieldNames) != 3) {
      len = length(fieldNames)
      message = 'Expected 3 values in the fieldnames vector, found ' + len + ' values'
      stop(message)
    }
  } else if (!rowIsDischarge & !dischargeId) {
    if (length(fieldNames) != 4) {
      len = length(fieldNames)
      message = 'Expected 4 values in the fieldnames vector, found ' + len + ' values'
      stop(message)
    }
  } else if (!rowIsDischarge & dischargeId) {
    if (length(fieldNames) != 5) {
      len = length(fieldNames)
      message = 'Expected 5 values in the fieldnames vector, found ' + len + ' values'
      stop(message)
    }
  }
  
  # store new field names in a vector
  updatedFields <- c('patient_zip', 'msdrg', 'hosp_id', 'discharge_id', 'discharge_count')
  
  # subset the raw data and update field names
  dataSlice = data[fieldNames]
  
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
                 id = c('discharge_id', 'hosp_id', 'zipcode', 'msdrg'),
                 measured = c('discharge_count'))
  return(result)
}

CastDischarge <- function(moltenDischarge, msdrgSubset = "all") {
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
  
  # merge in zip code key and msdrg key
  dataWithZipKey <- moltenDischarge
  fields <- names(dataWithZipKey)
  
  # subset the data
  completeData <- dataWithZipKey
  subset <- completeData[fields[fields != 'msdrg']]
  
  # cast the subset and return the result
  result <- dcast(subset, zipcode ~ hosp_id, sum)
  return(result)
}