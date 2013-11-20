AddLeadingZeroes <- function(vector, numCharacters) {
  #
  # Cooerces the vector to class character and adds leading zeroes to any value
  # such that all values are of length numCharacters
  #
  # Args:
  #   vector: the input vector to be changed
  #
  # Returns:
  #   a character vector, each of whose values are of length numCharacters
  
  toN <- function(x, n){
    if (nchar(x) >= n) {
      return(substring(x, nchar(x)-n+1, nchar(x)))
    } else {
      return(paste(rep("0", n-nchar(x)), x, sep=""))
    }
  }
  result <- sapply(as.list(vector), toN, n=numCharacters)
  return(result)
}