OverlapStandard <- function(x){
  result <- as.numeric(length(x[x>=4]) == length(x))
  return(result)
}