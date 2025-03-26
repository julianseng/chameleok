#' Check if a dataset is k-anonymized
#' 
#' This function verifies whether a dataset satisfies k-anonymity by counting the occurrences 
#' of each quasi-identifier combination and checking if all combinations appear at least k times.
#' 
#' @param data A data frame or matrix containing the dataset to be checked
#' @param quid A vector of column names or indices representing the quasi-identifiers
#' @param k The minimum number of occurrences required for k-anonymity
#' 
#' @return A logical value: TRUE if the dataset is k-anonymous, FALSE otherwise
#' 
#' @details 
#' The function groups records by the specified quasi-identifier columns and counts the number 
#' of records in each group. If all groups have at least k records, the dataset satisfies k-anonymity.
#' 
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   age = c(25, 25, 30, 40, 40, 40),
#'   zipcode = c("12345", "12345", "12345", "23456", "23456", "23456")
#' )
#' 
#' # Check if data is 2-anonymous with age and zipcode as quasi-identifiers
#' isKAnonymouse(sample_data, c("age", "zipcode"), 2)
#' 
#' @export

isKAnonymous <- function(data, quid, k){
if(!is.data.frame(data)){
  stop("data must be a data frame")
}
if(!is.character(quid) && !is.numeric(quid)){
  stop("quid must be a character vector or numeric vector")
}
if(!is.numeric(k) || k < 1){
  stop("k must be a positive integer")
}
# Check for valid quasi-identifiers
if(length(quid) == 0) {
  stop("quid must contain at least one column")
}

if(is.numeric(quid)) {
  if(any(quid <= 0) || any(quid > ncol(data))) {
    stop("numeric quid must be valid column indices")
  }
} else {
  if(!all(quid %in% colnames(data))) {
    stop("quid must contain valid column names")
  }
}
if(length(quid)==1){
  warning("Only one quasi-identifier specified. Consider using more columns for better anonymization.")
}

min_k <- compute_min_size_of_equivalence_class(data, quid)
if(min_k >= k){
  message(paste0("The data is ", min_k, "-anonymouse for the quid: ", paste(quid, sep =", ", collapse = " and ")))
  return(TRUE)

}
else{
  message(paste0("The data is not ", k, "-anonymouse. The minimum size of a equivalence class is ", min_k))
  return(FALSE)
}
}


