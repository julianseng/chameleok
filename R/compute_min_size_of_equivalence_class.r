#' Compute the Minimum Set Size of an Equivalence Class
#'
#' This function calculates the minimum frequency records that belong to the same constellation of quasi-identifiers (i.e. equivalence class).
#' It is useful for evaluating k-anonymity, where k is the minimum size of any equivalence class.
#'
#' @param data A data frame or data table containing the quasi-identifiers
#' @param quid A vector of column names or indices representing quasi-identifiers
#'
#' @return The minimum frequency (k) of any combination of quasi-identifier values
#'
#' @examples
#' # For a data frame with columns "age", "gender", "zipcode"
#' # To compute min size where "age" and "zipcode" are quasi-identifiers:
#' # compute_min_size_of_equivalence_class(my_data, c("age", "zipcode"))
#' 
#' @export 
compute_min_size_of_equivalence_class <- function(data, quid){
  freqs <- table(data[quid])
  freqs <- freqs[freqs != 0]
  min_k <- min(freqs)
  return(min_k)
}
