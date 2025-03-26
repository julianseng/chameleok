#' Generalize a child node to its parent node in a given attribute
#' 
#' @param attribute A factor attribute
#' @param node A child node to be generalized
#' @param Parent A parent node to generalize the child node to
#' 
#' @return A factor attribute with the child node generalized to the parent node
#' 
#' @keywords internal

generalize_child <- function(attribute, node, Parent){
  levels(attribute)[levels(attribute)==node] <- Parent
  return(attribute)
}

