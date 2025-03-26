#' Finds the parent node of a given node in a hierarchy.
#'
#' @param node A character the name of the node for which the parent node is to be found.
#' @param hierarchy A list representing the current hierarchy structure.
#' @return The parent node of the given node in the hierarchy.
#' @keywords internal

find_parent <- function(node,hierarchy){
  return(hierarchy[[node]])
}

