#' Add Generalization Relationship to Hierarchy
#'
#' This function adds a generalization relationship to a given hierarchy. 
#' It assigns a parent node to a specified node in the hierarchy.
#'
#' @param from A character or numeric value indicating the node to be generalized.
#' @param to A character or numeric value indicating the parent node to which the `from` node will be generalized.
#' @param hierachy A list representing the current hierarchy structure.
#' @return A modified hierarchy list with the new generalization relationship added.
#' @keywords internal
#' @examples
#' # Example usage:
#' hierarchy <- list()
#' hierarchy <- add_gen("child", "parent", hierarchy)
#' print(hierarchy)
#' @export

add_gen <- function(from, to, hierachy){
  hierachy[from] <- list(parent=to)
  return(hierachy)  
}
