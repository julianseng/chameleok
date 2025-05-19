#' Add Generalization Relationship to Hierarchy
#'
#' This function adds a generalization relationship to a given hierarchy. 
#' The function modifies the hierarchy list by adding a new 'to' entry for the `from` node.
#' It is typically used internal to the package for managing hierarchical structures when importing a structure from excel.
#' Manual creation/extension of the hierarchy is also possible.
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
