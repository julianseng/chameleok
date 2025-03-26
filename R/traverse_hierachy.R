#' Traverse hierachy
#' 
#' This is an internal function that generalizes all children of a parent node according to a specified hierarchy.
#' 
#' @param children 
#' @param attribute 
#' @param hierarchy A list representing the current hierarchy structure.
#' @return A modified attribute list with the new generalization relationship added.
#' @keywords internal
#' @export

traverse_hierachy <- function(children, attribute, hierarchy){
  # 1. Input children  to be generalized
  # 2. Find parent P of this child
  P <- find_parent(children, hierarchy)
  # 3. Find all children of P
  childrens <- find_childrens(node=P, hierarchy)
  # 4. Generalize all childrens of P incl. child from 1.
  if(!is.null(childrens)){
    attribute <- generalize_childrens(childrens, P, attribute, hierarchy)
  }
  # 5. END
  return(attribute)
}


