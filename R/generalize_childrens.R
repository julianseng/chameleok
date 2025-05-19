#' Generalize Children Nodes in a Hierarchical Structure
#'
#' This recursive function generalizes attribute values based on a hierarchical structure by
#' traversing through children nodes and replacing specific values with their parent value.
#'
#' @param childrens A vector of child node names to be generalized
#' @param Parent The parent node name that will replace the child values
#' @param attribute A factor vector containing the values to be generalized
#' @param hierarchy A hierarchical structure (typically a list) that defines the parent-child relationships
#'
#' @return The modified attribute factor with generalized values
#'
#' @details
#' The function recursively traverses through the hierarchy, replacing child values with their parent.
#' It first looks for sub-children of each node, and if found, generalizes those before
#' generalizing the current node. This ensures proper bottom-up generalization following
#' the hierarchical structure.
#'
#' @seealso \code{\link{generalize_child}}, \code{\link{find_child}}
#'
#' @examples
#' # hierarchy <- list("Europe" = c("Germany", "France"), "Germany" = c("Berlin", "Munich"))
#' # attribute <- factor(c("Berlin", "Munich", "Paris"))
#' # generalize_childrens(c("Berlin", "Munich"), "Germany", attribute, hierarchy)
#' 
#' 

generalize_childrens <- function(childrens, Parent, attribute, hierarchy){
#browser()
  for(traverse_node in childrens){
    sub_childs <- find_child(traverse_node, hierarchy)
      if(is.null(sub_childs)){
        attribute <- generalize_child(attribute, node=traverse_node, Parent)
      }else{
        attribute <- generalize_childrens(sub_childs, Parent, attribute, hierarchy)
        attribute <- generalize_child(attribute, node=traverse_node, Parent)
      }
    }
    
  return(attribute)
}
