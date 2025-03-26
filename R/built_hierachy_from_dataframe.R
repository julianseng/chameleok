#' Build Hierarchy from Taxonomy Dataframe for K-Anonymity
#'
#' This function transforms a taxonomy represented as a dataframe or matrix into a hierarchical structure
#' used for generalizing quasi-identifiers in k-anonymity implementations. Each row in the input represents 
#' a path in the generalization hierarchy, with more specific values appearing in earlier columns and 
#' their generalizations in subsequent columns.
#'
#' @param taxonomy A dataframe or matrix where each row represents a taxonomic path.
#'                 Non-NA and non-empty values are used to establish parent-child relationships.
#'                 The algorithm searches for consecutive non-empty entries in each row to determine these relationships.
#'
#' @return A list structure where each element represents a parent-child relationship in the generalization hierarchy.
#'         Each element is named after the child concept and contains a list with a 'parent' element
#'         identifying its parent concept (more general value).
#'
#' @keywords internal
#' @export



built_hierachy_from_dataframe <- function(taxonomy){
  if(is.data.frame(taxonomy) || is.matrix(taxonomy)){
    class_hierachy <- list()
    for(i in 1:nrow(taxonomy)){
      s <- 1
      for(j in 1:(ncol(taxonomy)-1)){
        if( !is.na(taxonomy[i,j]) | taxonomy[i,j]!=""){ # start must not missing
          searcher <- TRUE
          while(searcher){
            if(s+j>ncol(taxonomy)){
              searcher <- FALSE
              break
              }
            else if(is.na(taxonomy[i,j+s]) | taxonomy[i,j+s]==""){
              s <- s + 1
            }
            else{
              class_hierachy[taxonomy[i,j]] <- list(parent=taxonomy[i,j+s])      
              searcher <- FALSE
              s <- 1
            } 
          }
        }
      }
    }
    return(class_hierachy)
  }else{
    warning("Input is not matrix or dataframe")
  }
}
