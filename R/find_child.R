#

find_child <- function(node,hierarchy){
  if(!is.null(node)){
    matched_childs <- sapply(hierarchy, function(y) node %in% y)
    if(any(matched_childs)){
      return(names(hierarchy[matched_childs]))
    }else{
      return(NULL)
    }
  }
}


