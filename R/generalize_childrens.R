generalize_childrens <- function(childrens, Parent, attribute, hierarchy){
#browser()
  for(traverse_node in childrens){
    #if(any(levels(attribute)==Parent)){print(paste(Parent,"already generalized"))}
    sub_childs <- find_child(traverse_node, hierarchy)
      if(is.null(sub_childs)){
        attribute <- generalize_child(attribute, node=traverse_node, Parent)
        #hierarchy <- hierarchy[names(hierarchy)!=traverse_node]
      }else{
        #sub_childs <- sub_childs[!(sub_childs %in% levels(attribute))]
        attribute <- generalize_childrens(sub_childs, Parent, attribute, hierarchy)
        attribute <- generalize_child(attribute, node=traverse_node, Parent)
      }
    }
    
  return(attribute)
}
