#' Plot Hierarchical Structure
#'
#' Visualize the hierachical structure used to generalize quasi-identifiers.
#'
#' @param hierarchy_list A list containing hierarchical relationships.
#' @param var The variable name (i.e. single quasi-identifier) within the hierarchy list to plot.
#' @param main Title of the plot. If not given is generated. (optional)
#' @param vertex_size Size of the vertices. Default is 10.  (optional)
#' @param edge_arrow_size Size of the edge arrows. Default is 0.5. (optional)
#' @return An igraph object representing the hierarchy.
#' @importFrom igraph make_empty_graph add_vertices add_edges plot.igraph V E
#' @export
#' @examples
#' plot_hierarchy(taxonomy_adult, "age")
#' 
plot_hierarchy <- function(hierarchy_list, var, 
              vertex_size = 10, edge_arrow_size = 0.5, 
              main = NULL, ...) {
  # Validate inputs
  if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Package 'igraph' is required but not installed. Please install it.")
  }
  
  if (!is.list(hierarchy_list)) {
  stop("hierarchy_list must be a list")
  }
  
  if (!var %in% names(hierarchy_list)) {
  stop(paste("Variable", var, "not found in hierarchy list"))
  }
  
  if (length(hierarchy_list[[var]]) == 0) {
  warning("Empty hierarchy structure for variable", var)
  return(igraph::make_empty_graph(directed = TRUE))
  }
  
  # Helper function to get or create vertex ID
  lookup <- list()
  get_id <- function(name) {
  if (!name %in% names(lookup)) {
    lookup[[name]] <<- length(lookup) + 1
    g <<- igraph::add_vertices(g, 1, name = name)
  }
  return(lookup[[name]])
  }
  
  # Create graph
  g <- igraph::make_empty_graph(directed = TRUE)
  
  # Add edges representing hierarchical relationships
  for (entry in names(hierarchy_list[[var]])) {
  if (nchar(entry) > 0) {
    parent <- hierarchy_list[[var]][entry][[1]]
    if (!is.null(parent) && nchar(parent) > 0) {
    start_vertex <- get_id(entry)
    end_vertex <- get_id(parent)
    g <- igraph::add_edges(g, edges = c(start_vertex, end_vertex))
    }
  }
  }
  
  # Plot the graph
  if (!is.null(g) && igraph::vcount(g) > 0) {
  plot_title <- if(is.null(main)) paste("Hierarchy for", var) else main
  igraph::plot.igraph(g, 
            vertex.size = vertex_size,
            edge.arrow.size = edge_arrow_size,
            main = plot_title,
            ...)
  } else {
  warning("No valid hierarchy structure to plot")
  }
  
  # Return the graph object for further customization
  invisible(g)
}



