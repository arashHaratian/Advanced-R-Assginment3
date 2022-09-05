#' A graph from Wikipedia webpage
#'
#' This graph has 6 nodes and 18 edges. The `v1` and `v2` represent edges
#' and the `w` is the weight of that particular edge.
#'
#' @format A data frame with 18 rows and 3 columns:
#' \describe{
#'   \item{v1}{names of the nodes, there is an edge between node in `v1` and `v2`, numeric value with no decimals}
#'   \item{v1}{names of the nodes, there is an edge between node in `v1` and `v2`, numeric value with no decimals}
#'   \item{w}{The weight of the edges, numeric value}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
"wiki_graph"
