#' Dijkstra algorithm
#'
#'This an R implementation of Dijkstra algorithm which finds the shortest path from `init_node` to all the other nodes
#'
#' @param graph A `data.frame` representing the graph. It has to have 3 columns: `v1` and `v2` which represent
#'  an edge from the node in `v1` to the node in `v2`, and `w` which is the weight of that edge.
#' @param init_node A numeric scalar. The algorithm will use `init_node` as the source and will find the
#'  shortest path to all other nodes.
#'
#' @return A numeric vector. The vector contains the shortest path to every other node from the starting node or `init_node`.
#' @export
#'
#' @examples
#' data(wiki_graph)
#'
#' dijkstra(wiki_graph, 1) # 0 7 9 20 20 11
#' dijkstra(wiki_graph, 3) # 9 10 0 11 11 2

dijkstra <- function(graph, init_node){

  stopifnot(
    length(init_node) == 1,
    names(graph) %in% c("v1", "v2", "w")
  )

  node_unvisited <- c(unique(graph$v1))
  # initializing the distances to infinity since it is unknown
  distance <- rep(Inf, length(node_unvisited))
  # the distance of the init_node to itself is zero
  distance[init_node] <- 0
  # setting names for the vector (to support nodes with alphabetic names)
  names(distance) <- node_unvisited
  node_names <- names(distance)

  while(length(node_unvisited)){

    # selecting a node from unvisited nodes with lowest distance
    node_current_name <- node_names[distance %in% min(distance[node_unvisited])]
    # tie breaker (for cases that distance of the two nodes are equal to each other)
    if(length(node_current_name) > 1){
      node_current_name <- sample(node_current_name, 1)
    }


    # finding the neighbors of the current node which are not visited
    neighbors <- graph[graph$v1 == node_current_name, "v2"]
    neighbors <- neighbors[neighbors %in% node_unvisited]

    for(node in neighbors){
      # updating the distances

      weight <- graph[graph$v1 == node_current_name & graph$v2 == node , "w"]

      distance_new <- distance[node_current_name] + weight

      distance[node] <-  ifelse(
        distance[node] > distance_new,
        distance_new,
        distance[node]
      )

    }
    # remove the current node from unvisited set
    node_unvisited <- node_unvisited[-which(node_unvisited == node_current_name)]

  }

  return(distance)
}


#-----TEST---
wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))

wiki_graph <-
  data.frame(v1=c("a","a","a","b","b","b","c","c","c","c","d","d","d","e","e","f","f","f"),
             v2= c("b","c","f","a","c","d","a","b","d","f","b","c","e","d","f","a","c","e"),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph, 1)
dijkstra(wiki_graph, 3)
