% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{wiki_graph}
\alias{wiki_graph}
\title{A graph from Wikipedia webpage}
\format{
A data frame with 18 rows and 3 columns:
\describe{
\item{v1}{names of the nodes, there is an edge between node in \code{v1} and \code{v2}, numeric value with no decimals}
\item{v2}{names of the nodes, there is an edge between node in \code{v1} and \code{v2}, numeric value with no decimals}
\item{w}{The weight of the edges, numeric value}
}
}
\source{
\url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
}
\usage{
wiki_graph
}
\description{
This graph has 6 nodes and 18 edges. The \code{v1} and \code{v2} represent edges
and the \code{w} is the weight of that particular edge.
}
\keyword{datasets}
