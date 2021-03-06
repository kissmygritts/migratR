% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_mcp.R
\name{gen_mcp}
\alias{gen_mcp}
\title{Generates a Minimum Convex Polygon}
\usage{
gen_mcp(x, y, pct = 1)
}
\arguments{
\item{x}{A vector of the UTM Easting coordinates of a point set}

\item{y}{A vector of the UTM Northing coordinates of a point set}

\item{pct}{A number between 0 and 1 that determines the percent of points
used to generate the MCP}
}
\value{
A list that contains the MCP border, the centroid of the MCP,
the area of the MCP, and the perimeter of the MCP
}
\description{
\code{gen_mcp} generates the minimum convex polygon given x and y vectors
of a point set. The percentage of points to include can be controlled with
the \code{pct} parameter.
}
