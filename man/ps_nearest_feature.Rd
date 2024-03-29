% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nearest.R
\name{ps_nearest_feature}
\alias{ps_nearest_feature}
\title{Nearest Feature}
\usage{
ps_nearest_feature(x, y, dist_col = NULL, ...)
}
\arguments{
\item{x}{An sf object.}

\item{y}{An sf object.}

\item{dist_col}{A string indicating the name of the column to save the distance in.}

\item{...}{Not used}
}
\description{
Each row in object x is bound with the closest feature in object y.
Uses sf::st_nearest_feature.
}
\details{
sf objects have their sf (active geometry) column renamed to geometry.
}
