% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.R
\name{ps_utm_zone}
\alias{ps_utm_zone}
\title{Get UTM zone}
\usage{
ps_utm_zone(x, sfc_name = ps_active_sfc_name(x))
}
\arguments{
\item{x}{A sf object}

\item{sfc_name}{A character string indicating name of sfc column.}
}
\value{
A numeric vector of UTM zone(s).
}
\description{
Get UTM zone
}
