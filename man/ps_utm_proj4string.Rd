% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.R
\name{ps_utm_proj4string}
\alias{ps_utm_proj4string}
\title{Get UTM proj4string}
\usage{
ps_utm_proj4string(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84")
}
\arguments{
\item{x}{A sf object}

\item{sfc_name}{A character string indicating name of sfc column with valid crs.}

\item{datum}{A character string indicating desired datum of UTM proj4string.}
}
\value{
A numeric vector of UTM zone(s).
}
\description{
Get UTM proj4string
}
