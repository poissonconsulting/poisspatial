% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proj.R
\name{ps_utm_note}
\alias{ps_utm_note}
\title{Get UTM zone description.}
\usage{
ps_utm_note(x, sfc_name = ps_active_sfc_name(x), datum = "WGS84")
}
\arguments{
\item{x}{A sf object}

\item{sfc_name}{A character string indicating name of sfc column with valid crs.}

\item{datum}{A character string indicating desired datum of UTM proj4string.}
}
\value{
A character vector of UTM zone description(s).
}
\description{
Get UTM zone description.
}
