% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basemap.R
\name{ps_pad_bbox}
\alias{ps_pad_bbox}
\title{Pad bbox}
\usage{
ps_pad_bbox(x, pad)
}
\arguments{
\item{x}{A bbox object (or numeric vector of length 4 indicating xmin, ymin, xmax, ymax). Object should not be long/lat.}

\item{pad}{A numeric vector indicating amount in metres to pad bbox.
If vector of length 1, amount is added to all sides;
if vector of length 4 amount is added to left, top, right and bottom, respectively.}
}
\value{
modified object.
}
\description{
Pad bbox
}
