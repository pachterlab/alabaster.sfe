% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf.R
\name{readSF}
\alias{readSF}
\title{Read \code{sf} from \code{alabaster} on disk representation}
\usage{
readSF(path, metadata = NULL)
}
\arguments{
\item{path}{String containing a path to a directory, itself created with a \code{\link[alabaster.base]{saveObject}} method.}

\item{metadata}{Named list containing metadata for the object - most importantly, the \code{type} field that controls dispatch to the correct loading function.
If \code{NULL}, this is automatically read by \code{\link[alabaster.base]{readObjectFile}(path)}.}
}
\value{
An \code{sf} data frame
}
\description{
Read the GeoParquet file into R. GeoParquet should also work in any
programming language that supports \code{arrow}. Newer version of GDAL with
Parquet driver is not strictly necessary if the geometry's WKB can be 
converted to whichever language specific object such as \code{sfc} in R.
}
\examples{
library(sf)
fp <- tempfile()
df <- as.data.frame(matrix(rnorm(10), ncol = 2))
df <- st_as_sf(df, coords = names(df), crs = NA)
saveObject(df, path = fp)
df2 <- readObject(fp)

}
