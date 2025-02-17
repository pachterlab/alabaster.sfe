% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readImages.R
\name{readBioFormatsImage}
\alias{readBioFormatsImage}
\title{Read \code{BioFormatsImage} from \code{alabaster} on disk representation}
\usage{
readBioFormatsImage(path, metadata = NULL, ...)
}
\arguments{
\item{path}{String containing a path to a directory, itself created with a \code{\link[alabaster.base]{saveObject}} method.}

\item{metadata}{Named list containing metadata for the object - most importantly, the \code{type} field that controls dispatch to the correct loading function.
If \code{NULL}, this is automatically read by \code{\link[alabaster.base]{readObjectFile}(path)}.}

\item{...}{Ignored, but used for other methods.}
}
\value{
A \code{\link{BioFormatsImage}} object for SFE.
}
\description{
Reads the metadata and \code{imgSource} will point to the file within the
on disk representation of SFE. The image itself should not be moved or the
\code{BioFormatsImabe} object will no longer work.
}
\examples{
library(SFEData)
fp <- tempfile()
x1 <- XeniumOutput(dataset = "v1", file_path = file.path(fp, "xenium1"))
x2 <- XeniumOutput("v2", file_path = file.path(fp, "xenium2"))

# Single file OME-TIFF
fsave <- file.path(fp, "bfi1")
sfe <- readXenium(x1)
bfi <- getImg(sfe)
bfi <- affineImg(bfi, M = matrix(c(cos(pi/6), sin(pi/6), -sin(pi/6), cos(pi/6)), nrow = 2), 
                 v = c(0,0))
saveObject(bfi, fsave)
bfi2 <- readObject(fsave)

unlink(fsave, recursive = TRUE)

# Multi file OME-TIFF
fsave <- file.path(fp, "bfi2")
sfe <- readXenium(x2)
bfi <- getImg(sfe)
saveObject(bfi, fsave)
bfi2 <- readObject(fsave)
unlink(fsave, recursive = TRUE)
}
\seealso{
Other readObject-SFE-image: 
\code{\link{readExtImage}()},
\code{\link{readSpatRaster}()}
}
\concept{readObject-SFE-image}
