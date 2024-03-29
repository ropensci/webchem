% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemspider.R
\name{cs_img}
\alias{cs_img}
\title{Download images from ChemSpider}
\usage{
cs_img(
  csid,
  dir,
  overwrite = TRUE,
  apikey = NULL,
  verbose = getOption("verbose")
)
}
\arguments{
\item{csid}{numeric; the ChemSpider ID (CSID) of the substance. This will
also be the name of the image file.}

\item{dir}{character; the download directory. \code{dir} accepts both
absolute and relative paths.}

\item{overwrite}{logical; should existing files in the directory with the
same name be overwritten?}

\item{apikey}{character; your API key. If NULL (default),
\code{cs_check_key()} will look for it in .Renviron or .Rprofile.}

\item{verbose}{logical; should a verbose output be printed on the console?}
}
\description{
Retrieve images of substances from ChemSpider and export them
in PNG format.
}
\note{
An API key is needed. Register at \url{https://developer.rsc.org/}
for an API key. Please respect the Terms & Conditions. The Terms & Conditions
can be found at \url{https://developer.rsc.org/terms}.
}
\examples{
\dontrun{
cs_img(c(582, 682), dir = tempdir())
}
}
\references{
\url{https://developer.rsc.org/docs/compounds-v1-trial/1/overview}
}
\seealso{
\code{\link{get_csid}}, \code{\link{cs_check_key}}
}
