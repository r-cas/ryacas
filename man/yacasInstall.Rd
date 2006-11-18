\name{yacasInstall}
\alias{yacasInstall}
\title{ Install yacas files needed by Ryacas }
\description{
  Download the \code{script.dat} and \code{yacas.exe} files needed
  by \code{Ryacas}.
}
\usage{
yacasInstall(showonly = FALSE, ..., filesize = 376832)
}
\arguments{
\item{showonly}{If \code{TRUE} then no download is performed but instead
command simplfy shows what it would download and to where.}
\item{filesize}{The file size of \code{yacas.exe} is compared to this value
and if not the same then a warning is issued.  If this argument is specified
as \code{NA} then no comparison is done.}
\item{...}{Additional arguments passed to \code{download.file}.  In most cases
this is not used.}
}
\details{
Downloads the \code{yacas} files needed by \code{Ryacas}.
}
\value{
  No return value.
}
\note{ 
This function is intended for Windows systems only.  For other platforms the
user must install yacas manually prior to installing \code{Ryacas}.
}
\examples{
yacasInstall(showonly = TRUE)
}
\keyword{ symbolmath }
