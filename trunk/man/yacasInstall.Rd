\name{yacasInstall}
\alias{yacasInstall}
\alias{yacasFile}
\title{ Install yacas files needed by Ryacas }
\description{
  Download the \code{script.dat} and \code{yacas.exe} files needed
  by \code{Ryacas}.
}
\usage{
yacasInstall(url = "http://ryacas.googlecode.com/files/yacas-1.0.63.zip",
   overwrite = FALSE)
yacasFile(filename = c("yacas.exe", "scripts.dat", "R.ys"),
   slash = c("\\", "/"))
}
\arguments{
\item{url}{URL of a zip file containing 
    \code{yacas.exe} and \code{scripts.dat}.}
\item{overwrite}{If \code{TRUE} then existing files are overwritten.}
\item{filename}{Name of file whose full path is wanted.}
\item{slash}{slash style to use on output pathname.}
\item{...}{Additional arguments passed to \code{download.file}.  In most cases
this is not used.}
}
\details{
\code{yacasInstall} downloads a zip file and extracts \code{yacas.exe} and 
\code{scripts.dat} installing them into 
\code{yacasFile("yacas.exe")} and \code{yacasFile("scripts.dat")}.

}
\value{
\code{yacasInstall} has no return value.

\code{yacasFile} returns the full pathname of the indicated yacas file
or the location where it would be if it were installed.  For \code{yacas.exe}
this is the contents of the environment variable \code{YACAS_HOME} or if
that is not defined then it is the folder
\code{system.file(package = "Ryacas", "yacdir")}.
For \code{scripts.dat} this is the contents of the environment variable
\code{YACAS_SCRIPTS}, or if that is not defined, it is the folder where
\code{yacas.exe} is located or would be located.
For \code{R.ys} this is the contents of the environment variable
\code{YACAS_INIT}, or if that is not defined, it is located in
\code{system.file(package = "Ryacas", "yacdir")}.  Note that \code{R.ys}
is included with \code{Ryacas} since it is a text file 
but \code{scripts.dat} is not included since it is a binary file 
which is why their default locations
differ.

\code{yacasCheck} returns \code{0} if the yacas files, \code{yacas.exe}
and \code{scripts.dat} were found
in \code{yacasFile("yacas.exe")} and \code{yacasFile("scripts.dat")}, 
\code{-1} if they were not found and
\code{1} if they were found but have the wrong file size.  If the
user specifies the \code{YACAS_INVOKE_STRING} environment variable then
it will not be able to perform the check in which case \code{NA}
is returned.
}
\note{ 
These functions are for Windows systems only.  For other platforms these
environment variables are not available and the
user must install yacas manually prior to installing \code{Ryacas}.
}
\examples{
\dontrun{
Sys.getenv("YACAS_INVOKE_STRING")
Sys.getenv("YACAS_HOME")
Sys.getenv("YACAS_SCRIPTS")
system.file(package = "Ryacas", "yacdir")
yacasFile("yacas.exe")
yacasFile("scripts.dat")
yacasInstall()
}
}
\keyword{ symbolmath }
