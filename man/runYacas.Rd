\name{runYacas}
\alias{runYacas}
\title{ Run a yacas session directly. }
\description{
 This command is typically used without arguments.  It simply runs
 an ordinary interactive yacas session.  Note that this command is
 not used to spawn the yacas server but just to interact directly
 with yacas independently of R.
}
\usage{
runYacas(method = "system", yacas.args = "", yacas.init = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{method}{ Can be "system" or "server".  Normally not used. }
  \item{yacas.args}{ Can be used to specify the yacas 
  command line arguments.  Normally this is not used. }
  \item{yacas.init}{ Can be used to specify the yacas command line
  \code{--init} argument. Normally this is not used. }
}
\value{
  No return value.
}
\examples{
\dontrun{
yacasRun()
}
}
\keyword{ symbolmath }
