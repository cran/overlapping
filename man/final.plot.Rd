\name{final.plot}
\alias{final.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Final plot
}
\description{
Graphical representation of estimated densities and overlapping area.
}
\usage{
final.plot(DD, OV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DD}{Data frame obtained by function \code{\link{overlap}}}
  \item{OV}{Vector of overlapping areas obtained by \code{\link{overlap}}}
}
\details{
It requires the function \code{xyplot} of the package \code{lattice}.
}
%\value{
%Graph.
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Massimiliano Pastore
}
%\note{
%Da implementare eventuali aggiunte sui parametri grafici.
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{xyplot}}
}
\examples{
set.seed(20150605)
x <- list(X1=rnorm(100),X2=rt(50,8),X3=rchisq(80,2))
out <- overlap(x)
final.plot(out$DD,out$OV)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utility}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
