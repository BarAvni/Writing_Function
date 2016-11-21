\name{canopyInterception}
\alias{canopyInterception}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
canopyInterception(Tair, LAI, P, Smean = 6.6, L0 = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tair}{
%%     ~~Describe \code{Tair} here~~
}
  \item{LAI}{
%%     ~~Describe \code{LAI} here~~
}
  \item{P}{
%%     ~~Describe \code{P} here~~
}
  \item{Smean}{
%%     ~~Describe \code{Smean} here~~
}
  \item{L0}{
%%     ~~Describe \code{L0} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Tair, LAI, P, Smean = 6.6, L0 = 0) 
{
    ps = 67.92 + 51.25 * exp(Tair/2.59)
    S = Smean * (0.27 + (46/ps))
    Lstar = S * LAI
    k = 1/Lstar
    (Lstar - L0) * (1 - exp(-k * P))
    I = Il * c
    I
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
