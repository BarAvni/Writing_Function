\name{penman_montieth}
\alias{penman_montieth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
penman_montieth(Tair, vpd, Rnet, gs, ga, dayl, CP = 1010, Pair = 101325)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Tair}{
%%     ~~Describe \code{Tair} here~~
}
  \item{vpd}{
%%     ~~Describe \code{vpd} here~~
}
  \item{Rnet}{
%%     ~~Describe \code{Rnet} here~~
}
  \item{gs}{
%%     ~~Describe \code{gs} here~~
}
  \item{ga}{
%%     ~~Describe \code{ga} here~~
}
  \item{dayl}{
%%     ~~Describe \code{dayl} here~~
}
  \item{CP}{
%%     ~~Describe \code{CP} here~~
}
  \item{Pair}{
%%     ~~Describe \code{Pair} here~~
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
function (Tair, vpd, Rnet, gs, ga, dayl, CP = 1010, Pair = 101325) 
{
    Rnet = Rnet/(60 * 60 * dayl)
    rs = 1000/gs
    ra = 1000/ga
    tk = Tair + 273.15
    rho = 1.292 * (0.00428 * Tair)
    lhvap = 2502300 - 2430.54 * Tair
    dt = 0.2
    t1 = Tair + dt
    t2 = Tair - dt
    pvs1 = 610.7 * exp(17.38 * t1/(239 + t1))
    pvs2 = 610.7 * exp(17.38 * t2/(239 + t2))
    s = (pvs1 - pvs2)/(t1 - t2)
    gamma = CP * Pair/(lhvap)
    et = ((s * Rnet) + (rho * CP * vpd/ra))/(gamma * (1 + rs/ra) + 
        s)
    ewater = (et/(lhvap * 1000))
    ewater.day = ewater * dayl * 60 * 60 * 1000
    ewater.day
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
