\name{Thornthwaite}
\alias{Thornthwaite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Thornthwaite(day, year, Tair, TMDH)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{day}{
%%     ~~Describe \code{day} here~~
}
  \item{year}{
%%     ~~Describe \code{year} here~~
}
  \item{Tair}{
%%     ~~Describe \code{Tair} here~~
}
  \item{TMDH}{
%%     ~~Describe \code{TMDH} here~~
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
function (day, year, Tair, TMDH) 
{
    day_diff = diff(day)
    for (i in 1:length(day_diff)) {
        if (day_diff[i] < 1) {
            day_diff[i] = 0
        }
    }
    day_diff0 = which(day_diff \%in\% 0)
    for (j in 1:length(day_diff0)) {
        if (j == 1) {
            Tmonth = mean(Tair[1:day_diff0[j]])
        }
        else {
            Tmonth[j] = mean(Tair[(day_diff0[j - 1] + 1):day_diff0[j]])
        }
    }
    MH_index = (Tmonth/5)^(1.514)
    year_diff = diff(year)
    year_diff1 = which(year_diff \%in\% 1)
    year_intersect = intersect(year_diff1, day_diff0)
    year_index = which(day_diff0 \%in\% year_intersect)
    AH_factor = (MH_index/5)^(1.514)
    for (n in 1:length(year_index)) {
        if (n == 1) {
            AH_index = sum(AH_factor[1:year_index[n]])
        }
        else {
            AH_index[n] = sum(AH_factor[(year_index[n - 1] + 
                1):year_index[n]])
        }
    }
    Ep = 0
    for (m in 1:length(AH_index)) {
        a = 0.49 + 1.79 * 10^(-2) * AH_index[m] + 7.711 * 10^(-5) * 
            AH_index[m]^2 + 6.751 * 10^(-7) * AH_index[m]^3
        for (l in 1:length(MH_index)) {
            b = TMDH[l]/360
            Ep[l] = 1.6 * b * ((10 * Tmonth[l])/(AH_index[m]))^a
        }
    }
    Ep
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
