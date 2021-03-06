% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_yield.R
\name{compute_yield}
\alias{compute_yield}
\title{compute annual yield'}
\usage{
compute_yield(T, P, irr, crop.pars)
}
\arguments{
\item{T}{annual temperature (C)}

\item{P}{annual precipitation (mm)}

\item{irr}{irrigation in (mm)}

\item{T}{optimal temperature (C)}

\item{max.water}{maximum water requirement (mm)}

\item{ts}{slope on temperature}

\item{tp}{slope on precipitation}

\item{base.yield}{baseline yield  (kg)}
}
\value{
yield in kg
}
\description{
Function to compute yeild of different fruits as a function of annual temperature and precipitation
}

