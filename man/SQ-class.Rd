% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vote-classes.R
\docType{class}
\name{SQ-class}
\alias{SQ-class}
\alias{.SQ}
\alias{SQ}
\title{S4 class representing the Status Quo.}
\usage{
SQ(position, drift = NULL)
}
\arguments{
\item{position}{A position vector.}
}
\description{
S4 class representing the Status Quo.
}
\section{Slots}{

\describe{
\item{\code{position}}{A position vector of length \code{dimension}.}

\item{\code{dimension}}{The dimension of the status quo.}

\item{\code{drift}}{A vector indicating the drift (constant change per iteration).}
}}

