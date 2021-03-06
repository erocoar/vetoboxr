% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate.R
\name{animate}
\alias{animate}
\alias{animate.Vote}
\title{Animate selected iterations of a \code{Vote} object.}
\usage{
animate(vote, movie.name, iter = NULL, ...)

\method{animate}{Vote}(vote, movie.name, iter = NULL, ...)
}
\arguments{
\item{vote}{An object of class \code{Vote}.}

\item{movie.name}{Name of the resulting GIF file.}

\item{iter}{A vector of the iterations to plot, defaults to \code{seq(1, vote$iter)}.}

\item{...}{Additional keyword arguments.}
}
\description{
\code{animate.Vote()} plots selected iterations of a \code{Vote} object and combines them into one GIF using the \code{animation} package.
}
