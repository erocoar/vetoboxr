% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vote-helpers.R
\docType{methods}
\name{vote_helpers}
\alias{vote_helpers}
\alias{create_voter_array}
\alias{create_voter_array,Voters-method}
\alias{create_role_array}
\alias{create_role_array,Voters-method}
\title{Voters Array Generators}
\usage{
create_voter_array(voters, drift, vibration, iter, ...)

\S4method{create_voter_array}{Voters}(voters, drift, vibration, iter, ...)

create_role_array(voters, iter, ...)

\S4method{create_role_array}{Voters}(voters, iter, no_random_veto,
  no_random_normal, ...)
}
\arguments{
\item{voters}{A \code{Voters} object.}

\item{drift}{A vector or matrix of drift values. If vector, then the drift is constant per iteration and the vector must be of length \code{dimension * voter_count}. If matrix, the drift can vary per dimension and must be of shape \code{iterations} x \code{dimension * voter_count}.}

\item{vibration}{A function with first parameter \code{n} that generates random noise.}

\item{iter}{The number of iterations.}

\item{...}{Additional keyword arguments.}

\item{no_random_veto}{Indicator for whether voters with random position can take on the Veto role.}

\item{no_random_normal}{Indicator for whether random voters can take on the Normal role.}
}
\description{
Vote helpers are functions that take as input an object of class \code{Voters} and additional information regarding drift, vibration and the number of iterations to vote on to
- reshape the position matrix and role vector to the correct size, resembling the number of iterations
- add the specified drift to the voter positions and (calculate and) add the vibration
- for voters with random positions, determine the actual positions, per run
}
