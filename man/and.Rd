% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/and.R
\name{and}
\alias{and}
\alias{\%and\%}
\title{Get Intersection Set for Sets}
\usage{
and(...)

a \%and\% b
}
\arguments{
\item{...}{one or more objects}

\item{a}{one object}

\item{b}{the other object}
}
\value{
intersection elements
}
\description{
Get intersection set for sets.
}
\examples{
A <- c("a","b","c")
B <- c("a","b","c","d")
C <- c("a","e","h")
and(A, B)
and(A, B, C)
A <- c("a","b","c")
B <- c("a","b","c","d")
C <- c("a","e","h")
A \%and\% B
A \%and\% B \%and\% C
}
