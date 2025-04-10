% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_datafly_with_test_data.R
\name{core_datafly}
\alias{core_datafly}
\title{Datafly algorithm for k-anonymity}
\usage{
core_datafly(data, test_data, quid, k = 5, gen_try = 20, taxonomy)
}
\arguments{
\item{data}{A data frame containing the quasi-identifiers and sensitive attributes.}

\item{test_data}{A data frame containing the quasi-identifiers and sensitive attributes for testing.}

\item{quid}{A list of column names representing the quasi-identifiers.}

\item{k}{The minimum number of records that share the same quasi-identifier values.}

\item{gen_try}{The maximum number of generalization attempts.}

\item{taxonomy}{A list representing the current hierarchy structure.}
}
\value{
A list containing the training and testing data frames with k-anonymity applied.
}
\description{
The datafly algorithm uses generalization and suppression to achieve k-anonymity.
}
