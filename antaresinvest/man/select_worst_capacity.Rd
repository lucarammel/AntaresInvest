% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_2_investment_model.R
\name{select_worst_capacity}
\alias{select_worst_capacity}
\title{Compute the worst capacity based on revenues of the year and the expected net profit}
\usage{
select_worst_capacity(
  revenues_total_net,
  year_actual,
  policy_retired,
  candidates_retirement,
  LOLD,
  area
)
}
\arguments{
\item{revenues_total_net}{(dataframe) of net revenues for each clusters by year}

\item{year_actual}{(int) year actual}

\item{policy_retired}{(str vector) candidates to retire already set in antares studies}

\item{candidates_retirement}{(str vector) clusters not allowed for decomissionning. Find in parameters.}

\item{LOLD}{(scalar) Loss of load duration}

\item{area}{(str) country code}

\item{years_to_simulate}{(int vector)years_to_simulate}
}
\value{
worst cluster's name or string 'end'
}
\description{
Compute the worst capacity based on revenues of the year and the expected net profit
}
