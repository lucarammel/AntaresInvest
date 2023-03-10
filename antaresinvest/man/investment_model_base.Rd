% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3_2_investment_model.R
\name{investment_model_base}
\alias{investment_model_base}
\title{Investment basic : take the best/the worst capacity and invest/decommision}
\usage{
investment_model_base(
  year_actual,
  power_plants,
  investment_metrics,
  area,
  decisions,
  LOLD = NA,
  s
)
}
\arguments{
\item{year_actual}{(int) the actual year of the model}

\item{power_plants}{(dataframe) with costs hypothesis}

\item{investment_metrics}{(dataframe) with the investment metrics (ie revenues and metrics such as NPV or IRR here)}

\item{area}{(str) country code}

\item{decisions}{(list) investments/ decommissionning decisions}

\item{LOLD}{(scalar) Loss of load duration}

\item{s}{(int) loop step}
}
\value{
power_plants_moves : the record of all movements of capacities.
}
\description{
Investment basic : take the best/the worst capacity and invest/decommision
}
