% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_forecast_table.R
\name{generate_forecast_table}
\alias{generate_forecast_table}
\title{generate_forecast_table function}
\usage{
generate_forecast_table(
  data,
  models = c("ets"),
  start,
  freq = 365.25,
  h = 1,
  orimax = 1,
  train_length = NA,
  opt = rep(0, length(models)),
  export = FALSE
)
}
\arguments{
\item{data}{A dataframe containing the sales data. Here the columns are
individual time series, the different rows are different periods of the
time series.}

\item{models}{A vector with string notations of different models. A
selection can be made from c("naive", "seasonal naive", "holt-winters",
"ets", "auto.arima", "stlf", "tbats", "nnetar", "intermittent")
or their abbreviation c("n","sn","hw","e","a", "st", "t", "nn", "i").
Default is c("ets").}

\item{start}{A vector containing Year, Month and Day info in the format of
c(YYYY, MM, DD).}

\item{freq}{Frequency of the timeseries, numerical value.}

\item{h}{The forecast horizon, how many periods ahead are the models
forecasting.}

\item{orimax}{Maximum number of rolling origins. Default value is 1. This
parameter can be set to a different number if the goal is to simulate a
forecasting setup over a rolling origin.}

\item{train_length}{Number of train periods. When the default value NA is kept,
this parameter is set to the length of the data timeseries - (h + orimax - 1).}

\item{opt}{Optimise model: A numeric vector with options for each model that is called
in the argument models. This is 0 by default for all models, but can be set
to model-specific options.}

\item{export}{TRUE if the table with forecasts should be exported to a csv
file. Default is FALSE}
}
\value{
A data frame with the (yft) format.
}
\description{
Executes a list of forecast models for a predefined set of horizons
}
\examples{
# Daily example
data <- round(data.frame(AA=rnorm(100,100,10),AB=rnorm(100,500,10),
BB=rnorm(100,1000,10)))
generate_forecast_table(data, models=c("ets"), start=c(2019,7,1),
freq=365.25, h=7, orimax=1, train_length=NA, export=FALSE)
generate_forecast_table(data, models=c("ets"), start=c(2019,7,1),
freq=365.25, h=3, orimax=2)

# Monthly example
data <- round(data.frame(MA=rnorm(30,3000,300),MB=rnorm(30,7000,400),
MC=rnorm(30,200,50)))
generate_forecast_table(data, models=c("n","sn"), start=c(2019,7),
freq=12, h=3)

# Intermittent demand
data <- matrix(abs(round(rnorm(240,0,0.5))),nrow=60,ncol=4,
dimnames=list(NULL,c("IA","IB","IC","ID")))
generate_forecast_table(data, models=c("i","i","i"), start=c(2019,7),
freq=12, h=3, opt=c(0,1,2))
}
\author{
Yves R. Sagaert
}
