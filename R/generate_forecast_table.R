#' generate_forecast_table function
#'
#' Executes a list of forecast models for a predefined set of horizons
#'
#'
#' @param data A dataframe containing the sales data. Here the columns are
#' individual time series, the different rows are different periods of the
#' time series.
#' @param models A vector with string notations of different models. A
#' selection can be made from c("naive", "seasonal naive", "holt-winters","ets","auto.arima")
#' or their abbreviation c("n","sn","hw","e","a"). Default is c("ets").
#' @param start A vector containing Year, Month and Day info in the format of
#' c(YYYY, MM, DD).
#' @param freq Frequency of the timeseries, numerical value.
#' @param h The forecast horizon, how many periods ahead are the models
#' forecasting.
#' @param orimax Maximum number of rolling origins. Default value is 1. This
#' parameter can be set to a different number if the goal is to simulate a
#' forecasting setup over a rolling origin.
#' @param train_length Number of train periods. When the default value NA is kept,
#' this parameter is set to the length of the data timeseries - (h + orimax - 1).
#' @param export TRUE if the table with forecasts should be exported to a csv
#' file. Default is FALSE
#'
#' @author Yves R. Sagaert
#'
#'
#' @return A data frame with the (yft) format.
#'
#' @export
#'
#' @examples
#' # Daily example
#' data <- round(data.frame(AA=rnorm(100,100,10),AB=rnorm(100,500,10),BB=rnorm(100,1000,10)))
#' generate_forecast_table(data, models=c("ets"), start=c(2019,7,1),
#' freq=365.25, h=7, orimax=1, train_length=NA, export=FALSE)
#' generate_forecast_table(data, models=c("ets"), start=c(2019,7,1),
#' freq=365.25, h=3, orimax=2)
#'
#' # Monthly example
#' data <- round(data.frame(MA=rnorm(30,3000,300),MB=rnorm(30,7000,400),MC=rnorm(30,200,50)))
#' generate_forecast_table(data, models=c("n","sn"), start=c(2019,7),
#' freq=12, h=3, orimax=1)
#'
#'

# Info on the (yft) format:

# period; numeric representation of date
# nrsal; nr sales: numeric representation of time series
# nrmod; numeric representation of model
# nrdoe; numeric representation of design of experiment (doe)
# nrsetup; numeric representation of experimental setup
# nrhor; numeric representation of forecast horizon
# nrrolor; numeric representation of rolling origin in multi step forecast experiment
# PointForecast; numeric representation of forecast with bounds
# Actuals; numeric representation of sales when available
# GMRAE_benchmark; numeric representation of the one step ahead naive forecast in sample for calclulating the denominator of the geometric mean relative absolute error (GMRAE) metric
# meanhistdemand; average historical demand up until this moment
# IC's of the model when available, otherwise NA
# MSE: one-step ahead MSE of the model in-sample
# default values see below; data unavailable is NA

# nrmod convensions:
# 10001 = naive
# 10002 = seasonal naive
# 10003 = holt-winters
# 10004 = ETS
# 10005 = ARIMA
# 10006 = STL
# 10007 = TBATS
# 10008 = NNETAR
# 10009 = CROSTON
# 10010 = SBA
# rest: following numbers, higher number is higher complexity
# 1 in front is a one-stage model
# 2 is two-stage model, e.g. hierarchical reconciliation
# 3 is three-stage model, e.g. hierarchical reconciliation via distribution
# 4 is 4-stage model, e.g. inventory in cost function of 3
# 10 in front is no external data
# 11 is use of external data; calender effects
# 12 is promotional data
# 13 is calendar + promotions
# 14 is
# 15 is leading indicators (with or without other promo, calendar)

generate_forecast_table <- function(data, models=c("ets"),
                                    start, freq=365.25, h=1, orimax=1,
                                    train_length=NA, export=FALSE){
  # Return object: out-of-sample (oos) forecasting (fc) results
  colnames_oos_fc_results <- c("period","nrsal","nrmod","nrdoe","nrsetup",
                               "nrhor","nrrolor","pointforecast","lo80",
                               "hi80","lo95","hi95","actuals",
                               "naiveforecast", "meanhistdemand",
                               "AIC","BIC","AICC","MSE")
  oos_fc_result <-  matrix(ncol=length(colnames_oos_fc_results),
                           nrow=length(models)*ncol(data)*orimax*h)
  colnames(oos_fc_result) <- colnames_oos_fc_results

  # Make the forecast
  # start from 2 because not the date column
  if (is.na(train_length)){
    train_length <- nrow(data)-(h+orimax-1)
  }
  irow1 <- 1 # matrix of results
  # ori = 1;tsi=1;imod=1;ih=1 # debug
  for (ori in 1:orimax){
    for (tsi in 1:ncol(data)){
      # training data
      train <- stats::ts(data[1:(train_length+ori-1),tsi],start=start,frequency = freq)
      ytrue <- stats::ts(data[(train_length+ori-1)+c(1:h),tsi],start=stats::end(train)+1/round(freq),frequency = freq)

      for (imod in 1:length(models)){
        model <- tolower(models[imod])
        if (model=="naive" | model=="n"){
          # Naive forecasting model
          om <- forecast::naive(train,h)
          fc <- om
          om$mse=mean(om$residuals^2,na.rm=TRUE)
          mod_nr <- 10001
          # Missing info
          om$aic=NA
          om$bic=NA
          om$aicc=NA
        } else if (model=="seasonal naive" | model=="sn"){
          # Seasonal Naive forecasting model
          om <- forecast::snaive(train,h)
          fc <- om
          om$mse=mean(om$residuals^2,na.rm=TRUE)
          mod_nr <- 10002
          # Missing info
          om$aic=NA
          om$bic=NA
          om$aicc=NA
        } else if (model=="holt-winters" | model=="hw"){
          # Holt - Winters forecasting model
          om <- forecast::hw(train,h)
          fc <- om
          om$mse=mean(om$residuals^2,na.rm=TRUE)
          mod_nr <- 10003
          # Missing info
          om$aic=NA
          om$bic=NA
          om$aicc=NA
        } else if (model=="ets" | model=="e"){
          # Exponential Smoothing State-Space forecasting model
          om <- forecast::ets(train)
          # Forecast
          fc <- forecast::forecast(om,h=h)
          mod_nr <- 10004
        } else if (model=="auto.arima" | model=="a"){
          # Auto Arima forecasting model
          om <- forecast::auto.arima(train)
          fc <- forecast::forecast(om,h=h)
          om$mse=mean(om$residuals^2,na.rm=TRUE)
          mod_nr <- 10005
        } else {
          warning("Model not found. This model has not been implemented.")
        }
        # plot(forecast(ets(train),h=7)); lines(ytrue,col="red") # debugging if you want

        gmrae_benchmark <- forecast::naive(train,h=h)$mean
        # benchmark model for GMRAE in denominator
        mean_hist_demand <- mean(train, na.rm=TRUE)
        # mean historical demand for MASE + RMSSE

        # fill in the data
        for (ih in 1:h){
          # ETS or other forecast model case
          oos_fc_result[irow1,] <- c((train_length+ori-1+ih), tsi, mod_nr, 1, 1, ih, ori,
                                     fc$mean[ih], fc$lower[ih,1], fc$upper[ih,1],
                                     fc$lower[ih,2], fc$upper[ih,2], ytrue[ih],
                                     gmrae_benchmark[ih], mean_hist_demand,
                                     om$aic, om$bic, om$aicc, om$mse)
          irow1 <- irow1 + 1
        } # end of ih
      } # end of imod
    } # end of tsi
  } # end of ori
  # Save the results
  if (export==TRUE){
    # save with ; or , in a csv file:
    utils::write.csv(x=oos_fc_result, file="forecast_file_models_ets0.csv")
    utils::write.csv2(x=oos_fc_result, file="forecast_file_models_ets.csv")
  }
  # Return the yft table
  return(oos_fc_result)

}
