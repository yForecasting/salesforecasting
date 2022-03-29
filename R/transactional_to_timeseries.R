#' transactional_to_timeseries functions
#'
#' Converts a transactional dataframe into a dataframe with time series objects, or a unique time series
#' User can select the aggregation level, in terms of frequency, the transformed df can be
#' If the transcational dataframe has skipped values due to non zero demand, the function fills this values
#' with zeros
#'
#'
#'
#' For example:
#'  |    Date     | ID | Value |    |    Date     |  a1  |  a2  |
#'
#'  | 01/02/2014  | a1 |   20  |    | 01/02/2014  |  20  |  80  |
#'
#'  | 01/02/2014  | a2 |   80  |    | 02/02/2014  |  30  |   0  |
#'
#'  | 02/02/2014  | a1 |   30  | => | 03/02/2014  |   0  |  14  |
#'
#'  | 03/02/2014  | a2 |   14  |    |     ..      |  ..  |  ..  |
#'
#'  |     ..      | .. |   ..  |    |     ..      |  ..  |  ..  |
#'
#'  |     ..      | .. |   ..  |    | 28/40/2014  |  12  |   0  |
#'  | 28/40/2014  | a1 |   12  |

#'
#' @param df A transactional dataframe or matrix
#' @param index The index (or the column with the timestamps) of the dataframe. It should be in a date format.
#' @param orig_freq The original frequency of the dataframe.
#' @param aggregated_freq The frequency of the transformed dataframe.
#' NOTE 1: Default is set to null. With this setting it returns data at the original frequency
#' NOTE 2: Accepts values in the form: 'month', 'week', '2 months' etc
#' @param return_all A boolean variable indicating if all stored ids will be returned.
#' NOTE 1: Default is set to TRUE.
#' NOTE 2: If user passes in FALSE then variable columns_to_return will be considered
#' @param columns_to_return A list with the names of the columns that will be included.
#' NOTE 1: Default setting is NULL in accordance with return_all equal to TRUE.
#' NOTE 2: If return_all is FALSE then columns_to_return can not be NULL
#'
#' @author Filotas Theodosiou
#'
#'
#' @return A data frame with time series objects or a single time series
#' @export
#'
#' @examples
#' dates <- c('2019-07-01', '2019-07-02', '2019-07-15', '2019-07-21')
#' AM <- c(90, 100, 50, 5 )
#' AC <- c(100, 100, 25, 4)
#' example_df <- data.frame(dates, AM, AC)
#' transactional_to_timeseries(example_df, index = 'dates', orig_freq = 'day')
#' transactional_to_timeseries(example_df, index = 'dates', orig_freq = 'day',
#'                                         aggregated_freq = 'week', return_all = FALSE,
#'                                         columns_to_return = c('AM'))
#'
#'

transactional_to_timeseries <- function(df, index, orig_freq, aggregated_freq = NULL,
                                        return_all = TRUE, columns_to_return = NULL ){

  # Ensuring the index col is of class Date
  df[[index]] <- as.Date(df[[index]])

  # If no aggregated frequency is given, using the original
  if (is.null(aggregated_freq)){
    aggregated_freq <- orig_freq
  }

  # Renaming the index column
  names(df)[names(df) == index] <- 'Date'

  # filling missing transactional values
  ts_df <- tidyr::complete(df, "Date" = seq.Date(min(df$Date), max(df$Date), by = orig_freq))

  # Filling NaNs
  ts_df[is.na(ts_df)] <- 0
  # Keeping only the relevant columns
  if (return_all != TRUE){
    cols <- c('Date', columns_to_return )
    ts_df <- (ts_df[,cols])
  }


  # Aggregating
  agg_data <- cut(ts_df$Date, breaks = aggregated_freq)
  ts_df$Date <- agg_data

  # Grouping
  ts_df <- stats::aggregate(. ~ Date, ts_df, sum)


  # Convert to a ts object
  start <- ts_df[[1,'Date']]
  freq <- stats::frequency(ts_df[['Date']])

  for (i in 2:dim(ts_df)[2]){
    ts_df[,i] <- stats::ts(ts_df[,i], frequency = freq, start = start)
  }

  # Sorts based on the Dates
  ts_df <- ts_df[order(ts_df$Date),]

  return(ts_df)
}


