#' transform_to_transactional function
#'
#' Converts a dataframe to another in a transactional form.
#' Removes zero_demand observations for storage purposes.
#'
#' For example:
#'
#' | ID | d1 | d2 | d3 | ... | dt |    | ID |    Date     | Value |
#' | a1 | 20 | 30 | 0  | ... | 12 | => | a1 | 01/02/2014  |   20  |
#' | a2 | 80 | 0  | 14 | ... | 0  |    | a2 | 01/02/2014  |   80  |
#'                                     | a1 | 02/02/2014  |   30  |
#'                                     | a2 | 03/02/2014  |   14  |
#'                                     | .. |     ..      |   ..  |
#'                                     | .. |     ..      |   ..  |
#'                                     | a1 | 28/40/2014  |   12  |
#'
#'
#'
#' @param df A dataframe or matrix containing sales on a particular date. Columns stand for time observations
#' @param keep_columns A boolean stating if the function returns the original columns. Default is TRUE.
#' @param non_zero_demand A boolen stating if the function should returns zero observations. Default is FALSE.
#' @param start A date object indicating the starting date in the transactional df.
#' NOTE 1: Only considered if keep_columns is set to FALSE.
#' NOTE 2: If keep_columns is set to false, the start parameter should be given along with end or freq
#' @param end A date object indicating the last date on the transcational df
#' @param freq The frequency of the observated observations. Can be numeric, or string (eg 'day')
#'
#' @author Filotas Theodosiou
#'
#'
#' @return A data frame or matrix
#'
#'
#' @example
#' dates <- c('2019-07-01', '2019-07-02', '2019-07-03', '2019-07-01')
#' stores <- c('AM','AC')
#' example_df <- data.frame( t1 = c(15, 12),
#'                  t2 = c(30, 40 ),
#'                  t3 = c(0, 5),
#'                  t4 = c(21, 30)
#'                  )
#' rownames(example_df) <- stores
#' tr_df <- transform_to_transactional(example_df, keep_columns = TRUE) # Keeps current columns
#' @example
#' tr_df_dates <- transform_to_transactional(example_df, keep_columns = FALSE,
#'                                           start = as.Date(dates[1]), freq = 'day' )
#'


transform_to_transactional <- function(df, keep_columns = TRUE, non_zero_demand = FALSE,
                                       start = NULL, end = NULL, freq = NULL){


  # Getting the total number of observations
  total_obs <- dim(df)[2]

  # Assert we have either columns or a date-range
  if (keep_columns == FALSE){
    # Assert that 2 out of 3 are non null

    # If I have the start and the freq
    if (!is.null(start) & !is.null(end))
    {
      # Convert to dates
      start <- as.Date(start)
      end <- as.Date(end)
      # Generate sequence
      dates <- seq(from = start, to = end, length.out = total_obs)
      colnames(df) <- dates
    }
    else if (!is.null(start) & !is.null(freq))
    {
      start <- as.Date(start)
      dates <- seq(from = start, by = freq, length.out = total_obs)
      colnames(df) <- dates
    }
    else
    {
      stop("Use current columns or provide the start date and either the end or the frequency")
    }
  }

  # Melting
  melt_df <- reshape2::melt(as.matrix(df), value.name = "value",   varnames=c('ID', 'Date'))

  # Asserting we have the correct type
  melt_df$value <- as.numeric(melt_df$value)

  # Removing non zero demand if asked
  if (non_zero_demand == FALSE)
  {
    melt_df <-dplyr::filter(melt_df, melt_df$value > 0)
    # Re-ordering
    melt_df <- melt_df[order(melt_df$Date),]
  }

  # return
  return(melt_df)

}
