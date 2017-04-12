#' Convert a malt quality dataset to Field Book
#'
#' @description Takes a data.frame of malt quality data and converts it
#' to a Field Book table format. This allows for easy database upload and for
#' spatial adjustment
#'
#' @param maltq.df A \code{data.frame} of raw malting quality data compiled
#' using the \code{\link{extract.maltq}} function with \code{tidy = FALSE}. The first
#' two columns should be the lab number (LabNo.), and the entry, (VarietyorSelection),
#' the last column should be the batch (Batch), and the remaining columns should
#' be the malt quality traits.
#' @param lab.to.plot A \code{data.frame} with lab number (LabNo.) and the
#' corresponding plot number of the trial from which grain was sampled for malting
#' analysis.
#' @param field.book A \code{data.frame} of the blank field.book table listing plot_id,
#' plot, trial, line_name, row, column, entry, rep, and notes.
#'
#' @return A modified field.book \code{data.frame} with the raw data from malting
#' quality.
#'
#' @import tidyr
#' @import dplyr
#'
#' @export
#'
#'
maltq_to_fb <- function(maltq.df, lab.to.plot, field.book) {

  ## Error
  # Make sure the field.book input has the right columns
  fb.columns <- c("plot_id", "plot", "trial", "line_name", "row", "column",
                  "entry", "rep", "notes")
  if (any(!fb.columns %in% colnames(field.book)))
    stop(paste0(c("The 'field.book' input does not appear to be the proper format. Make sure
         the following data is included in the input:", fb.columns), collapse = " "))

  # Make sure the maltq.tidy input has the right columns
  mq.columns <- c("LabNo.", "VarietyorSelection", "Batch")
  if (any(!mq.columns %in% colnames(maltq.df)))
    stop(paste0(c("The 'maltq.df' input does not appear to be the proper format. Make sure
         the following data is included in the input:", mq.columns), collapse = " "))

  # Make sure the lab.to.plot input has the right columns
  ltp.columns <- c("LabNo.", "Plot")
  if (any(!ltp.columns %in% colnames(lab.to.plot)))
    stop(paste0(c("The 'lab.to.plot' input does not appear to be the proper format. Make sure
                  the following data is included in the input:", ltp.columns), collapse = " "))

  # Data validation
  maltq.df <- as.data.frame(maltq.df)
  lab.to.plot <- as.data.frame(lab.to.plot)
  field.book <- as.data.frame(field.book)

  # Convert inputs to tibble
  maltq.df1 <- tbl_df(maltq.df)
  lab.to.plot1 <- tbl_df(lab.to.plot)
  field.book1 <- tbl_df(field.book)

  # Convert lab numbers in maltq.tidy1 to integer
  maltq.df1$LabNo. <- as.integer(maltq.df1$LabNo.)
  lab.to.plot1$LabNo. <- as.integer(lab.to.plot1$LabNo.)


  # Join
  tidy.merge <- full_join(x = maltq.df1, lab.to.plot, by = "LabNo.")
  # Rename Plot to plot
  colnames(tidy.merge) <- sub(pattern = "Plot", replacement = "plot", colnames(tidy.merge))

  # Join on plot
  fb.join <- left_join(x = field.book1, y = tidy.merge, by = "plot")

  # Remove the columns from maltq.df1 data.frame and the lab.to.plot1 data.frame
  remove.columns <- c(mq.columns, colnames(lab.to.plot))
  fb.join1 <- fb.join %>%
    select(which(!colnames(fb.join) %in% remove.columns))

  # Return the output
  return(as.data.frame(fb.join1))

} # Close the function










