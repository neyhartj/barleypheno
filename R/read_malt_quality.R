#' Read in malt quality data
#'
#' @description
#' Loads malt quality data typically received in the University of Minnesota
#' Barley Breeding Group. This data often comes from the USDA Cereal Laboratory
#' in Madison, Wisconsin. Data from this lab is usually formatted consistently,
#' permitting use of a common function.
#'
#' @param files A \code{character} vector of filenames or filepaths of the malt
#' quality Excel files to read in.
#' @param tidy A \code{logical} indicating whether the output should be in tidy
#' format. See \code{Details} or \code{\link[tidyr]{gather}}
#'
#' @return A \code{list} with the following elements:
#'
#' $data A \code{data.frame} of malting quality values for experimental entries
#'
#' $stats A \code{data.frame} of summary statistics from each of the batches
#'
#' $checks A \code{data.frame} of malting quality values for the laboratory checks
#'
#' @import tidyr
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @export
#'
extract.maltq <- function(files, tidy = FALSE) {

  ## Error reporting
  # Make sure the files are Excel files
  if ( !any(grepl(pattern = paste0(c(".xls$", ".xlsx$"), collapse = "|"), files)) )
    stop("The filenames in 'files' do not have the extension '.xls' or '.xlsx'.")

  # Blank output list
  maltq.compiled <- list()

  # Iterate over the files, read in the data, and reformat
  for (f in files) {

    # Read in the file
    data.f <- read_excel(path = f, col_names = FALSE)

    # Record the table number
    table.no <- as.character(data.f[2,1])

    # Remove the first two rows
    data.f1 <- slice(data.f, -c(1:2))

    # Subset the next 3 rows (these are the variable identifiers)
    var.names <- slice(data.f1, 1:3) %>%
      # Collapse into a single string
      apply(X = ., MARGIN = 2, FUN = function(var) paste0(na.omit(var), collapse = "")) %>%
      # Remove spaces
      gsub(pattern = " ", replacement = "", x = .) %>%
      # Remove other annoying characters
      gsub(pattern = "\"", replacement = "", x = .) %>%
      as.character()

    # Replace the names in the dataset
    data.f2 <- slice(data.f1, -c(1:3))
    colnames(data.f2) <- var.names

    # Find the next row with the table name
    table.row <- which(select(data.f2, LabNo.) == table.no)
    # If that table name does not occur again, skip
    if (length(table.row) != 0) {
      # Remove that row and the next 3 rows
      data.f2 <- slice(data.f2, -seq(table.row, table.row + 3))
    }

    # Find the row with "Coefficients of Variation"
    coef.row <- which(select(data.f2, LabNo.) == "Coefficients of Variation")
    # Remove all data after this row
    data.f3 <- slice(data.f2, -((coef.row + 1):nrow(data.f2)))

    # Remove rows with all NA
    data.f3 <- filter(data.f3, !apply(X = data.f3, MARGIN = 1, FUN = function(row) all(is.na(row)) ))

    # Iterate over the variable names except for LabNo. and VarietyorSelection,
    ## extract numeric values, and replace those values in the data.frame
    for (varname in var.names[!var.names %in% c("LabNo.", "VarietyorSelection")]) {
      suppressWarnings(data.f3[[varname]] <- extract_numeric(data.f3[[varname]]))
    }


    # Add a new column with the table number
    data.f3 <- mutate(data.f3, Batch = table.no)
    # Remove the OverallRank column
    data.f3 <- select(data.f3, -OverallRank)


    # Cut out the checks
    check.slice <- data.f3$VarietyorSelection %>% grep(pattern = "MALT CHECK")
    check.f <- slice(data.f3, check.slice)
    # Cut out the stats
    stats.slice <- grep(pattern = "^[^0-9]{1,}", x = data.f3$LabNo.)
    stats.f <- slice(data.f3, stats.slice)
    # Create a final data.frame
    data.f4 <- data.f3 %>%
      slice(-c(check.slice, stats.slice)) %>%
      # Sort on entry name
      arrange(VarietyorSelection)

    # Add to the list
    maltq.compiled[[table.no]] <- list(data = data.f4, checks = check.f, stats = stats.f)
  }

  # Combine each set of data.frame
  maltq.data <- bind_rows(lapply(X = maltq.compiled, FUN = function(batch) batch$data)) %>%
    arrange(VarietyorSelection)
  maltq.stats <- bind_rows(lapply(X = maltq.compiled, FUN = function(batch) batch$stats))
  maltq.checks <- bind_rows(lapply(X = maltq.compiled, FUN = function(batch) batch$checks))


  # Rearrange if requested
  if (tidy) {
    maltq.data.gather <- select(maltq.data, -LabNo.) %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch)
    maltq.stats.gather <- select(maltq.stats, -LabNo.) %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch)
    maltq.checks.gather <- select(maltq.checks, -LabNo.) %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch)

    # Output list
    return(list(data = maltq.data.gather,
                stats = maltq.stats.gather,
                checks = maltq.checks.gather))

  } else { # Otherwise just output a list

    return(list(data = as.data.frame(maltq.data),
                stats = as.data.frame(maltq.stats),
                checks = as.data.frame(maltq.checks)))

  }

} # Close the function
