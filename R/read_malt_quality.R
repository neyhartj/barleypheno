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
#' @import readr
#' @import dplyr
#' @import stringr
#' @importFrom readxl read_excel
#'
#' @export
#'
read_maltq <- function(files, tidy = FALSE) {

  ## Error reporting
  # Make sure the files are Excel files
  if ( !all(endsWith(x = files, suffix = c(".xlsx")) | endsWith(x = files, suffix = c(".xls"))) )
    stop("The filenames in 'files' do not have the extension '.xls' or '.xlsx'.")

  # Blank output list
  maltq.compiled <- list()

  # Iterate over the files, read in the data, and reformat
  for (f in files) {

    # Read in the file
    data.f <- read_excel(path = f, col_names = FALSE) %>%
      # Remove columns that are all NA
      select(which(colMeans(is.na(.)) != 1))
    
    # Record the table number
    table.no <- data.f[2,1] %>%
      as.character()

    # Remove the first two rows
    data.f1 <- data.f %>%
      slice(-c(1:2))

    # Subset the next 3 rows (these are the variable identifiers)
    var.names <- slice(data.f1, 1:3) %>%
      # Collapse into a single string
      apply(X = ., MARGIN = 2, FUN = function(var) paste0(na.omit(var), collapse = "")) %>%
      # Remove spaces
      str_replace_all(pattern = " ", replacement = "") %>%
      str_replace_all(pattern = "\"", replacement = "") %>%
      as.character()

    # Replace the names in the dataset
    data.f2 <- slice(data.f1, -c(1:3))
    colnames(data.f2) <- var.names

    # Find the next row with the table name
    table.row <- which(data.f2$LabNo. == table.no)
    # If that table name does not occur again, skip
    if (length(table.row) != 0) {
      # Remove that row and the next 3 rows
      to.remove <- as.numeric(sapply(X = table.row, FUN = function(row) seq(row, row + 3)))
      data.f2 <- slice(data.f2, -to.remove)
    }

    # Find the row with "Coefficients of Variation"
    coef.row <- which(select(data.f2, LabNo.) == "Coefficients of Variation")
    # Remove all data after this row
    data.f3 <- slice(data.f2, -((coef.row + 1):nrow(data.f2)))

    # Iterate over the variable names except for LabNo. and VarietyorSelection,
    ## extract numeric values, and replace those values in the data.frame
    for (varname in var.names[!var.names %in% c("LabNo.", "VarietyorSelection")]) {
      suppressWarnings(data.f3[[varname]] <- parse_numeric(data.f3[[varname]]))
    }


    # Add a new column with the table number
    data.f3 <- mutate(data.f3, Batch = table.no)


    # Cut out the checks
    check.slice <- data.f3$VarietyorSelection %>%
      str_detect(pattern = "MALT CHECK") %>%
      which()
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
    maltq.data.gather <- maltq.data %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch, -LabNo.)
    maltq.stats.gather <- maltq.data %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch, -LabNo.)
    maltq.checks.gather <- maltq.data %>%
      gather(key = "Parameter", value = "Value", -VarietyorSelection, -Batch, -LabNo.)

    # Output list
    return(list(data = as.data.frame(maltq.data.gather),
                stats = as.data.frame(maltq.stats.gather),
                checks = as.data.frame(maltq.checks.gather)))

  } else { # Otherwise just output a list

    return(list(data = as.data.frame(maltq.data),
                stats = as.data.frame(maltq.stats),
                checks = as.data.frame(maltq.checks)))

  }

} # Close the function
