#' Read in one nurses' stress data file
#'
#' @param file_path A path to data file
#' @param max_rows Maximum of rows to read
#'
#' @returns Dataframe

read <- function(file_path, max_rows = 100) { # default argument, so if we dont specify max rows, this will be the default¨
  data <- file_path |> # this is the argument for the function, whatever path is put in, will be read in as a csv file.
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}

