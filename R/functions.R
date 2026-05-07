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


read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)

  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")

  return(data)
}


get_participant_id <- function(data) {
  data_with_id <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        "(?<=/stress/)[:alnum:]{2}(?=/)"
      ),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data_with_id)
}

summarise_by_datetime <- function(data) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = "minute"
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        list(mean = mean, sd = sd, median = median)
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}


tidy_survey_data <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) %>%
    dplyr::select(-c(date, start_time, end_time, duration))

  return(tidied)
}
