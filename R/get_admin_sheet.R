#' Retreive Master Admin Project Sheet
#'
#' @param osf_file_id A string containing the code at the end of an OSF url.
#' For example, https://osf.io/z8kpa/.
#' The default string value is directly to the master sheet.
#'
#' @return A tibble in long format
#'
#' @example
#' login_to_osf()
#' admin_dat <- get_admin_sheet()
#'
#' @export get_admin_sheet

get_admin_sheet <- function(osf_file_id = "z8kpa") {

  admin_dat <- osfr::download_files(id = osf_file_id,
                                    path = tempdir(),
                                    private = TRUE) %>%
    readxl::read_xlsx()

  # Clean data
  test <- admin_dat %>%
    mutate(treatments = str_split(treatments, ",")) %>%
    unnest() %>%
    mutate(variables = str_split(variables, ",")) %>%
    unnest() %>%
    mutate(replications = map(replications, ~as.character(as.roman(1:.x)))) %>%
    unnest() %>%
    mutate(units_of_measurement = str_split(units_of_measurement, ",")) %>%
    unnest() %>%
    mutate_all(trimws) %>%
    select(type, project_name, treatments, variables, units_of_measurement, replications)

  return(test)
}
