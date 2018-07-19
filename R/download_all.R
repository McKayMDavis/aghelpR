#' Download all data files from each project (in "Data" folder) in main OSF project.
#'
#' @param download_local Logical. If TRUE, files will be downloaded to specified local directory.
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @param path Path to download files. Defaults to working directory.
#'
#' @return Nothing. If download_local == FALSE, then saves list of dataframes to global environment.
#'
#' @export download_all

download_all <- function(download_local = FALSE, id = "judwb", path = getwd()) {
  links <- get_all_file_links(id)

  if (download_local == TRUE) {
    j <- 1

    message("Downloading files:")
    for (i in 1:length(links$link)) {
      f_path <- paste0(path, "/", links$name[[i]])
      httr::GET(links$link[[i]], config = get_config(TRUE),
                httr::write_disk(f_path, overwrite = TRUE))

      cat(j, "/", length(links$link), "\n")
      j <- j + 1
    }
  } else {
    j <- 1

    tmp <- tempdir()
    df_list <- vector(mode = "list")

    message("Reading data:")
    for (i in 1:length(links$link)) {
      f_path <- paste0(tmp, "/", links$name[[i]])
      httr::GET(links$link[[i]], config = get_config(TRUE),
                httr::write_disk(f_path, overwrite = TRUE))

      if (stringr::str_detect(links$name[[i]], ".csv")) {
        # Read the file to unique name
        f_name <- gsub(".csv", "", links$name[[i]])
        df_list[[f_name]] <- suppressMessages(readr::read_csv(f_path))
      } else if (stringr::str_detect(links$name[[i]], ".xlsx")) {
        # Read the file to unique name
        f_name <- gsub(".xlsx", "", links$name[[i]])
        df_list[[f_name]] <- readxl::read_xlsx(f_path)
      } else if (stringr::str_detect(links$name[[i]], ".xls")) {
        f_name <- gsub(".xls", "", links$name[[i]])
        df_list[[f_name]] <- readxl::read_xls(f_path)
      } else {
        message(paste("File",
                      links$name[[i]],
                      "not read in due to unused file extension."))
      }

      cat(j, "/", length(links$link), "\n")
      j <- j + 1
    }

    osfr_list_data <<- df_list
  }

}
