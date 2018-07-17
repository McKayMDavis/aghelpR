#' Download all data files from each project (in "Data" folder) in main OSF project.
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @param path Path to download files. Defaults to working directory.
#'
#' @export download_all

download_all <- function(id = "judwb", path = getwd()) {
  links <- get_all_file_links(id)

  for (i in links) {
    httr::GET(i, config = get_config(TRUE),
              httr::write_disk(path, overwrite = TRUE))

  }
}
