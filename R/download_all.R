#' Download all data files from each project (in "Data" folder) in main OSF project.
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @param path Path to download files. Defaults to working directory.
#'
#' @export download_all

download_all <- function(id = "judwb", path = getwd()) {
  links <- get_all_file_links(id)

  j <- 1

  message("Downloading files:")
  for (i in 1:length(links$link)) {
    f_path <- paste0(path, "/", links$name[[i]])
    httr::GET(links$link[[i]], config = get_config(TRUE),
              httr::write_disk(f_path, overwrite = TRUE))

    cat(j, "/", length(links$link), "\n")
    j <- j + 1
  }
}
