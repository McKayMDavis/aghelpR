#' Move files from one location to another in OSF using a parent node id.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)
#'
#' @param year The current year component or year component to which file moving is desired
#'
#' @param from The file number from which the user wishes to begin. Defaults to the first file in the data folder.
#' @export move_to_projects

move_to_projects <- function(id = "judwb", year, from = 1) {
  cat("Setting up. Please wait (This could take a couple minutes).\n")
  # Get current year's component, then get the project components (children) dictionary of that component
  all_child_components <- suppressMessages(get_dictionary(id = id, type = "children"))
  working_child_id <- all_child_components[[year]]
  project_components <- suppressMessages(get_dictionary(id = working_child_id, type = "children"))
  # NEED A WAY TO GET DELETE LINKS TO PASS TO MOVE FILES
  # NEED TO get_nodes ON TO COMPONENT ID
  # Get dictionary of files for current year
  data_component_id <- all_child_components[["Data"]]
  raw_data_component <- suppressMessages(get_dictionary(id = data_component_id, type = "children"))
  raw_data_component_id <- raw_data_component[["Raw_Data"]]
  files_dict <- suppressMessages(get_dictionary(id = raw_data_component_id, type = "files"))
  current_files <- vector(mode = "list")
  for (i in names(files_dict)) {
    if (stringr::str_detect(i, year)) {
      current_files[[i]] <- files_dict[[i]]
    }
  }
  len <- length(current_files)
  cur <- from

  # Move files
  for (j in names(project_components)) {
    for (k in c(from:length(current_files))) {
      l <- names(current_files[k])
      if (stringr::str_detect(tolower(l),
                              tolower(stringr::str_replace_all(j,
                                                               "[^[:alnum:]]",
                                                               "")))) {
        cat(l, "in project", j, ":", "\n")
        # HERE
        # Might need to issue GET requests to grab info on the files in the project components.
        proj_current <- suppressMessages(get_dictionary(project_components[[j]], type = "files"))
        for (m in names(proj_current)) {
          if (m %in% c("Data", "Journal", "Pictures")) {
            req <- httr::GET(proj_current[[m]], config = get_config(TRUE))
            res <- rjson::fromJSON(httr::content(req, 'text', encoding = "UTF-8"))
            for (o in seq_along(res$data)) {
              if (!is.null(res$data[[o]]$attributes$name)) {
                if (l == res$data[[o]]$attributes$name) {
                  httr::DELETE(res$data[[o]]$links$delete, config = get_config(TRUE))
                  cat("FILE: ", l, " DELETED\n")
                }
              }
            }
          }
        }

        folders_dict <- get_dictionary(id = project_components[[j]], type = "folders")
        if (stringr::str_detect(l, ".csv")) {
          move(files = current_files, folders = folders_dict, file_name = l, folder_name = "Data")
        } else if (stringr::str_detect(l, ".txt")) {
          move(files = current_files, folders = folders_dict, file_name = l, folder_name = "Journal")
        } else {
          move(files = current_files, folders = folders_dict, file_name = l, folder_name = "Pictures")
        }
        cat(cur, "/", len, "\n")
        cur <- cur + 1
      }
    }
  }
}


