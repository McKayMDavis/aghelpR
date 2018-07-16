#' Move files from one location to another in OSF using a parent node id.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)
#'
#' @param year The current year component or year component to which file moving is desired
#' @export move_to_projects

move_to_projects <- function(id, year) {
  # Get current year's component, then get the project components (children) dictionary of that component
  all_child_components <- get_dictionary(id = id, type = "children")
  working_child_id <- all_child_components[[year]]
  project_components <- get_dictionary(id = working_child_id, type = "children")

  # Get dictionary of files for current year
  data_component_id <- all_child_components[["Data"]]
  raw_data_component <- get_dictionary(id = data_component_id, type = "children")
  raw_data_component_id <- raw_data_component[["Raw_Data"]]
  files_dict <- get_dictionary(id = raw_data_component_id, type = "files")
  current_files <- vector(mode = "list")
  for (i in names(files_dict)) {
    if (stringr::str_detect(i, year)) {
      current_files[[i]] <- files_dict[[i]]
    }
  }
  len <- length(current_files)
  cur <- 1

  # Move files
  for (j in names(project_components)) {
    for (k in names(current_files)) {
      if (stringr::str_detect(k, stringr::str_replace_all(j,
                                                 "[^[:alnum:]]",
                                                 ""))) {
        folders_dict <- get_dictionary(id = project_components[[j]], type = "folders")
        if (stringr::str_detect(k, ".csv")) {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Data")
        } else if (stringr::str_detect(k, ".txt")) {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Journal")
        } else {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Pictures")
        }
        print(paste0(cur, "/", len))
        cur <- cur + 1
      }
    }
  }
}


