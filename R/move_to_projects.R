#' Move files from one location to another in OSF using a parent node id.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)
#'
#' @param year The current year component or year component to which file moving is desired
#' @export move_to_projects

move_to_projects <- function(id = "judwb", year) {
  # Get current year's component, then get the project components (children) dictionary of that component
  all_child_components <- get_dictionary(id = id, type = "children")
  working_child_id <- all_child_components[[year]]
  project_components <- get_dictionary(id = working_child_id, type = "children")
  # NEED A WAY TO GET DELETE LINKS TO PASS TO MOVE FILES
  # NEED TO get_nodes ON TO COMPONENT ID
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
        cat(k, "in project", j, ":", "\n")
        # HERE
        # Might need to issue GET requests to grab info on the files in the project components.
        proj_current <- get_dictionary(project_components[[j]], type = "files")
        for (l in names(proj_current)) {
          if (l %in% c("Data", "Journal", "Pictures")) {
            req <- httr::GET(proj_current[[l]], config = get_config(TRUE))
            res <- rjson::fromJSON(httr::content(req, 'text', encoding = "UTF-8"))
            for (m in seq_along(res$data)) {
              if (!is.null(res$data[[m]]$attributes$name)) {
                if (k == res$data[[m]]$attributes$name) {
                  httr::DELETE(res$data[[m]]$links$delete, config = get_config(TRUE))
                  cat("FILE: ", k, " DELETED\n")
                }
              }
            }
          }
        }

        folders_dict <- get_dictionary(id = project_components[[j]], type = "folders")
        if (stringr::str_detect(k, ".csv")) {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Data")
        } else if (stringr::str_detect(k, ".txt")) {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Journal")
        } else {
          move(files = current_files, folders = folders_dict, file_name = k, folder_name = "Pictures")
        }
        cat(cur, "/", len, "\n")
        cur <- cur + 1
      }
    }
  }
}


