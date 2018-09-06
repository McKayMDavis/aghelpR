#' Move files from one location to another in OSF using a parent node id.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)
#'
#' @param year The current year component or year component to which file moving is desired
#'
#' @param from The file number from which the user wishes to begin. Defaults to the first file in the data folder.
#'
#' @param to The file number that we want to end the file moving process on. Defaults to the last file in the data folder. Useful to change if warnings indicate files that failed to upload.
#' @export move_to_projects

# NEED TO:
# 3. Then reupload files
# 4. Add a "name" variable to allow user to upload a file by name/project/whatever
move_to_projects <- function(id = "judwb", year, from = 1, to) {
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

  if (to) {
    len_cur <- c(from:to)
  } else {
    len_cur <- c(from:len)
  }

  # Move files
  for (j in names(project_components)) {
    for (k in len_cur) {
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
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Data",
               file_num = k)
        } else if (stringr::str_detect(l, ".txt")) {
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Journal",
               file_num = k)
        } else {
          move(files = current_files,
               folders = folders_dict,
               file_name = l,
               folder_name = "Pictures",
               file_num = k)
        }
        cat(cur, "/", len, "\n")
        cur <- cur + 1
      }
    }
  }
}


