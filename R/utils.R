#' Create authorization config (function from package osfr)
#'
#' @param login_required Boolean
#'
#' @return configuration for use in httr request
#' @export get_config

get_config <- function(login_required) {
  config <- list()

  if (login_required) {
    config <- httr::add_headers(Authorization = sprintf('Bearer %s', osfr::login()))
  }

  return(config)
}


#' Get a dictionary (vector) of files, folders, or children using the parent node id
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node
#'
#' @param type Type of OSF child ("children", "files", "folders") for which you are obtaining a dictionary
#'
#' @return A dictionary (vector) with values = ids or WaterButler Links if no id available and keys = names of children
#' @export get_dictionary

get_dictionary <- function(id, type) {

  node_ids <- vector(mode = "list")
  node_names <- vector(mode = "list")

  if (type == "children") {
    node_info <- osfr::get_nodes(id = id, private = TRUE, children = TRUE)

    for (i in seq_along(node_info$data)) {
      node_ids[[i]] <- node_info$data[[i]]$id
      node_names[[i]] <- node_info$data[[i]]$attributes$title
    }

  } else if (type == "files" || type == "folders") {
    # THIS MAY NEED TO BE CHANGED TO get_file_info
    node_info <- osfr::get_nodes(id = id, private = TRUE, files = TRUE)

    for (i in seq_along(node_info$data)) {
      node_ids[[i]] <- node_info$data[[i]]$links$move
      node_names[[i]] <- node_info$data[[i]]$attributes$name
    }

  } else {
    stop('Supported types are "children", "files", and "folders".')
  }

  # Generate dictionary with 'values = component ids' and 'keys = component names'
  names(node_ids) <- node_names

  return(node_ids)
}


#' Move a file from one location to another using WaterButler links.
#'
#' @param files A dictionary of key = file names and value = Waterbutler link
#'
#' @param folders A dictionary of key = folder names and value = Waterbutler link
#'
#' @param file_name Name of the file to be moved
#'
#' @param folder_name Name of the folder that gets the moved file
#' @export move

move <- function(files, folders, file_name, folder_name) {
  path <- paste0(tempdir(), "/", file_name)
  to_folder_url <- paste0(folders[[folder_name]], "?kind=file&name=", file_name)



  get_req <- httr::GET(files[[file_name]], config = get_config(TRUE), httr::write_disk(path, overwrite = TRUE))
  cat("GET Code:", get_req$status_code, "\n")
  put_req <- httr::PUT(to_folder_url, config = get_config(TRUE), body = httr::upload_file(path))
  cat("PUT Code:", put_req$status_code, "\n")
}


#' Get all file links from project
#'
#' @param id OSF id. Defaults to main Ag project.
#'
#' @export get_all_file_links

get_all_file_links <- function(id = "judwb") {
  file_links <- data.frame(link = rep(NA, 20000),
                           name = NA,
                           year = NA,
                           project = NA)

  message("Building dictionary")
  if (exists("get_file_dict", envir = .GlobalEnv)) {
    get_file_dict <- get("get_file_dict", envir = .GlobalEnv)
  } else {
    get_file_dict <- suppressMessages(aghelpR::concoct_directory(id))
    get_file_dict <<- get_file_dict
  }

  o <- 1
  message("Issuing requests:")
  for (i in seq_along(get_file_dict$data)) {
    for (j in get_file_dict$data[[i]]$attributes$title) {
      for (k in seq_along(get_file_dict$components[[j]]$data)) {
        for (l in get_file_dict$components[[j]]$data[[k]]$attributes$title) {
          # Files stored differently in different years ISSUE WITH 2016
          if (j %in% c("2014", "2015", "2017")) {
            for (m in seq_along(get_file_dict$components[[j]]$files[[l]]$data)) {
              if (!is.null(get_file_dict$components[[j]]$files[[l]]$data[[m]]$links$download)) {
                file_links$link[o] <- get_file_dict$components[[j]]$files[[l]]$data[[m]]$links$download
                file_links$name[o] <- gsub(" ", "_",
                                           get_file_dict$components[[j]]$files[[l]]$data[[m]]$attributes$name)
                file_links$year[o] <- j
                file_links$project[o] <- gsub(" ", "_", l)
                message(o)
                o <- o + 1
              }
            }
          } else {
            for (name in get_file_dict$components[[j]]$files[[l]]$data) {
              if (stringr::str_detect(name$attributes$name, "Data")) {
                file_url <- get_file_dict$components[[j]]$files[[l]]$data[[1]]$relationships$files$links$related$href

                if (!is.null(file_url)) {
                  req <- httr::GET(file_url, config = get_config(TRUE))
                  res <- rjson::fromJSON(httr::content(req, 'text', encoding = "UTF-8"))
                  for (m in seq_along(res$data)) {
                    file_links$link[o] <- res$data[[m]]$links$download
                    file_links$name[o] <- gsub(" ", "_", res$data[[m]]$attributes$name)
                    file_links$year[o] <- j
                    file_links$project[o] <- gsub(" ", "_", l)
                    message(o)
                    o <- o + 1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  # 1-174 works 175, 176 bad: fixed with empty error catcher in download_all
  file_links <- subset(file_links, !is.na(file_links$link))
  file_links <- file_links
  return(file_links)
}
