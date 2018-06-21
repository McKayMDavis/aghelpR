#' Get url at the end of a file path. Helper function for move_waterbutler()
#'
#' @param list A vector of a file path
#'
#' @export get_node_url

get_node_url <- function(list) {
  for (i in (length(list) - 1)) {
    if (i == 1) {
      comp_dict <- get_dictionary(list[[1]], type = "files")
    } else {
      call <- httr::GET(url = comp_dict[[list[[i]]]], config = get_config(TRUE))
      res <- rjson::fromJSON(httr::content(call, 'text', encoding = "UTF-8"))
      node_ids <- vector(mode = "list")
      node_names <- vector(mode = "list")
      for (j in seq_along(res$data)) {
        node_ids[[j]] <- node_info$data[[j]]$links$move
        node_names[[j]] <- node_info$data[[j]]$attributes$name
      }
      names(node_ids) <- node_names
      comp_dict <- node_ids
    }
  }

  for (k in names(comp_dict)) {
    if (k == list[[length(list)]]) {
      node_url <- comp_dict[[k]]
    }
  }

  return(node_url) # WE SHOULD RETURN COMP_DICT AND MAKE THIS FUNCTION "GET_SUBFOLDER_DICT" eventually...

}


#' Move a file that is untouched from one component or folder to another component or folder
#'
#' @param from A file path beginning with the deepest level component id to the name of the file itself ("XXXX/foldername/filename")
#'
#' @param from A file path beginning with the deepest level component id to the name of the destination folder itself ("XXXX/foldername/subfoldername") or simply a component id
#'
#' @export move_waterbutler
move_waterbutler <- function(from, to) {
  from_list <- as.vector(strsplit(from, "/")[[1]], mode = "list")
  from_url <- get_node_url(from_list)
  file_name <- from_list[[length(from_list)]]

  to_list <- as.vector(strsplit(to, "/")[[1]], mode = "list")
  to_url <- get_node_url(to_list)

  path <- paste0(tempdir(), "/", file_name)
  to_folder_url <- paste0(to_url, "?kind=file&name=", file_name)

  httr::GET(from_url, config = get_config(TRUE), httr::write_disk(path, overwrite = TRUE))
  httr::PUT(to_folder_url, config = get_config(TRUE), body = httr::upload_file(path))
}
