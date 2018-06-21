#' Get a directory of all nodes, files, and folders within a parent node
#'
#' @description This function is recursive and can take a long time. Please plan accordingly.
#'
#' @param id OSF id (osf.io/XXXX) (just "XXXX") of the parent node
#'
#' @return A vector with parent node information and options to look at component or file information using extensions e.g. origin$files$ComponentName will return an rjson style vector of all data on files within ComponentName whereas origin$components$ComponentName will return information on all components within ComponentName
#'
#' @export concoct_directory

concoct_directory <- function(id) {

  children <- osfr::get_nodes(id = id, children = TRUE, private = TRUE)

  for (i in seq_along(children$data)) {
    if (length(children$data) != 0) {
      index <- children$data[[i]]$attributes$title
      children[["components"]][[index]] <- osfr::get_nodes(children$data[[i]]$id,
                                                           children = TRUE,
                                                           private = TRUE)

      # This works but not quite how we want
      children[["files"]][[index]] <- osfr::get_nodes(children$data[[i]]$id,
                                                           files = TRUE,
                                                           private = TRUE)

      children[["components"]][[index]] <- concoct_directory(children$data[[i]]$id)
    }
  }

  return(children)
}
