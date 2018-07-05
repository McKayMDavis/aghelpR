#' Login to OSF (Open Science Framework)
#'
#' @param osf_pat A Personal Access Token to access a private osf account.
#' This defaults to using a PAT stored as text file in www folder.
#' A string can be passed in here to sub for a different PAT.
#'
#' @return NULL - Only prints a statement welcoming user to OSF
#'
#' @example
#' login_to_osf()
#'
login_to_osf <- function(osf_pat = readr::read_lines("www/PAT")) {
  osfr::login(pat = osf_pat)
  osfr::welcome()
}
