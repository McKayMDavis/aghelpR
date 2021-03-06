% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_all.R
\name{download_all}
\alias{download_all}
\title{Download all data files from each project (in "Data" folder) in main OSF project.}
\usage{
download_all(download_local = FALSE, id = "judwb", path = getwd())
}
\arguments{
\item{download_local}{Logical. If TRUE, files will be downloaded to specified local directory.}

\item{id}{OSF id. Defaults to main Ag project.}

\item{path}{Path to download files. Defaults to working directory.}
}
\value{
Nothing. If download_local == FALSE, then saves list of dataframes to global environment.
}
\description{
Download all data files from each project (in "Data" folder) in main OSF project.
}
