% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/move_to_projects.R
\name{move_to_projects}
\alias{move_to_projects}
\title{Move files from one location to another in OSF using a parent node id.}
\usage{
move_to_projects(id = "judwb", year, from = 1, to)
}
\arguments{
\item{id}{OSF id (osf.io/XXXX) (just "XXXX") of the parent node (Agriculture Division main project component id)}

\item{year}{The current year component or year component to which file moving is desired}

\item{from}{The file number from which the user wishes to begin. Defaults to the first file in the data folder.}

\item{to}{The file number that we want to end the file moving process on. Defaults to the last file in the data folder. Useful to change if warnings indicate files that failed to upload.}
}
\description{
Move files from one location to another in OSF using a parent node id.
}
