% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_remote.R
\name{git_clone}
\alias{git_clone}
\title{Git Clone from Default Remote}
\usage{
git_clone(repo_name = "", to = "~/")
}
\arguments{
\item{repo_name}{Name of repo to clone, leave blank to display a list of available
repo names. Also accepted is the url or complete filepath to a
repo to clone (identified if containing '/').}

\item{to}{Local path to create local project folder in, defaults to home directory}
}
\value{
Invisible NULL
}
\description{
Clone another repository given as a URL of absolute filepath. Alternatively,
if \code{GIT_DEFAULT_REMOTE} environmental variable has been set
a list of repositories within this path is displayed and can be cloned by name.
}
\details{
Running \code{repo_name=""} will print a list of valid repositories found at the
\code{GIT_DEFAULT_REMOTE} path, and can then be re-run with one of these names to
clone a local version. This is to make basic on-premises collaboration and backups
more effortless. \code{GIT_DEFAULT_REMOTE} can be set in .Renviron file as per \code{?git_remote}

Other repositories can be cloned as expected by giving a URL. Credentials are handled
automatically in the same way as \code{?git_push}.
}
