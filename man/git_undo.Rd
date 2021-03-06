% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_history.R
\name{git_undo}
\alias{git_undo}
\title{Reset To Any Previous Commit}
\usage{
git_undo(top = 10)
}
\arguments{
\item{top}{Number of undo commits to show}
}
\value{
Invisible NULL
}
\description{
Reset working directory to any previous commit, or action. This will not change
the active branch so do this \emph{first} if you want to continue on a different
branch. All previous commits are shown, even if the branch they were on has
been deleted, and this undo 'reset' itself will appear as a 'reflog' action.
}
