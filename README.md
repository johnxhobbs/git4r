
# git4r

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/git4r)](https://CRAN.R-project.org/package=git4r)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Git For R

The goal of git4r is to allow fast use of git tracking and branching with 
helpful interactive functions, built upon the `git2r` package. Default behaviour
is designed to use a shared-drive directory as a remote, particularly helpful 
for corporate teams unable to access cloud services with proxy issues. 
Furthermore, since it is built on `git2r`, a system-wide git is not required.

Using git has been simplified by forcing certain behaviour, such as ensuring all
changes are committed before changing to another branch, and only allowing 
changes to be pulled from the 'origin' remote. This is designed to reduce 
frustrations, conflicts, and lost work. However no use of git is ever pain-free
and it is expected that when these tools hit their limit, users may have to resort
to full git-scm or equivalent to make more drastic repairs.

This tool set is designed to be minimal but sufficient to get the key advantages 
of using git:
 - tracking changes with an audit-trail of commit messages
 - undoing mistakes
 - visualising the changes made between versions of a file or folder
 - working on many parallel version (branches) and effortlessly swapping and merging
 - synchronising with a remote version to collaborate with others and provide backup


## Installation

You can install the latest version of git4r from its git repository. The
package uses environmental variables for default behaviour, including personal
access tokens (which are not required if you use a local remote directory).

Use of `GIT_DEFAULT_REMOTE` is entirely optional and is particularly for 
organisations wishing to have an entirely on-premises git workflow. 

``` r
install.packages('git4r')
# (This can be installed with dependencies=FALSE to avoid the large
#  dependency trail of diffr, however this is only for the most
#  extreme minimal installations, and git_diff() will not work)

# This package uses environmental variables to store the path of an on-premises
# remote (where push and clone will default to) and any Personal Access Tokens
# for cloud-based remote access.
file.edit('~/.Renviron')
GIT_DEFAULT_REMOTE=L:/ocal/shared/directory
GIT_PAT_GITHUB=credentials-are-automatically-pulled-from-here
GIT_PAT_BITBUCKET=or-here-whichever-name-matches-the-url-best
```

## Example

All functions are designed to be used interactively and, hopefully, intuitively.

See the vignette for a very basic introduction to git concepts, the documentation
for each function, or just make a temporary working directory and have a go.

``` r
library(git4r)

getwd()  # Everything inside this folder will be git-change-controlled
         # Usually this will be the top-level project folder containing .Rproj

# The workhorse of git is adding files, committing, and push/pull changes to 
# a remote copy of the repository. See:
git_pull()
git_add()
git_commit()
git_push()
git()    # <- this does all 4 at once!

# If using a GIT_DEFAULT_REMOTE folder as remote, others can now find it to clone
git_remote()
git_clone()

# Git really encourages users to keep separate branches for each separate task
# and then merge results together when happy. Swapping branch is made effortless:
git_branch()
git_merge()

# We can visualise changes between directories or files across different
# branches or historic commits, getting an audit trail of changes
git_history()
git_diff()

# And undo changes to a previous commit (helps to commit really regularly!)
# if things start going wrong. If it still is going wrong, find a full 
# installation of git-scm to make some fixes
git_undo()
```

