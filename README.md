
# git4r

<!-- badges: start -->
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
to full git-scm or equivalent to make drastic repairs.

This tool set is designed to be minimal but sufficient to get the key advantages 
of using git:
 - tracking changes with an audit-trail of commit messages
 - undoing mistakes
 - visualising the changes made between versions of a file or file structure
 - working on many parallel version (branches) and effortlessly swapping and merging
 - synchronising with a remote version to collaborate with others and provide backup


## Installation

You can install the latest version of git4r from its git repository. The
package uses environmental variables for default behaviour, including personal
access tokens (which are not required if you use a local remote directory).

``` r
remotes::install_git('path/to/git/git4r')

file.edit('~/.Renviron')
# Copy these environmental variables used by the package
# GIT_DEFAULT_REMOTE can be used where a shared-drive remote
# is wanted, for example on a corporate network
GIT_DEFAULT_REMOTE=L:/ocal/shared/directory
GIT_USER=myname
GIT_EMAIL=myemail@company.com
GIT_PAT_GITHUB=credentials-are-automatically-pulled-from-here
GIT_PAT_BITBUCKET=or-here-whichever-name-matches-the-url-best
```

## Example

For interactive demonstration run `demo('git-intro')` and `demo('git-remote')`
(note, this changes working directory to a temporary example folder and 
overwrites GIT_DEFAULT_REMOTE with a test value).

All functions are designed to be used interactively and, hopefully, intuitively.

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

