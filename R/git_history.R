

#' Git Checkout Commit
#'
#' Rewind the entire working directory to a previous commit and start a new branch
#' from there. Use this if you want to backtrack reversibly. If you want to
#' wind back irreversibly to a previous commit, use `git_undo()`.
#'
#' A typical work flow could be to retract three commits on the master branch.
#' Use `git_checkout(commit=git_get(n=3), onto_new_branch='temp')`. Then move back
#' to the master branch with `git_branch('master')` and merge the old version back
#' in with `git_merge('temp')`.
#'
#' The alternative (`git_undo()` and then select 3rd commit) will delete all record
#' of the three intermediate commits in the long run.
#'
#' As with changing branch, it is forbidden to change with changes yet-to-be-committed.
#' This is to prevent irreversible loss if you ever wanted to change back again.
#'
#' There is currently no option to checkout a specific file, however this can be
#' done by calling `git_diff()` and finding the named 'git4r_abc123' in
#' `tempdir()`
#'
#' @seealso git_get, git_history, git_diff, git_undo
#'
#' @param commit Commit object as returned by git_get() or git_history()
#' @param onto_new_branch Name of new branch to start from this historic commit
#' @returns Invisible NULL
#' @export
git_checkout = function(commit, onto_new_branch){

  if(!inherits(commit, 'git_commit')) stop('commit is not a valid git_commit object: use output of git_get() or git_history()')

  check_everything_committed()

  already_exists = onto_new_branch %in% names(git2r::branches())
  if(already_exists) stop('Branch \'',onto_new_branch,'\' already exists -- must create a new branch')

  print(commit)
  proceed = ask_proceed(paste0('Rewind working directory to this commit and start new branch ',onto_new_branch,'? (Y/N) '))
  if(proceed){
    git2r::checkout(object=commit, force=TRUE)
    message('Done')
    # This ends up with a detached HEAD every time
    # Can do the same thing with reset --hard which will change the HEAD, staging area, and working directory
    # git2r::reset(object=commit, reset_type='hard', path='.')

    git_branch(branchname=onto_new_branch)
  }
  return(invisible())
}

#' Reset To Any Previous Commit
#'
#' Reset working directory to any previous commit, or action. This will not change
#' the active branch so do this *first* if you want to continue on a different
#' branch. All previous commits are shown, even if the branch they were on has
#' been deleted, and this undo 'reset' itself will appear as a 'reflog' action.
#'
#' @param top Number of undo commits to show
#' @returns Invisible NULL
#' @export
git_undo = function(top = 10){
  check_everything_committed(warn=TRUE)

  head_history = git2r::reflog()
  for(i in 1:min(top,length(head_history))){
    cat(sprintf("%-3d",i-1))
    print(head_history[[i]])}

  which_history = ask_which('Which commit to reset to? (Hit ESCAPE to cancel) ')
  if(length(which_history)!=1){
    message('No changes made')
    return(invisible())
  }
  which_history = which_history[1] + 1
  if(!which_history %in% 1:top) stop('Could not reach undo step ',which_history-1)

  undo_commit = head_history[[which_history]]
  cat('Selected: ')
  print(undo_commit)
  # Dirty fix to allow us to reset to this hash
  class(undo_commit) = 'git_commit'
  proceed = ask_proceed('Confirm that you really want to change the working directory to historic state? (Y/N) ')
  if(proceed)
    git2r::reset(undo_commit, reset_type='hard')
  return(invisible())
}


#' Git Display History and Select Commit
#'
#' Display the history of commits in a specific branch with the option to filter.
#' The list of commits is returned invisibly and can be extracted using `[[i]]`
#' alternatively use `git_get()` to return the newest commit after filtering.
#'
#' By default the history of the current branch is returned. The commits of a remote
#' branch can be found using `branch='origin/master`, for example.
#'
#' The following filter arguments can be given:
#' - `n` - integer vector of commits back from latest (1 = most recent commit)
#' - `hash` - character to match the beginning of commit hash
#' - `before` and `after` - date object or string like '2021-01-31'
#' - `message` - regular expression string to find in commit message
#' - `author` and `email` - regular expression string of author or email of the commit
#'
#' If nothing could be matched, `get_git()` will start looking on other branches,
#' for example when looking for a particular hash. The user is notified if this
#' happens.
#'
#' @returns
#' Return the git_commit object which best matches a wide variety of types. This
#' is used for git_diff() and git_checkout().
#'
#' @param branch Name of branch to display. If a match cannot be found with
#'               git_get() in this branch, all branches are searched. Remote
#'               branches can be viewed by using the format "origin/master"
#'               providing that fetch or pull has downloaded changes.
#' @param top    Maximum number of commits to display (post-filtering)
#' @param path   Only display / search commits which affect this path (dir or file)
#' @param ...    Filters such as before='2021-01-01' or author='somebody'
#' @returns Vector of commit objects invisibly
#' @export
git_history = function(path = '.', branch = NULL, top = 10, ...){
  if(!is.null(branch)){
    if(grepl('/',branch)) branchref=paste0('refs/remotes/',branch)
    else branchref=paste0('refs/heads/',branch)
  } else branchref=NULL

  if(path=='.') path = NULL

  commits_so_far = git2r::commits(path = path, ref = branchref)

  if(length(list(...))>0)
    commits_so_far = git_filter_commits(commits_so_far, ...)

  # TODO add author initials, eg first 3
  top = min(top, length(commits_so_far))
  for(i in 1:top) {cat(sprintf('%3d ',i)); print(commits_so_far[[i]])}

  return(invisible(commits_so_far))
}

#' Get a Git Commit from Identifier
#' @rdname git_history
#' @returns Single commit object
#' @export
git_get = function(path = '.', branch = NULL, ...){

  # Search specified branch, or default (null) is current branch
  if(!is.null(branch)){
    if(grepl('/',branch)) branchref=paste0('refs/remotes/',branch)
    else branchref=paste0('refs/heads/',branch)
  } else branchref=NULL

  if(path=='.') path = NULL
  commits_so_far = git2r::commits(ref=branchref, path=path)

  # If multiple matches, take the first (= newest)
  tryCatch({
    correct_commit = git_filter_commits(commits_so_far, ...)[[1]]
    if(length(correct_commit) > 0) return(correct_commit)},
    error=function(err) {message('Searching other branches...')})

  # If it DID NOT find a match, try search EVERY branch
  branches = names(git2r::branches())
  # Convert to the branchref prefix
  branchrefs = paste0(ifelse(grepl('/',branches),'refs/remotes/','refs/heads/'),branches)

  commits_so_far = lapply(branchrefs, function(each_branch)
    git2r::commits(ref=each_branch, path=path) )

  # Combine into a single list, removing duplicates in multiple branches
  commits_so_far = unlist(commits_so_far, recursive=FALSE)
  commits_so_far = commits_so_far[!duplicated(commits_so_far)]

  # This will throws an error itself if zero results
  correct_commit = git_filter_commits(commits_so_far, ...)[[1]]

  return(correct_commit)
}


# Mechanism for filtering, called by history and get_commit
# Returns: a list of commits, maybe of 0, 1 or many
git_filter_commits = function(list_of_commits, n=NULL, before=NULL,
                              after=NULL, hash=NULL, author=NULL,
                              email=NULL, message=NULL){

  commits_tabulated = do.call(rbind,lapply(list_of_commits,data.frame))

  # Start with all commits being returned, and then remove with each filter
  correct = rep(TRUE, length(list_of_commits) )

  if(!is.null(n)){
    if(n > length(list_of_commits)){
      n = length(list_of_commits)
      message('n exceeds the number of unique commits, returning first commit with n = ',n)
    }
    correct[-n] = FALSE}
  if(!is.null(before))
    correct = correct & (as.Date(commits_tabulated$when) <= before)
  if(!is.null(after))
    correct = correct & (as.Date(commits_tabulated$when) >= after)
  if(!is.null(hash))
    correct = correct & grepl(paste0('^',gsub('[^a-zA-Z0-9]','',hash)), commits_tabulated$sha)
  if(!is.null(message))
    correct = correct & grepl(toupper(message), toupper(commits_tabulated$summary))
  if(!is.null(author))
    correct = correct & grepl(toupper(author), toupper(commits_tabulated$author))
  if(!is.null(email))
    correct = correct & grepl(toupper(email), toupper(commits_tabulated$email))

  if(sum(correct)==0) stop('Could not find any commits which matched filter')
  return(list_of_commits[correct])
}

#' Git Diff
#'
#' Display the difference between two commits of a file or a directory structure.
#' Paths must be relative to the top-level git directory so it is recommended that
#' `getwd()` is the same so that tab auto-complete gives the right path. The default
#' filter is `n=1` and `NULL` meaning compare working tree (or a file contents)
#' of latest commit with current working directory.
#'
#' Two commits can be specified using `git_get()` or `git_history()[[i]]`, or a
#' single named filter argument for each commit for simple requests. Use `NULL`
#' to compare with the current working file (not even committed yet).
#'
#' Hidden files (for example '.Rbuildignore') are omitted when listing current
#' directory contents (a commit identifier of NULL), neither is .gitignore
#' respected, so all ignored files will be flagged up.
#'
#' Usually the more recent commit should be second so that additions are shown
#' in green.
#'
#' Previous versions of files can be opened for editing by finding them at the
#' `tempdir()` path.
#'
#'
#' See ?git_history for the filter arguments that can be used
#'
#' @seealso git_history
#' @examples
#' \dontrun{
#' # Compare the last committed change with current working version
#' git_diff() # this is exactly equivalent to next line
#' git_diff('.', n=1, NULL)
#' git_diff('README.md')
#' git_diff('README.md', n=2)
#' git_diff('README.md', n=2, n=1)
#'
#' # Compare the directory structure now with a previous date
#' git_diff('.', before='2021-10-25')
#'
#' # Latest between branches
#' git_diff('myfile', branch='master', branch='test')
#'
#' # Compare file contents of a commit hash with most recent by author
#' git_diff('R/git_helper.R', hash='abc123', author='somebody')
#'
#' # Can also get a git_commit object directly to pass over if multiple filtering
#' git_diff('R/', git_get(author='somebody', before='2021-10-25'), NULL)
#'
#' # Or even taking a commit number from the branch history (but will be verbose)
#' git_diff('README.md', git_history()[[1]], n=1 )
#' }
#'
#' @param path Path or directory to compare - must be relative to the
#'             root directory of the repository, regardless of whether `getwd()`
#'             is a subdirectory.
#' @param ...  Two arguments to be passed for which versions to select: can be
#'             a commit object from git_history() or get_git(), or a single filter
#'             argument which is passed to get_git().
#' @returns A diffr htmlwidget object which is automatically opened in RStudio Viewer tab
#' @export
git_diff = function(path = '.', ...){
  if(path=='') path='.'

  # INTERPRET INPUT ARGS TO GET COMMIT
  # If only one arg is given, compare with now. If zero given, compare last commit with now.
  commit_args = list(...)
  if(length(commit_args)>2) stop('Must give upto two commit identifiers')
  if(length(commit_args)==1) commit_args = c(commit_args, list(NULL))
  if(length(commit_args)==0) commit_args = list(n=1, NULL)
  # message('Not two commit identifiers given, defaulting to diff from last commit:')
  # message(paste0('\tgit_diff(path=\'',path,'\', n=1, NULL)'))

  if(inherits(commit_args[[1]],'git_commit')) commit1 = commit_args[[1]]
  else if(is.null(commit_args[[1]])) commit1 = NULL
  else commit1 = do.call(git_get,c(list(path=path),commit_args[1]))

  if(inherits(commit_args[[2]],'git_commit')) commit2 = commit_args[[2]]
  else if(is.null(commit_args[[2]])) commit2 = NULL
  else commit2 = do.call(git_get,c(list(path=path),commit_args[2]))

  if(!is.null(commit1) & length(commit1)==0) stop('Commit1 not defined -- use git_history() to find a valid commit')
  if(!is.null(commit2) & length(commit2)==0) stop('Commit2 not defined -- use git_history() to find a valid commit')

  # INITIALISE TEMPFILES
  # Possibly use the same file over and over if comparing huge files (while build up in tempdir)
  file1 = tempfile(pattern='git4r_')
  file2 = tempfile(pattern='git4r_')

  # HELPER TO WRITE FILE FROM COMMIT
  write_diff_copy = function(each_commit, each_file){
    # Write header for the diff file # EDIT  this is now done as diffr header
    # if(is.null(each_commit)) write('RIGHT NOW VERSION\n-------------', each_file)
    # else
    write(get_metadata(each_commit), each_file)

    rootpath = dirname(git2r::discover_repository('.'))
    fullpath = paste0(rootpath,'/',path)
    # If directory, compare trees, not actual file
    if(dir.exists(fullpath)){
      # If not '.' then make SURE there is trailing slash, then make sure no double slash TODO check network path
      if(path!='.') path = paste0(path,'/')
      path = sub('[\\/]+$','/',path)  # if network path, maybe use '(?!=^)[\\/]+$'

      if(is.null(each_commit)){
        # Always list relative to root - might take a fraction longer
        tree1 = list.files(path=rootpath, recursive=TRUE)
        # trim to get just the path we want
        if(path!='.') tree1 = tree1[grepl(paste0('^',path), tree1)]
        tree_infos = file.info(paste0(rootpath,'/',tree1))
        tree1 = data.frame(size=tree_infos$size, path=tree1)
      }
      else{
        tree1 = git2r::ls_tree(git2r::tree(each_commit))
        if(path!='.') tree1 = tree1[tree1$path==path,]
        tree1 = data.frame(size=tree1$len,path=paste0(tree1$path,tree1$name))
      }
      tree_fomatted = data.frame(SIZE=format(tree1$size, big.mark=',', width=10),
                                 PATH=tree1$path)
      tree_fomatted = tree_fomatted[order(tree_fomatted$PATH),]

      treelines = paste(tree_fomatted$SIZE, tree_fomatted$PATH)
      write(treelines, each_file, append=TRUE)
      # write.table(tree_fomatted, each_file, quote=FALSE, row.names=FALSE, append=TRUE)
    }
    else{
      # Is a single file
      if(is.null(each_commit)) write(readLines(fullpath), each_file, append=TRUE)
      else write(git2r::content(read_git_tree(each_commit, path)), each_file, append=TRUE)
    }
  }

  write_diff_copy(commit1, file1)
  write_diff_copy(commit2, file2)

  return(diffr::diffr(file1=file1, file2=file2, before=basename(file1), after=basename(file2)))
}

# Helper for git_diff()
get_metadata = function(commit){
  if(is.null(commit)){
    commit_when = Sys.time()
    commit_who = 'valid at this timestamp'
    commit_message = ''
    commit_hash = '>>> CURRENT WORKING DIRECTORY <<<'
  }
  else{
    metadata = unlist(commit)
    #git2r::when(commit)
    commit_when = as.POSIXct(as.integer(metadata['author.when.time']), origin='1970-01-01')
    commit_who = metadata['author.name'] # ,metadata['author.when.time']
    commit_message = metadata['message']
    commit_hash = metadata['sha']
  }

  return(paste0(commit_hash,'\n"',commit_message,'"\n',commit_when,' -- ',commit_who,'\n----------------------------------------'))
}

# Helper for git_diff()
read_git_tree = function(commit, filepath){
  split_path = strsplit(filepath,'/')[[1]]
  blobtree = git2r::tree(commit)
  for(layer in 1:length(split_path))
    blobtree = blobtree[split_path[layer]]
  return(blobtree)
}
