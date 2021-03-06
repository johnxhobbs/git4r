

#' Git Branch
#'
#' Swaps to a different branch if exists, otherwise makes one from most recent
#' commit, and checks out this branch. If no branch name is specified, all
#' branches will be printed.
#'
#' Changing to a branch which is only listed under remote (for example origin/test)
#' by `git_branch('test')` will automatically create a local copy of this branch.
#'
#' If there are uncommitted changes, it will forbid changing to an existing
#' branch which would result in irreversible loss of current working directory.
#' If the branch does not exist yet, the working directory is not changed and
#' the next commit will be on this new branch.
#'
#' You are allowed to change branch to any which share the same latest commit
#' so you can create a branch and instantly remove it, whilst keeping any
#' uncommitted changes. The user is notified that the branch has been changed
#' invisibly (namely with no change to the working directory). This may result
#' in a warning that not all changes have been committed when returning much later
#' to the first branch if some changes are staged,
#'
#' Currently, this is the only situation you are allowed to delete a branch,
#' because in other situations it would result in irreversibly loss of commits
#' (although `git_undo()` will still work for a short time to find orphaned commits).
#' For now, housekeeping and deleting branches should be done manually with system
#' git or `git2r::branch_delete()` if you are really sure.
#'
#' @seealso git_merge
#'
#' @param branchname  Branch name to move to or create, use empty string "" to list
#'                existing branches. This is asked interactively if left as NULL.
#'
#' @returns Invisible NULL
#' @export
git_branch = function(branchname = NULL){
  current_head = git2r::repository_head()
  current_branch = current_head$name
  cat('Current active branch: ',current_branch)
  if(is.null(current_branch)) message('Detached HEAD -- this may have happened after checking out a commit')
  message('\n')

  # this can be simulated with this (see git_checkout() and how you MUST immediately move to a new  branch)
  # git2r::checkout(object=git_get(n=2), force=TRUE)

  branch = ask_generic('New or existing branch name to move to (or hit ENTER to list branches) ', answer=branchname)

  if(branch==''){
    for(each_branch in git2r::branches()) print(each_branch)
    if(!is.null(branchname)) return(invisible())
    # TODO -- add a delete-which-branch chooser with lots of safety catches
  }

  if(branch=='') branch = ask_generic('New or existing branch name to move to (or hit ENTER to cancel) ')
  # And if it is STILL blank, then time to move on
  if(branch=='') return(invisible())

  # Does this branch name exist?
  already_exists = branch %in% names(git2r::branches())
  # Is this actually identical to any others?

  if(already_exists==TRUE){
    # Check if share the same latest commit
    branches = git2r::branches()
    # Must find current wording directory hash - not always HEAD
    #current_hash = git2r::lookup_commit(current_head)$sha
    current_hash = git2r::revparse_single('.','HEAD')$sha
    branch_hash = git2r::lookup_commit(branches[[branch]])$sha

    if(current_hash==branch_hash){
      git2r::checkout(branch=branch, create=TRUE)
      cat('Moved invisibly from',current_branch,'back to',branch)
      if(isTRUE(!current_branch %in% c('main','master',branch))){
        if(ask_proceed(paste0('Delete the unused branch ',current_branch,'? (Y/N) ')))
          git2r::branch_delete(branches[[current_branch]])
      }
      # TODO
      # Remove the staged files from the branch you just left
      # This can cause some lingering behaviour when you eventually
      # go back to the first branch again
      # OR maybe this is caused by .gitignore changing between branches
      # possible sync check to make sure this is always the same?
      return(invisible())
    }

    # If you have lost your HEAD by checking out a commit - not a branch tip
    # AND the branch you are trying to move to is not at the current hash
    if(is.null(current_branch)){
      message('The branch you are trying to swap to does not have the same latest commit.')
      message('Either create a new branch, such as git_branch(\'temp\') and then merge.')
      message('Or use git_branch(\'\') to discover which branch DOES have the same hash.')
      message('You may only change to an existing branch if it has the hash [',substr(current_hash,1,6),']')
      return(invisible())
    }

    # If branch already exists, everything must be fully-committed, else error
    check_everything_committed()
  }

  # If there is detached HEAD then need to explicitly create branch first
  if(is.null(current_branch)){
    cat('\nCreating new branch',branch,'which should then be merged back with git_merge()')
    git2r::branch_create(name=branch)
  }

  # Create the branch if it doesn't exist yet
  git2r::checkout(branch=branch, create=TRUE)
}


#' Merge Branch into Current Branch
#'
#' Merge a different branch into the current branch. The user is asked interactively
#' to delete the merged (and now expendable) branch, and any conflicts arising are
#' printed and the option given to open the files immediately for editing. Any
#' files which resulted in conflicts will be flagged by `git_add()` to remind
#' the user to manually confirm conflicts are resolved.
#'
#' A very helpful tool in RStudio is `Edit` -> `Find in Files...` which allows
#' you to search your entire repository for where the conflicts are. These are
#' identified by searching for the chevrons <<<
#'
#' Conflicts are the usual format for git and may also happen after `git_pull()`
#'
#' \preformatted{
#' <<<<<<< HEAD
#' code-from-the-branch-you-have-stayed-on
#' =======
#' code-from-the-branch-you-have-just-merged-and-deleted
#' >>>>>>> merged-and-deleted-branch-name
#' }
#'
#' @param branchname Name of other branch to merge into the current one, will be asked
#'          interactively if left as NULL.
#' @returns Invisible NULL
#' @export
git_merge = function(branchname = NULL){
  current_branch = git2r::repository_head()$name

  # Check no uncommitted files
  check_everything_committed()

  merge_branch = ask_generic(paste0('Branch to merge into ',current_branch,': '), answer=branchname)

  if(merge_branch==''){
    for(each_branch in git2r::branches()) print(each_branch)
  }

  if(merge_branch=='') merge_branch = ask_generic(paste0('Branch to merge into ',current_branch,': '))
  # And if it is STILL blank, then time to move on
  if(merge_branch=='') return(invisible())

  # check named branch exists
  branch_exists = merge_branch %in% names(git2r::branches())
  if(!branch_exists) stop(merge_branch,' is not a branch')

  cat('Merge',merge_branch,'into',current_branch,'\n')
  proceed = ask_proceed('Proceed? (Y/N) ')
  if(!proceed) return(invisible())

  delete_after = ask_proceed('Delete this branch after successful merge? (Y/N) ')

  message('Merging ',merge_branch,' into ',current_branch)
  # Try the first time to see if there are any conflicts
  merge_res = git2r::merge(x = '.', y = merge_branch, fail=TRUE)
  # TEST ME
  if(length(merge_res$conflicts)==0){
    message('Left over uncommitted changes are in this branch - run git_add(), git_Commit(), and try merge again')
    return(invisible())
  }
  if(merge_res$conflicts==TRUE){
    proceed = ask_proceed('This merge will result with conflicts to resolve manually. Continue anyway? (Y/N) ')
    if(proceed==FALSE) {
      message("Merge cancelled: use git_diff() to investigate ")
      return(invisible())}

    # Proceed with conflicting merge
    merge_res = git2r::merge(x = '.', y = merge_branch, fail=FALSE)
    current_status = unlist(git2r::status())
    conflicting = current_status[grepl('conflicted',names(current_status))]
    message('The following files have conflicts:')
    for(each_file in conflicting) message('\t',each_file)

    # In the future, an interactive helper will be made to address conflicts
    # for now find them yourself
  }

  # Now delete the branch if merge has succeeded to this point
  # git_history will contain the commits of BOTH branches so can rewind still
  if(delete_after==TRUE){
    message('Deleting branch ', merge_branch)
    git2r::branch_delete(branch=git2r::branches()[[merge_branch]])}

  if(merge_res$conflicts==TRUE){
    proceed = ask_proceed('Open the conflicting files and resolve <<<???>>> now? (Y/N) ')
    if(proceed==FALSE){
      cat('You must go through each file before you git_add()\n')
      return(invisible())}
    # If using RStudio, this will open as a tab -- much nicer! Otherwise will call some pop-up external editor
    for(each_file in conflicting)
      tryCatch(rstudioapi::navigateToFile(each_file), error=function(err) utils::file.edit(each_file) )
  }
  return(invisible())
}
