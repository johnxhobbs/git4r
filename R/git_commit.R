

#' Auto Git
#'
#' Call the git pull-add-commit-push cycle interactively
#'
#' @seealso git_remote, git_add, git_commit, git_push, git_branch, git_diff
#'
#' @export
git = function(){
  check_is_repo() # This is otherwise called by git_add()
  git_pull()
  git_add()
  git_commit()
  git_push(do_default=TRUE)
  return(invisible())
}


#' Git Add
#'
#' Prints all changed files since last commit and waits for you to hit
#' ENTER to add everything, or type out the space-separated numbers of
#' what you do want to add to the next commit, else ESCAPE to cancel. It
#' is highly recommended that you call without any arguments - `git_add()`
#'
#' Files can be specified by number by typing a list of space-separated
#' values. The inverse can also be used, for example "-2 -4" would add
#' (or remove)
#'
#' Each file or directory is given a symbol for change type:
#' - `-` has been deleted
#' - `+` has been created
#' - `*` has been changed since git_add() was last called and should be re-added
#' - `@` has been renamed, but this will often show as a pair of `+` and `-`
#' - `?` contains conflict from latest merge; go in and edit by hand, searching
#'       for "<<<<<<< HEAD" upto ">>>>>>> (some branch)"
#'
#' Arguments can be given which are the character or integer answer to the
#' interactive questions asked. It is highly recommended that you do not rely
#' on passing arguments because it is operating blind.
#'
#' @param remove_index Integer vector or character of file numbers to un-add?
#'              (Use "" to keep all)
#' @param add_index Integer vector or character of file numbers to add?
#'              (Use "" to add all non-conflicting)
#'
#' @export
git_add = function(remove_index = NULL, add_index = NULL){

  check_is_repo() # This will throw error if not

  # Go through each of the file groups you might want to add

  # TODO use names(git2r::status()) to separate files into groups:
  # staged/unstaged: create/delete/modified. Maybe +/-/*  in this order?

  # Already added
  added_already = sort(unlist(git2r::status(staged = TRUE, unstaged = FALSE, untracked = FALSE)))
  not_yet_added = sort(unlist(git2r::status(staged = FALSE, unstaged = TRUE, untracked = TRUE)))

  if(length(added_already) > 0){
    # Tell user what is already tracked, give the option to clear the cache

    # All possible options!
    # status: ignored, untracked, tracked, staged, unstaged
    # change: none, new, deleted, modified, renamed

    added_symbol = rep(' ', length(added_already))
    added_symbol[grepl('deleted',names(added_already))] = '-'
    added_symbol[grepl('new',names(added_already))] = '+'
    added_symbol[grepl('renamed',names(added_already))] = '@'
    added_symbol[grepl('conflicted',names(added_already))] = '?'
    added_symbol[added_already %in% not_yet_added] = '*'

    cat('Changes staged already \n')
    cat(paste(sprintf("%-3d",seq_along(added_already)),
              added_symbol,
              added_already, sep='  '), sep='\n')

    unstage_these = ask_which('Any file numbers to un-add? (Hit ENTER to keep all) ', answer=remove_index)

    if(length(unstage_these) > 0){
      # Cannot unstage things when there has not been a first commit
      if(length(git2r::commits(n=1))==0)
        stop('Cannot unstage added files if repository has zero commits')
      for(unstage_each_file in added_already[unstage_these] )
        git2r::reset(object = '.', path = unstage_each_file)
    }
  }

  # Update which files have or have not been added, just in case you have unstaged some
  added_already = sort(unlist(git2r::status(staged=TRUE, unstaged=FALSE, untracked=FALSE)))
  not_yet_added = sort(unlist(git2r::status(staged=FALSE, unstaged=TRUE, untracked=TRUE)))

  if(length(not_yet_added)==0){
    message('No modified files to add!')
    return(invisible())
  }

  # Tell user which are allowed to be added

  notyet_symbol = rep(' ', length(not_yet_added))
  notyet_symbol[grepl('deleted',names(not_yet_added))] = '-'
  notyet_symbol[grepl('untracked.untracked',names(not_yet_added))] = '+'
  notyet_symbol[grepl('renamed',names(not_yet_added))] = '@'
  notyet_symbol[grepl('conflicted',names(not_yet_added))] = '?'
  notyet_symbol[not_yet_added %in% added_already] = '*'

  cat('Modified files to be added (+ created, - deleted, * changed since added, ? conflict) \n')
  cat(paste(sprintf("%-3d",seq_along(not_yet_added)),
            notyet_symbol,
            not_yet_added, sep='  '), sep='\n')


  add_these = ask_which('Which file numbers to add? (Hit ENTER to add all non-conflicting, else ESCAPE) ', answer=add_index)
  add_these = add_these[!is.na(add_these)]
  if(length(add_these) > 0){
    message('Adding ',length(not_yet_added[add_these]),' file(s)')
    for(add_each_file in not_yet_added[add_these] )
      git2r::add(path = add_each_file)
  }
  else{
    # Weirdly MUST have a message() statement to allow ESC key to crash out, rather than be read as a "" string
    conflicting = grepl('conflicted',names(not_yet_added))
    message('Adding ',length(not_yet_added)-sum(conflicting),' file(s)')
    if(sum(conflicting)>0){
      for(add_each_file in not_yet_added[!conflicting] )
        git2r::add(path = add_each_file)
      message('Conflicting files have NOT been automatically added. When manually resolved / fixed, add these by number ')
    }
    else{
      git2r::add(path = '.')
    }
  }

  return(invisible())
}

#' Git Commit
#'
#' Run git_add() immediately before to see what files are being committed. It is
#' highly recommended that you run in interactive mode using no arguments
#' (`git_commit()`)
#'
#' @param message Commit message, usually one sentence about a specific change,
#'                character atomic.
#' @param proceed Confirmation before proceeding, logical atomic with option for
#'                any character string starting 'Y' to be interpreted as `TRUE`
#' @export
git_commit = function(message = NULL, proceed = NULL){

  check_is_repo() # This will throw error if not

  # Optionally run git_add() to show you what you are commiting
  message = ask_generic('Commit message: ', answer=message)
  if(message==''){
    stop('Must give a message! Cannot amend previous commit (see issue #213 for git2r)')
  }

  check_unresolved_conflicts()

  current_branch = git2r::repository_head()$name
  if(length(current_branch)==0) current_branch = 'master'

  ask_proceed(paste0('Commit to ',current_branch,'? (hit ESCAPE to cancel) '), answer=proceed)

  git2r::commit(message=message)
  message('Done')

  return(invisible())
}
