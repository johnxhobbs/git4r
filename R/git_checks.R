
# Internal functions for testing or checking whether an action is valid / helpers

# Interactive ask a (Y/N) question and return TRUE else FALSE
# Need an empty message('') so that ESCAPE really does cancel
# Remember to have a trailing space in prompt
# This COULD be replaced with utils::askYesNo()
# except blank line defaults to Yes which is not conservative
ask_proceed = function(prompt, answer=NULL){
  # Return the prespecifed answer
  if(!is.null(answer)){
    cat(prompt)
    message(answer)
    if(is.logical(answer)) return(isTRUE(answer))
    answer = substr(toupper(answer),1,1)=='Y'
  }
  else{
    answer = substr(toupper(readline(prompt = prompt)),1,1)=='Y'
    # Required break-point if user presses escape as their 'answer'
    message('')
  }
  return(answer)
}
# TODO add option for regex pattern, return this as string to allow partial matches
ask_which = function(prompt, answer=NULL){
  # Return the prespecifed answer
  if(!is.null(answer)){
    cat(prompt)
    message(answer)
    if(is.integer(answer)) return(answer)
    answer = as.integer(strsplit(as.character(answer), split = '[^0-9\\-]')[[1]])
  }
  else{
    answer = as.integer(strsplit(readline(prompt = prompt), split = '[^0-9\\-]')[[1]])
    message('')
  }
  return(answer)
}
ask_generic = function(prompt, answer=NULL){
  if(!is.null(answer)){
    cat(prompt)
    message(answer)
  }
  else{
    answer = readline(prompt = prompt)
    message('')
  }
  return(answer)
}

# Try and get the best PAT token from .Renviron (or other system env)
# based on the remote -- allows you to have a different PAT for github
# to devops -- by matching the string
# ALWAYS must start with GIT_PAT
# Give the name or url of remote
choose_credential = function(remote=''){
  # Has a URL or remote name been given?
  remote_names = git2r::remotes()
  remote_urls = git2r::remote_url()

  # Swap any known remote name (such as 'origin') with its url / path
  if(remote %in% remote_names) remote = remote_urls[which(remote_names==remote)]

  # Return null credentials if it is a shared drive
  is_http = grepl('^http',remote)
  if(!is_http) return(git2r::cred_user_pass('',''))

  all_env_var = names(Sys.getenv())
  all_env_var = all_env_var[grepl('^GIT_PAT',all_env_var)]
  choose_best = utils::adist(toupper(remote), toupper(all_env_var), costs = c(i=1,d=0,s=1))
  choose_best = all_env_var[which.min(choose_best)]
  #message('Using token: ',choose_best)
  return(git2r::cred_token(token=choose_best))
}

# Check If Git Repo
# Is the current working directory inside a git repo? If not ask if it should
# be made into one, using git-central as a remote.
check_is_repo = function(){
  if(is.null(git2r::discover_repository())){
    cat(basename(getwd()),'directory is not inside a git repository - check this is the top level else ESCAPE and setwd()')
    proceed = ask_proceed(prompt = 'Should this directory be turned into a git repo? (Y/N) ')
    if(proceed==FALSE) # This will crash out any parent task (eg 'git_add')
      stop('This is not a git repository')

    # If YES carry on and make our repo
    git2r::init()
    if(!file.exists('.gitignore')){
      message('Copying default .gitignore to working directory')
      file.copy(from=system.file('templates/default_gitignore', package='git4r'), to='.gitignore')
    }
    message(basename(getwd()),' is now a git repo \n')
  }
  return(TRUE)
}


# Checks if Usable for Remote
#
# Does various checks and returns TRUE having found or created a valid remote
# repo at target_path, else gives an error.
# Default is to compare the request new remote to current working dir but can
# specify using here='/another/repo/path'
check_and_create_valid_repo = function(target_path, here='.',bare=TRUE){
  if(dir.exists(target_path)){
    if(length(list.files(target_path,all.files=TRUE))==0){
      # Empty, existing folder
      message('Converting empty directory into git repo')
      git2r::init(path=target_path, bare=bare)
      return(TRUE)
    }
    if(!is.null(git2r::discover_repository(path=target_path))){
      # Is some kind of git repo
      if(!git2r::is_bare(target_path) & bare==TRUE){
        # Bare repo already here!
        stop('This is a working directory, not a bare git repo. Call again with bare=FALSE to proceed')
      }
      else{
        tryCatch({remote_latest_commit = git2r::last_commit(target_path)$sha},
                 error=function(err) {
                   message('No commits made yet: allowing add')
                   return(TRUE)})
        # Check if this repo is empty
        if(length(remote_latest_commit)==0){
          message('Found an empty bare repo which has zero commits so can be requisitioned')
          return(TRUE)
        }
        # Check if same project (local ahead). NB if someone else has committed to it since
        # _could_ allow add and pull, but do this yourself with git_clone and fix your mess
        local_history = unlist(lapply(git2r::commits(repo=here), function(x) x$sha))

        if(remote_latest_commit %in% local_history){
          message('This remote shares the same git history: allowing add')
          return(TRUE)
        }

        stop(target_path,' already a git repo! Choose a different project dir name, or use git_clone() to merge')}
    }
    stop(target_path,' not empty, nor git repo')
  }
  else{
    # Directory does NOT exist yet
    #ask_proceed('Create directory as git remote repo? (Y/N) ')
    message('Creating', if(bare) ' bare' ,' repo at ',target_path)
    dir.create(target_path, recursive=TRUE)
    git2r::init(path=target_path, bare=bare)
  }
  return(TRUE)
}

check_unresolved_conflicts = function(){
  # Used by git_commit
  # Check if any unresolved conflicts: "Cannot create a tree from a not fully merged index."
  if(any(grepl('conflicted',names(unlist(git2r::status(unstaged=TRUE, staged=FALSE, untracked=FALSE))))))
    stop('Cannot commit when you have unresolved conflicts -- run git_add() and make sure every ? is added by number')
  return(TRUE)
}

check_everything_committed = function(warn = FALSE){
  not_yet_committed = unlist(git2r::status(staged=TRUE, unstaged=TRUE, untracked=TRUE))
  if(length(not_yet_committed)>0){
    if(warn==TRUE){
      message('
Not all changes have been committed! Run git_diff() to see what.

>>>>> Continuing will result in IRREVERSIBLE LOSS <<<<<
')
    } else stop('Not all changes have been committed to current branch -- do this first')
  } else return(TRUE)
}

# Confirm that a valid git user exists and print it
check_username = function(){

  # Load your user settings
  local_user = git2r::config()$local$user.name
  local_email = git2r::config()$local$user.email
  global_user = git2r::config()$global$user.name
  global_email = git2r::config()$global$user.email

  if(!is.null(local_user) & !is.null(local_email)){
    cat('Using local (repo-specific) identity: ',local_user,' <',local_email,'>', sep='')
    return(invisible(TRUE))
  } else if(!is.null(global_user) & !is.null(global_email)){
    cat('Using global (system default) identity: ',global_user,' <',global_email,'>', sep='')
    return(invisible(TRUE))
  } else {
    message('No config values for user.name and user.email could be found, these are required to commit')
    set_global = ask_proceed('Set up global config for this user? (Y, or N to use repo-specific, or ESCAPE to cancel) ')
    set_user = ask_generic('user.name: ')
    set_email = ask_generic('user.email: ')
    if(nchar(set_user)==0 | nchar(set_email)==0)
      stop('Cannot leave either user.name or user.email blank')
    git2r::config(global=set_global, user.name=set_user, user.email=set_email)
    message('Done')
    # Now confirm values being used
    cat('Using ',if(set_global) 'global' else 'local',' identity: ',global_user,' <',global_email,'>', sep='')
    return(invisible(TRUE))
  }
}
