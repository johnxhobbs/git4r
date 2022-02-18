
#' Git Push
#'
#' Pushes just current branch to 'origin' by default, but allows selective
#' pushing of branches to different remotes if this is declined, for example
#' if you want to keep a test branch private.
#'
#' It is enforced that when pushing to 'origin' the branch is set to track this
#' remote branch. If the branch does not exist yet in the remote, it is not
#' pushed by default and must be manually added.
#'
#' Credentials are sorted in the same way as git_pull() and git_push()
#' using the best-matching `GIT_PAT*` environmental variable to the remote's
#' address, for example `GIT_PAT_AZURE=` in `~/.Renviron`
#'
#' @seealso git_pull, git_remote
#'
#' @param do_default Confirm that the default push-all-tracking-branches-to-origin ("Y" or TRUE)
#' @param remote_index If this is not true, selection of remote to push to (integer vector)
#' @param branch_index The selection of branches to push (integer vector)
#' @returns Invisible NULL
#' @export
git_push = function(do_default = NULL, remote_index = NULL, branch_index = NULL){

  # Get branch info
  local_branches = names(git2r::branches(flags='local'))
  remote_branches = names(git2r::branches(flags='remote'))
  available_remotes = git2r::remotes()
  available_remote_urls = git2r::remote_url()

  if(length(available_remotes)==0){
    message('No remotes configured')
    return(invisible())
  }

  current_branch = git2r::repository_head()$name

  # Shortcut - just push current branch to origin
  if('origin' %in% available_remotes){
    if(paste0('origin/',current_branch) %in% remote_branches){
      if(ask_proceed(paste('Push',current_branch,'to origin? (Y/N) '), answer=do_default)){
        # Otherwise if you don't proceed, more interactive options are coming
        git2r::push(name='origin', refspec=paste0('refs/heads/',current_branch), set_upstream=TRUE, credentials=choose_credential(remote='origin'))
        message('Done')
        return(invisible())
      }
    } else message('Current branch is not yet pushed to origin -- do this manually')
  } else message('No \'origin\' remote')

  # Otherwise more interactive questions

  cat('Available remotes\n')
  cat(paste(sprintf("%-3d",seq_along(available_remotes)),
            available_remotes, available_remote_urls, sep='  '), sep='\n')

  # If there is no choice (normal!) then answer this automatically
  if(length(available_remotes)==1)
    remote_index = 1

  remote = available_remotes[ask_which('Index number of remote to push to: ', answer=remote_index)]
  if(length(remote)!=1) stop('Must give exactly one remote. If none exist, use git_remote()')

  # Only causes error on the first ever commit-push
  if(length(git2r::commits(n=1))==0)
    stop('Must git_add() then git_commit() first')

  # Get only the remote branches in chosen remote
  remote_branches = remote_branches[remote_branches %in% paste0(remote,'/',local_branches)]
  # Now strip the remote branch name to be just a name
  remote_branches = sub(paste0(remote,'/'),'',remote_branches)

  already_pushed = local_branches %in% remote_branches

  message()
  cat('Local branches to push (+ if not in remote, > current branch) \n')
  cat(paste(sprintf("%-3d",seq_along(local_branches)),
            ifelse(already_pushed,' ','+'),
            ifelse(local_branches==current_branch,'>',' '),
            local_branches, sep='  '), sep='\n')

  add_these = ask_which('Which branch numbers to push? (Hit ENTER to add all except + else ESCAPE) ', answer=branch_index)
  if(length(add_these)==0) add_these = already_pushed

  if(!current_branch %in% local_branches[add_these]){
    message('Current branch not included - will not push anything')
    return(invisible())
  }

  # TODO work out why this error sometimes comes up?
  # 'git2r_branch_set_upstream': cannot set upstream for branch
  # if(!current_branch %in% local_branches[add_these])
  #   stop('Current branch must always be included in the push')

  #if(!ask_proceed(paste0('Push ',length(local_branches[add_these]),' branch(es) to ',remote,'? (Y/N) '))) stop('No branches pushed')
  if(length(local_branches[add_these])==0) stop('No branches selected')
  for(push_each_branch in local_branches[add_these] )
    git2r::push(name=remote, refspec=paste0('refs/heads/',push_each_branch), set_upstream=(remote=='origin'), credentials=choose_credential(remote=remote))
  #TODO this works when setting set_upstream=TRUE but not like this now....

  message('Done')
  return(invisible())
}

#' Git Pull
#'
#' This will pull any updates from the remote which each branch is tracking.
#' This is set using git_remote() to be the 'origin' remote, usually at
#' GIT_DEFAULT_REMOTE. This should be run before any new commits.
#'
#' If git_push() has not been run yet, it will complain that branches are not
#' set to track anything. If so, run git_remote() and git_push() to synchronise.
#'
#' Credentials are sorted in the same way as git_pull() and git_push()
#' using the best-matching `GIT_PAT`* environmental variable to the remote's
#' address, for example `GIT_PAT_AZURE=` in `~/.Renviron`
#'
#' @seealso git_push, git_remote
#' @returns Invisible NULL
#' @export
git_pull = function(){
  if(is.null(git2r::discover_repository()))
    stop('Working directory is not in a git repo, maybe you want git_clone(), or set up with git_remote() ')

  # Check has upstream branches, else skip
  # upstream_tracking = unlist(lapply(git2r::branches(flags='local'), function(brnch) !is.null(git2r::branch_get_upstream(brnch)) ) )
  currentbranch = git2r::repository_head('.')
  if(is.null(currentbranch)){
    message('Skipping pull: no active branch found')
    return(invisible())
  }
  upstream_of_current_branch = git2r::branch_get_upstream(branch=currentbranch)
  if(is.null(upstream_of_current_branch)){
    message('Skipping pull: active branch has not been pushed to origin remote')
    return(invisible())
  }

  pull_results = git2r::pull(credentials=choose_credential(remote=git2r::remote_url(remote='origin')[[1]]))
  cat('Pull result: ')
  print(pull_results)
  return(invisible())
}

#' Git Clone from Default Remote
#'
#' Running without any repo_name will print a list of valid repos found at the
#' GIT_DEFAULT_REMOTE path, and can then be re-run with one of these names to
#' clone a local version. Otherwise a url or complete filepath can be used.
#'
#' To change where the default remote path is use
#' - `Sys.setenv(GIT_DEFAULT_REMOTE='P:/ath/to/teams/remote_repo/folder')`
#'
#' @param repo_name Name of repo to clone, leave blank to display a list of available
#'             repo names. Also accepted is the url or complete filepath to a
#'             repo to clone (identified if containing '/').
#' @param to Local path to create local project folder in, defaults to My Documents
#' @returns Invisible NULL
#' @export
git_clone = function(repo_name='', to='~'){

  # Possibly move this into arguments to allow cloning from other places (eg cloud)
  path=Sys.getenv('GIT_DEFAULT_REMOTE')

  # If no DEAFULT_REPO on a shared drive is found, or repo_name has a / (absolute path or url)
  if(path=='' | grepl('/',repo_name)){
    from = repo_name
    message('Is URL')
  } else {
    # List available repo if not given by name
    try_dirs = list.dirs(path = path, recursive = FALSE)
    available_repos = lapply(try_dirs, git2r::discover_repository )
    available_repos = available_repos[!unlist(lapply(available_repos, is.null))]

    # TODO print the most recent commit message & datetime
    # last_commit = lapply(available_repos, function(repo) unlist(git2r::commits(repo=repo, n=1), recursive=FALSE) )
    # last_commit = lapply(available_repos, git2r::commits, n=1)
    # unlist(last_commit, recursive=FALSE)
    available_names = unlist(lapply(unlist(available_repos), basename))

    if(repo_name==''){
      # List them
      cat('Available repo names: \n')
      cat(available_names, sep='\n')
      return(invisible())
    }
    if(!repo_name %in% available_names){
      stop(repo_name,' not found!')
    }

    from = paste0(path,'/',repo_name)
  }
  to = paste0(to,'/',basename(repo_name))
  cat('Cloning ',repo_name,' into "',to,'"')
  ask_proceed(' -- proceed? (Y/N) ')
  # Clean up 'to' so it's a pure / path
  to = gsub('/+|\\\\+','/',to)
  # Annoying bug - git2r::clone() insists you are already in a git directory
  # This makes no sense if you're downloading a new repo
  # Annoying fix: make a temp repo, set as wd, and then setwd to new folder
  if(dir.exists(to)) stop(to,' already exists.\nDelete this directory or change the target parent directory "to=" argument')
  message('Cloning to ',to)
  currentwd = getwd()
  on.exit(expr=setwd(currentwd))  # Safety backup - if error return to current wd
  deleteme = tempfile()      # Will create a random temp-folder which can be left floating
  dir.create(path=deleteme)
  git2r::init(path=deleteme)
  setwd(deleteme)
  # BUG FIXED  must have progress=FALSE if inside a function
  git2r::clone(from, path.expand(to), progress=FALSE, credentials=choose_credential(from))
  setwd(currentwd)
  on.exit() # Clear the error handling to allow wd to be changed if wanted now
  if(ask_proceed('Set working directory to here? (Y/N) ')){
    setwd(to)
  }

  return(invisible())
}


# TODO allow this to look at http if no proxy
git_display_remotes = function(url='.', prefix=''){
  if(length(url)==0) return()
  if(!dir.exists(url)) return()

  subnames =  git2r::remotes(url)
  suburl = git2r::remote_url(url)

  #tryCatch({suburl = git2r::remote_url(remurl)}, error = function(err) {return(err)})
  prefix = paste0('-- ',prefix)
  for(i in seq_along(suburl)){
    cat(prefix, subnames[i],'=\'',suburl[i],'\'\n', sep='')
    git_display_remotes(suburl, prefix=prefix)
  }
  return(invisible())
}


#' Git Edit Remotes
#'
#' View and interactively add or edit remotes for this repo or another. When
#' adding a local shared-drive remote, it will create a bare git repo in the
#' GIT_DEFAULT_REMOTE directory with the name of the current project.
#'
#' Within git4r branches will always track the 'origin' remote.
#'
#' Across your team, create a folder that you all have read/write permissions
#' to and nominate it as the location for all remotes using:
#' `Sys.setenv(GIT_DEFAULT_REMOTE='P:/ath/to/teams/remote_repo/folder')`
#' or better still, put this value in your settings with `file.edit('~/.Renviron')`
#'
#' It is expected that a shared-drive folder 'origin' is used for all collaboration /
#' projects, which in turn may have its own 'origin' pointing to a cloud service.
#' This 'hybrid' is best achieved by using fully-fledged git installation set up
#' to synchronise as a scheduled task or somehow triggered. The shared-drive
#' remote repo can be overwritten with:
#'  - `git clone --mirror <url>`
#'
#' And then these commands run regularly will keep both in sync. If a branch gets
#' divergent commits between cloud and shared-drive, the latter will take precedence.
#' This has not been thoroughly tested and goes beyond the limitations of this
#' package!
#'  - `git push`
#'  - `git remote update --prune`
#'
#' @param repo_path Path of repo to modify parents of, "" or "." for default current repo
#' @param remote_index Index of remote to edit / remove? (Integer vector or string)
#' @param remote_name Name of remote to add (string of format "name='/path/or/url'", or "" for `GIT_DEFAULT_REMOTE`)
#' @returns Invisible NULL
#' @export
git_remote = function(repo_path = NULL, remote_index = NULL, remote_name = NULL){
  # git_display_remotes()

  repo = ask_generic('Path to repo to modify remotes? (Hit ENTER for current repo) ', answer=repo_path)
  if(repo=='') repo='.'

  cat('Listing remotes for the remote / repo "',repo,'"\n', sep='')
  # lookatrepo = c(remote_urls,remote_urls)[which(c(remote_names,remote_urls)==repo)[1] ]
  if(!dir.exists(repo)) stop('Repo path "',repo,'" does not exist')

  remote_names = git2r::remotes(repo=repo)
  remote_urls = git2r::remote_url(repo=repo)

  if(length(remote_names)==0) message('No remote yet!')
  else{
    cat('Existing remotes: \n')
    if(length(remote_names)==0) message('None!')
    else {
      cat(paste0(sprintf("%-3d",seq_along(remote_names)),remote_names,"='",remote_urls,"'"), sep='\n')
      remove_remotes = ask_which('Remote number to edit / remove? ', answer=remote_index)
      if(length(remove_remotes)==1){
        message('Removing ',remote_names[remove_remotes])
        git2r::remote_remove(repo = repo, name = remote_names[remove_remotes])}
    }
  }

  add_remote = ask_generic("Add remote? Specify with name='/path/or/url' or hit ENTER to add default origin or ESCAPE ", remote_name)

  if(add_remote==''){
    if(repo!='.') stop('Cannot automatically add default remote within GIT_DEFAULT_REMOTE unless repo is current directory ')

    if(check_has_default_remote()==TRUE){
      cat('Already has origin=\'',remote_urls[remote_names=='origin'],'\'',sep='')
      return(invisible())
    }

    repo_name = basename(dirname(git2r::discover_repository()))
    add_name = 'origin'
    add_url = paste0(Sys.getenv('GIT_DEFAULT_REMOTE'),'/',repo_name)
    message('Now run git_push() to complete setup of remote branches')
  }
  else{
    add_remote = gsub('[\'"]','',add_remote)
    add_name = strsplit(add_remote,'=')[[1]][1]
    add_url = strsplit(add_remote,'=')[[1]][2]
  }

  # If local path, will need to make the dir and init
  if(!grepl('^http',add_url)){
    check_and_create_valid_repo(target_path=add_url)
  }

  message('Adding ',add_name,' at ',add_url)
  git2r::remote_add(name=add_name, url=add_url)
  return(invisible())
}
