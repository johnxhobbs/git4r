
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
#' Credentials are sorted in the same way for `git_pull()` and `git_push()`. If
#' `gitcreds` package is installed, this is used first to check whether the
#' system git installation already has a username / password for this host.
#' This can be changed or added using stand-alone git, or `gitcreds::gitcreds_set()`.
#'
#' Alternatively, if `gitcreds` is not installed, a system-git is not available,
#' or no existing git credentials are found, then the the environmental variables
#' are searched for a suitable Personal Access Token. The variable name must begin
#' `GIT_PAT` and any additional words are used to distinguish the PAT for the relevant
#' host using a closest-string match for the remote URL, for example
#' `GIT_PAT_AZURE=abc123def456` will be chosen to authenticate
#' an Azure DevOps remote above `GIT_PAT_GITHUB` or `GIT_PAT_GITLAB`.
#'
#' To set this up from scratch by creating a Personal Access Token and saving it
#' to your `.Renviron` file with the name "GIT_PAT****" where asterisks can be
#' replaced with part of the remote URL if you need to distinguish between several
#' different PATs (and a good reminder of what it is).
#'
#' @seealso git_pull, git_remote
#'
#' @param do_default Character or logical passed as the answer to the interactive question
#'          which is "push the current branch to origin". If 'Y' or TRUE, this function
#'          runs without user input (used by `git()`), the default value `NULL` will
#'          prompt the user to answer interactively.
#' @returns Invisible NULL
#' @export
git_push = function(do_default = NULL){

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

  remote = available_remotes[ask_which('Index number of remote to push to: ')]
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

  add_these = ask_which('Which branch numbers to push? (Hit ENTER to add all except + else ESCAPE) ')
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
#' This will pull any updates from the 'origin' remote for every branch.
#' This should be run before making a commit (as is done by wrapper `git()`) in
#' order to avoid conflicts arising if somebody else has made changes
#' on the same branch. If this happens, merge conflicts are raised in the usual
#' way and `git_add()` will show you what needs resolving.
#'
#' To setup an 'origin' remote, use `git_remote()`. Credentials are handled
#' in the same way as `git_push()`, see `?git_push` for details.
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
#' Clone another repository given as a URL of absolute filepath. Alternatively,
#' if `GIT_DEFAULT_REMOTE` environmental variable has been set
#' a list of repositories within this path is displayed and can be cloned by name.
#'
#' Running `repo_name=""` will print a list of valid repositories found at the
#' `GIT_DEFAULT_REMOTE` path, and can then be re-run with one of these names to
#' clone a local version. This is to make basic on-premises collaboration and backups
#' more effortless. `GIT_DEFAULT_REMOTE` can be set in .Renviron file as per `?git_remote`
#'
#' Other repositories can be cloned as expected by giving a URL. Credentials are handled
#' automatically in the same way as `?git_push`.
#'
#' @param repo_name Name of repo to clone, leave blank to display a list of available
#'             repo names. Also accepted is the url or complete filepath to a
#'             repo to clone (identified if containing '/').
#' @param to Local path to create local project folder in, defaults to home directory
#' @returns Invisible NULL
#' @export
git_clone = function(repo_name='', to='~/'){

  # Possibly move this into arguments to allow cloning from other places (eg cloud)
  path=Sys.getenv('GIT_DEFAULT_REMOTE')

  # If no DEAFULT_REPO on a shared drive is found, or repo_name has a "/" (absolute path or url)
  if(path=='' | grepl('/',repo_name)){
    from = repo_name
  } else {
    # Proceed with the method which uses a name within GIT_DEFAULT_REMOTE
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

  if(from=='') stop('No repo_name given, and no GIT_DEFAULT_REMOTE set to list available repos')

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


#' Git Modify Remotes
#'
#' View and interactively edit remotes for this repo. The most important of these
#' is 'origin' which will be used by `git_pull()`, however you may also want a
#' write-only mirror, for example where you push a production-ready commit. If
#' `GIT_DEFAULT_REMOTE` environmental value has been set, this will be suggested
#' for 'origin'.
#'
#' If using a filesystem path for the remote, the option is given to create a full
#' working tree (copy of all files) or bare repository. A bare repository is
#' recommended for 'origin' because no one should make any changes to this directly
#' (instead, clone their own copy). For pushing to a 'backup' or 'production' remote,
#' you will want to push a copy of the files themselves.
#'
#' If you have set `GIT_DEFAULT_REMOTE` environmental value this will be suggested
#' as the path for creating an 'origin' remote. This will make life easier because
#' it will be the same as the rest of your projects / team and will be searched
#' by `git_clone()`.
#' Use `Sys.setenv(GIT_DEFAULT_REMOTE='P:/ath/to/teams/remote_repo/folder')`
#' or better still, put this value in your settings with `file.edit('~/.Renviron')`
#'
#' It is possible to use an on-premises 'origin' remote which in turn synchronises
#' with another server.
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
#' @param remote_name Name of remote to add (such as 'origin') else leave NULL
#'                    and answer interactively
#' @param remote_path Path / URL of remote to add, empty string will use
#'                    `GIT_DEFAULT_REMOTE` env variable, else leave NULL and
#'                    answer interactively
#' @returns Invisible NULL
#' @export
git_remote = function(remote_name = NULL, remote_path = NULL){
  # git_display_remotes()

  default_remote = Sys.getenv('GIT_DEFAULT_REMOTE')

  check_is_repo()

  remote_names = git2r::remotes(repo='.')
  remote_urls = git2r::remote_url(repo='.')

  # List and Remove any remotes IF no arguments given
  if(is.null(remote_name)){
    if(length(remote_names)==0) {
      message('No remote yet!')
    } else {
      cat('Existing remotes: \n')
      if(length(remote_names)==0) message('None!')
      else {
        cat(paste0(sprintf("%-3d",seq_along(remote_names)),remote_names,"='",remote_urls,"'"), sep='\n')

        # If using 'GIT_DEFAULT_REMOTE' - warn if this repo has something different
        if(default_remote!=''){
          origin_path = remote_names[remote_urls=='origin']
          if(length(origin_path)==1){
            if(!grepl(default_remote,origin_path))
              message('Note: origin=',origin_path,' is not in GIT_DEFAULT_REMOTE=',default_remote)
          }
        }

        remove_remotes = ask_which('Remote number to edit / remove? ')
        if(length(remove_remotes)==1){
          message('Removing ',remote_names[remove_remotes])
          git2r::remote_remove(repo = '.', name = remote_names[remove_remotes])}
      }
    }
  }

  # Default behaviour is to add 'origin' if it doesn't already exist
  if(!'origin' %in% remote_names){
    add_name = ask_generic("Hit ENTER to add 'origin' remote, else type remote name, or ESCAPE to cancel: ", answer=remote_name)
    if(add_name=='') add_name = 'origin'
  } else {
    add_name = ask_generic("Type name of new remote, or ESCAPE to cancel: ", answer=remote_name)
    if(add_name==''){
      message('No new remote added')
      return(invisible())
    }
  }

  # Default behaviour is to use GIT_DEFAULT_REMOTE for 'origin' if it does exist
  if(default_remote!='' & add_name=='origin'){
    repo_name = basename(dirname(git2r::discover_repository()))
    default_url = paste0(Sys.getenv('GIT_DEFAULT_REMOTE'),'/',repo_name)
    add_url = ask_generic(paste0("Path / URL for '",add_name,"' (hit ENTER to use default ",default_url,"): "), answer=remote_path)
    if(add_url==''){
      add_url = default_url
    }
  } else {
    add_url = ask_generic(paste0("Path / URL for '",add_name,"': "), answer=remote_path)
    if(add_url==''){
      message('No new remote added')
      return(invisible())
    }
  }

  # Check whether any characters are illegal - maybe none?!
  #if(grepl('/',add_name)) stop('Cannot have "/" in remote name')

  # If local path, will need to make the dir and init
  if(!grepl('^http',add_url)){
    use_bare = ask_proceed("Use bare repository for remote (see ?git_remote: recommended for 'origin') (Y/N): ")
    check_and_create_valid_repo(target_path=add_url, bare=use_bare)
  }

  # Possibly allow GIT_DEFAULT_REMOTE to be a cloud path and have
  # check_and_create_valid_url(target_path=add_url)
  # which would call the various API for each major provider...
  # Nah, so much easier to do this in browser!

  message('Adding ',add_name,' at ',add_url)
  git2r::remote_add(name=add_name, url=add_url)

  if(add_name=='origin')
    message('Now run git_push() to complete setup of remote branch tracking')

  return(invisible())
}
