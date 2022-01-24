
#' Run On Load
#'
#' This initialises the user environment when the package is attached to make sure
#' that required git username / password is available (and notify which is being
#' used). It is highly anticipated that this will be used in interactive mode,
#' and may be reloaded several times in one session using `library(git4r)`
#'
#' Some of the environmental options can be set as .Renviron variables, such
#' as username, local remote directory, and personal access token secrets.
#' @param libname (as required)
#' @param pkgname (as required)
.onAttach = function(libname, pkgname){

  # Load your user settings
  local_user = git2r::config()$local$user.name
  local_email = git2r::config()$local$user.email
  global_user = git2r::config()$global$user.name
  global_email = git2r::config()$global$user.email

  if(!is.null(local_user) & !is.null(local_email)){
    packageStartupMessage('Using local (repo-specific) config: ',local_user,' <',local_email,'>')
  } else if(!is.null(global_user) & !is.null(global_email)){
    packageStartupMessage('Using global (system default) config: ',global_user,' <',global_email,'>')
  } else if(Sys.getenv('GIT_USER')!='' & Sys.getenv('GIT_EMAIL')!=''){
    packageStartupMessage('Initialising global git config values from environmental variables GIT_USER and GIT_EMAIL')
    packageStartupMessage('These can be changed any time with git2r::config()')
    git2r::config(global=TRUE, user.name=Sys.getenv('GIT_USER'), user.email=Sys.getenv('GIT_EMAIL'))
  } else {
    packageStartupMessage('
            No settings for git username and email could be found
            They would be automatically copied from GIT_USER and GIT_EMAIL environmental values
            Otherwise use "git config" or git2r::config()
            You will be unable to modify this repo.')
  }
  if(Sys.getenv('GIT_DEFAULT_REMOTE')!='')
    packageStartupMessage(' and default folder for remotes: ',Sys.getenv('GIT_DEFAULT_REMOTE'))
}
