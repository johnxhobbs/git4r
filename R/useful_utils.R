
#' Copy and Paste to Excel
#'
#' This reads a table from a (enlarged) clipboard or writes as tab-separated
#' which behaves nicely in Excel
#'
#' Pasting from Excel breaks if the text contains linebreaks within a cell.
#' To fix this, use Find&Replace "ctrl+J" within Excel
#'
#' If `data.table` is attached, xpaste() will return a data.table
#'
#' @param x A data.frame or any table which can be written with `write.table()`
#' @param header Data includes headers -- the only reason to omit when pasting is
#'            concatenating multiple chunks below each other
#'
#' @export
xcopy = function(x, header=TRUE)
  utils::write.table(x, 'clipboard-10240', sep='\t', row.names=FALSE, col.names=header)

#' @rdname xcopy
#' @export
xpaste = function(header=TRUE){
  res = utils::read.table('clipboard', sep='\t', header=header, quote='', comment.char='')
  if('data.table' %in% .packages()) res = data.table::setDT(res)
  return(res)
}

#' Clean File Path
#'
#' Converts backslashes to forward and removes and double-slash except a possible
#' double-slash at the start for a UNC path. Paste in the dirty path interactively
#' to allow single-backslash-paths to be pasted in without issues
#'
#' @returns Atomic character vector giving the cleaned path string
#' @export
fix_path = function(){
  path = readline('Path = ')
  path = gsub('\\\\','/',path)
  path = gsub('(?<!^)//+','/',path,perl=TRUE)
  return(path)
}


#' Edit File In Tempdir()
#'
#' `git_diff()` saves copies of each file opened in `tempdir()` which can be
#' opened for editing by this helper.
#'
#' @param filename Character vector of filenames which exist in `tempdir()` to
#'           open for editing. Parses with `basename()` so no subdirectories allowed.
#' @export
edit_tempfile = function(filename){
  # Code borrowed from git_merge()
  for(each_file in paste0(tempdir(),'/',basename(filename)))
    tryCatch(rstudioapi::navigateToFile(each_file), error=function(err) utils::file.edit(each_file) )
  return(invisible())
}
