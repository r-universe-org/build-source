#' @export
annotated_error <- function(package, what = c("vignettes", "readme", "pdfmanual", "install")){
  what <- match.arg(what)
  errfile <- switch(what,
                    vignettes = 'stderr_build.log',
                    readme = 'stderr_readme.txt',
                    pdfmanual = 'stderr_manual.txt',
                    install = 'install.log')
  logs <- readLines(errfile)
  start <- which(grepl("ERROR|Error", logs))[1]
  end <- length(logs)
  if(!is.na(start)){
    logs <- logs[start:end]
  }
  logs <- if(what == 'install'){
    c("Failed to install:", logs)
  } else {
    c(paste("Installation OK but failed to build", what), logs)
  }
  if(what == 'vignettes'){
    logs <- c(logs, "If this is expected, consider precomputing your vignettes: https://ropensci.org/blog/2019/12/08/precompute-vignettes/")
  }
  cat(sprintf("::error file=%s::%s\n", package, paste(utils::tail(logs, 100), collapse = '%0A')))
  quit(status = 1)
}
