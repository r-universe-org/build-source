#' @export
annotated_error <- function(package, what = c("vignettes", "readme", "pdfmanual")){
  what <- match.arg(what)
  errfile <- switch(what,
                    vignettes = 'stderr_build.log',
                    readme = 'stderr_readme.txt',
                    pdfmanual = 'stderr_manual.txt')
  logs <- readLines(errfile)
  start <- which(grepl("ERROR", logs))[1]
  end <- length(logs)
  if(!is.na(start)){
    logs <- logs[start:end]
  }
  logs <- c(paste("Installation OK but failed to build", what), logs)
  if(what == 'vignettes'){
    logs <- c(logs, "If this is expected, consider precomputing your vignettes: https://ropensci.org/blog/2019/12/08/precompute-vignettes/")
  }
  cat(sprintf("::error file=%s::%s\n", package, paste(logs, collapse = '%0A')))
  quit(status = 1)
}
