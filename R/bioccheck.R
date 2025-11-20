#' @export
bioc_check <- function(sourcepkg, install = FALSE){
  options(BiocManager.check_repositories = FALSE)
  if(Sys.getenv("UNIVERSE_NAME") == 'bioc-release'){
    Sys.setenv(R_BIOC_VERSION = as.character(BiocManager:::.version_bioc("release")))
  } else {
    Sys.setenv(R_BIOC_VERSION = as.character(BiocManager:::.version_bioc("devel")))
  }

  library(BiocCheck)
  BiocCheck(sourcepkg, install = install, 'no-check-R-ver' = TRUE)
}
