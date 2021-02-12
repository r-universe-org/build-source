#' Build tools
#'
#' Get some extra info about packages.
#'
#' @export
#' @rdname buildtools
#' @param path root directory of package
find_logo <- function (path = ".") {
  # Match logic from pkgdown but return path relative package root.
  files <- c('logo.svg', 'man/figures/logo.svg', 'logo.png', 'man/figures/logo.png')
  candidates <- file.path(path, files)
  utils::head(files[file.exists(candidates)], 1)
}

#' @export
#' @rdname buildtools
#' @param pkg name of the installed package
vignettes_base64 <- function(path, pkg = basename(path)){
  df <- vignettes_info(path = path, pkg = pkg)
  if(is.data.frame(df)){
    base64_gzip(jsonlite::toJSON(df))
  }
}

vignettes_info <- function(path, pkg){
  repo <- gert::git_open(repo = path)
  vignettes <- as.data.frame(tools::getVignetteInfo(pkg))
  if(nrow(vignettes) > 0){
    df <- vignettes[c('File', 'PDF', 'Title')]
    names(df) <- c("source", "filename", "title")
    inputs <- file.path('vignettes', df$source)
    stats <- gert::git_stat_files(inputs, repo = repo)
    df$created = stats$created
    df$modified = stats$modified
    df$commits = stats$commits
    return(df)
  }
}

base64_gzip <- function(bin){
  buf <- memCompress(bin, 'gzip')
  gsub("\n", "", jsonlite::base64_enc(buf), fixed = TRUE)
}
