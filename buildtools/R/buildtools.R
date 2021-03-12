#' Build tools
#'
#' Get some extra info about packages.
#'
#' @export find_logo package_sysdeps_string
#' @aliases find_logo package_sysdeps_string
#' @rdname buildtools
#' @param path root directory of package
find_logo <- function (path = ".") {
  cardimage <- find_opengraph_image(path = path)
  if(length(cardimage))
    return(cardimage)
  # Match logic from pkgdown but return path relative package root.
  files <- c('logo.svg', 'man/figures/logo.svg', 'logo.png', 'man/figures/logo.png')
  candidates <- file.path(path, files)
  utils::head(files[file.exists(candidates)], 1)
}

find_opengraph_image <- function(path = "."){
  tryCatch({
    yml <- load_pkgdown_yml(path = path)
    return(yml$template$opengraph$image$src)
  }, error = function(e){})
}

load_pkgdown_yml <- function(path = '.'){
  candidates <- file.path(path, c('_pkgdown.yml', 'pkgdown/_pkgdown.yml'))
  for(f in candidates){
    if(file.exists(f))
      return(yaml::read_yaml(f))
  }
}

#' @export
#' @rdname buildtools
#' @param repo path to the git repository
#' @param pkg name of the installed package
#' @param subdir path within the git repo where the pkg is
vignettes_base64 <- function(repo, pkg = basename(repo), subdir = ""){
  df <- vignettes_info(repo = repo, pkg = pkg, subdir = subdir)
  if(is.data.frame(df)){
    base64_gzip(jsonlite::toJSON(df))
  }
}

vignettes_info <- function(repo, pkg, subdir = ""){
  repo <- gert::git_open(repo = repo)
  vignettes <- as.data.frame(tools::getVignetteInfo(pkg))
  if(nrow(vignettes) > 0){
    df <- vignettes[c('File', 'PDF', 'Title')]
    names(df) <- c("source", "filename", "title")
    inputs <- file.path('vignettes', df$source)
    if(length(subdir) && nchar(subdir)){
      inputs <- file.path(subdir, inputs)
    }
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

#' @importFrom maketools package_sysdeps_string
package_sysdeps_string <- maketools::package_sysdeps_string
