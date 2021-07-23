#' Build tools
#'
#' Get some extra info about packages.
#'
#' @export find_logo package_sysdeps_string
#' @aliases find_logo package_sysdeps_string
#' @rdname buildtools
#' @param path root directory of package
#' @param git_url of the git repository
#' @param subdir if the package lives in a subdir in the repo
find_logo <- function (path, git_url, subdir = "") {
  # Match logic from pkgdown but return path relative package root.
  files <- c('logo.svg', 'man/figures/logo.svg', 'logo.png', 'man/figures/logo.png')
  cardimage <- find_opengraph_image(path = path)
  if(length(cardimage)){
    if(grepl('^http', cardimage)){
      return(cardimage)
    }
    files <- c(cardimage, files)
  }
  candidates <- file.path(path, files)
  logo <- utils::head(files[file.exists(candidates)], 1)
  if(!length(logo))
    return(NULL)
  file_to_url(logo, git_url, subdir)
}

file_to_url <- function(logo, git_url, subdir){
  git_url <- sub("\\.git$", "", git_url)
  upstream <- paste0(git_url, '/raw/HEAD')
  if(length(subdir) && nchar(subdir)){
    upstream <- paste0(upstream, '/', subdir)
  }
  logo_url <- paste0(upstream, '/', logo)
  if(url_exists(logo_url)){
    return(logo_url)
  } else{
    message("NOTE: Did not find upstream logo URL on GitHub: ", logo_url)
    return(logo)
  }
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
    df$author = vignette_author(inputs, repo = repo)
    df$created = stats$created
    df$modified = stats$modified
    df$commits = stats$commits
    return(df)
  }
}

normalize_author <- function(x){
  if(!length(x)){
    return(NA_character_)
  }
  if(is.list(x) && length(x$name)){
    return(as.character(x$name))
  }
  # Author can be a list of named lists, e.g. BiocManager
  if(is.list(x)){
    x <- unlist(lapply(x, normalize_author))
  }
  paste(as.character(x), collapse = ', ')
}

vignette_author <- function(inputs, repo = repo){
  path <- gert::git_info(repo)$path
  files <- file.path(path, inputs)
  vapply(files, function(x){
    tryCatch({
      normalize_author(rmarkdown::yaml_front_matter(x)$author)
    }, error = function(e){
      NA_character_
    })
  }, character(1), USE.NAMES = FALSE)
}

base64_gzip <- function(bin){
  buf <- memCompress(bin, 'gzip')
  gsub("\n", "", jsonlite::base64_enc(buf), fixed = TRUE)
}

url_exists <- function(url){
  req <- curl::curl_fetch_memory(url)
  return(req$status < 400)
}

#' @importFrom maketools package_sysdeps_string
package_sysdeps_string <- function(pkg){
  str <- maketools::package_sysdeps_string(pkg = pkg)
  if(!file.exists('/NEED_RJAVA'))
    return(str)
  java <- maketools::package_sysdeps_string('rJava')
  if(!nchar(str))
    return(java)
  paste(str, java, sep = ', ')
}

#' @rdname buildtools
#' @export
install_dependencies <- function(path = '.'){
  setwd(path)
  deps <- remotes::local_package_deps(dependencies=TRUE)
  utils::install.packages(deps)
  remotes <- as.data.frame(read.dcf('DESCRIPTION'))$Remotes

  # The following should not be needed if the remote is part of the universe
  # However we install it anyway to avoid race conditions if the remote was just added
  if(length(remotes)){
    try({
      remotes_repos <- trimws(strsplit(remotes, ',')[[1]])
      lapply(remotes_repos, function(x){remotes::install_github(x, upgrade = FALSE)})
    })
  }
  if(grepl('ggseg', Sys.getenv('MY_UNIVERSE'))){
    utils::install.packages("ggplot2", repos = 'https://ggseg.r-universe.dev')
  }

  # Check if rJava is a (recursive) dependency
  harddeps <- remotes::local_package_deps()
  if(length(harddeps)){
    alldeps <- sort(unique(c(harddeps, unlist(unname(tools::package_dependencies(harddeps, recursive = TRUE))))))
    #cat("Hard dependencies:", paste(alldeps, collapse=', '), '\n', file = stderr())
    if(isTRUE('rJava' %in% alldeps)){
      cat('::set-output name=NEED_RJAVA::true\n')
      file.create('/NEED_RJAVA')
    }
  }
}
