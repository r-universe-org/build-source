#' Build tools
#'
#' Get some extra info about packages.
#'
#' @export find_logo
#' @aliases find_logo
#' @rdname buildtools
#' @param path root directory of package
#' @param git_url of the git repository
#' @param subdir if the package lives in a subdir in the repo
find_logo <- function (path, git_url, subdir = "") {
  # Match logic from pkgdown but return path relative package root.
  rel_path <- c('logo.svg', 'man/figures/logo.svg', 'logo.png', 'man/figures/logo.png')
  cardimage <- find_opengraph_image(path = path)
  if(length(cardimage)){
    if(grepl('^http', cardimage)){
      return(cardimage)
    }
    rel_path <- c(cardimage, rel_path)
  }
  abs_path <- file.path(path, rel_path)
  if(!any(file.exists(abs_path))){
    return(NULL)
  }
  # pkg 'ragg' has huge svg logo
  smallest <- which.min(file.info(abs_path)$size)
  logo_path <- rel_path[smallest]
  file_to_url(logo_path, git_url, subdir)
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

#' @export
#' @rdname buildtools
commit_info_base64 <- function(repo = '.'){
  info <- gert::git_commit_info(repo = repo)
  out <- info[c("id", "author", "committer", "message", "time")]
  out$message <- substring(out$message, 1, 2000) #Do not overflow http headers
  out$time <- unclass(out$time)
  json <- jsonlite::toJSON(out, auto_unbox = TRUE)
  base64_gzip(json)
}

# offline version of GitHub /stats/commit_activity
# results may be slightly different due to the start day of a week, and also
# the week 53 numbering.
# also gert assumes linear history and only counts commits in the main branch,
# so this metric is more like weekly 'updates' that would have been deployed.
weekly_commits <- function(repo = '.'){
  format_week <- function(x){
    paste0(lubridate::isoyear(x), '-', sprintf('%02d', lubridate::isoweek(x)))
  }
  now <- Sys.Date()
  start_date <- now - 375 # some extra days to prevent mid-week truncation
  commit_dates <- gert::git_log(after = start_date, repo = repo, max = 999999)$time
  commit_week <- format_week(commit_dates)
  levels <- sort(unique(format_week(now - 0:365)))
  counts <- table(factor(commit_week, levels = levels))
  #list(counts = as.integer(counts), range = range(levels))
  df <- data.frame(week=levels, n=as.integer(counts))
  subset(df, n > 0)
}

latest_tags <- function(repo = '.'){
  now <- Sys.Date()
  df <- gert::git_tag_list(repo = repo)
  df$date <- vapply(df$commit, function(ref){
    as.Date(gert::git_commit_info(ref, repo = repo)$time)
  }, now)
  class(df$date) <- class(now)
  df <- df[df$date > (now - 365), c('name', 'date')]
  df[order(df$date),]
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
    df$engine = vignette_engines(inputs, repo = repo)
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

vignette_engines <- function(inputs, repo = repo){
  path <- gert::git_info(repo)$path
  files <- file.path(path, inputs)
  vapply(files, function(x){
    tryCatch({
      tools:::getVignetteEngine(x)
    }, error = function(e){
      NA_character_
    })
  }, character(1), USE.NAMES = FALSE)
}

base64_gzip <- function(bin){
  buf <- memCompress(bin, 'gzip')
  b64 <- gsub("\n", "", jsonlite::base64_enc(buf), fixed = TRUE)
  chartr('+/', '-_', b64)
}

base64_gunzip <- function(b64){
  b64 <- chartr('-_', '+/', b64)
  bin <- jsonlite::base64_dec(b64)
  rawToChar(memDecompress(bin, 'gzip'))
}

url_exists <- function(url){
  req <- curl::curl_fetch_memory(url)
  return(req$status < 400)
}

sysdep_shortname <- function(x){
  switch(x$package,
    'libgfortran5' = 'fortran',
    'libstdc++6' = "c++",
    'libgomp1' = 'openmp',
    'openjdk-11-jre-headless' = 'java',
    'libpng16-16' = 'libpng',
    'libtiff5' = 'libtiff',
    sub('\\d+\\.\\d+$', "", x$source)
  )
}

#' @export
#' @rdname buildtools
#' @importFrom maketools package_sysdeps
sysdeps_base64 <- function(pkg){
  sysdeps <- maketools::package_sysdeps(pkg)
  if(file.exists('/NEED_RJAVA')){
    sysdeps <- rbind(sysdeps, maketools::package_sysdeps('rJava'))
  }
  if(file.exists('/NEED_JAGS')){
    sysdeps <- rbind(sysdeps, maketools::package_sysdeps('rjags'))
  }
  df <- sysdeps[!is.na(sysdeps$package), c('package', 'headers', 'source', 'version')]
  if(is.data.frame(df) && nrow(df) > 0){
    df$name = NA_character_
    df$homepage = NA_character_
    df$description = NA_character_
    for(i in seq_len(nrow(df))){
      df$name[i] = sysdep_shortname(df[i,])
      pkginfo <- apt_cache_info(df$package[i])
      if(length(pkginfo$homepage))
        df$homepage[i] = pkginfo$homepage[1]
      if(length(pkginfo$description))
        df$description[i] = pkginfo$description[1]
    }
    json <- jsonlite::toJSON(df, auto_unbox = TRUE)
    base64_gzip(json)
  }
}

apt_cache_info <- function(pkg){
  out <- sys::exec_internal('apt-cache', c('show', pkg))
  con <- rawConnection(out$stdout)
  on.exit(close(con))
  df <- as.data.frame(read.dcf(con))
  names(df) <- tolower(names(df))
  df
}

#' @rdname buildtools
#' @export
install_sysdeps <- function(path = '.'){
  setwd(path)
  if(grepl('java', basename(path), ignore.case = TRUE)){
    # rJava description file cannot be parsed before 'mkdist'
    system("apt-get install -y default-jdk")
  }

  # Try to install missing sysdeps.
  # This only installs the first match; system_requirements may return many recursive sysdeps.
  # But most sysdeps are preinstalled for us anyway

  # Temp workaround for: https://github.com/r-lib/remotes/pull/705
  #ubuntu <- gsub(" ", "-", tolower(substring(utils::osVersion,1,12)))
  ubuntu <- 'ubuntu-20.04'
  tryCatch({
    aptline <- remotes::system_requirements(ubuntu)
    if(length(aptline) && !grepl('(libcurl|pandoc)', aptline[1])){
      system(aptline[1])
    }
    # Special case extra libs that we don't have in the base image
    extras <- grep('qgis|librdf0-dev|default-jdk', aptline, value = TRUE)
    lapply(extras, system)
  }, error = function(e){
    message("Problem looking for system requirements: ", e$message)
  })
}

# These are not on CRAN, so we filter them out
basepkgs <- names(which(installed.packages()[ ,"Priority"] == "base", ))

#' @rdname buildtools
#' @export
install_dependencies <- function(path = '.'){
  setwd(path)

  # Try to work around intermittend RSPM fails
  precache_rspm()

  desc <- as.data.frame(read.dcf('DESCRIPTION'))
  message("Running: remotes::local_package_deps(dependencies=TRUE)")
  deps <- remotes::local_package_deps(dependencies=TRUE)

  # Workaround for https://bugs.r-project.org/show_bug.cgi?id=18191
  deps <- setdiff(unique(c(deps, desc$VignetteBuilder)), basepkgs)

  message("Running: utils::install.packages(deps)")
  utils::install.packages(deps)

  # The following should not be needed if the remote is part of the universe
  # However we install it anyway to avoid race conditions if the remote was just added
  remotes <- desc$Remotes
  if(length(remotes)){
    try({
      remotes_repos <- trimws(strsplit(remotes, ',')[[1]])
      lapply(remotes_repos, function(x){remotes::install_github(x, upgrade = FALSE)})
    })
  }

  # Store recursive runtime and checktime dependencies
  harddeps <- remotes::local_package_deps()
  rundeps <- recurse_deps(harddeps)
  cat(sprintf('::set-output name=RUNDEPS::%s\n', base64_gzip(jsonlite::toJSON(as.character(rundeps)))))

  # Not used right now: mostly shows all the testthat/rmarkdown stack stuff
  #checkdeps <- setdiff(recurse_deps(setdiff(deps, rundeps)), rundeps)
  #cat(sprintf('::set-output name=CHECKDEPS::%s\n', base64_gzip(jsonlite::toJSON(as.character(checkdeps)))))

  # Check if Java/JAGS are required
  if(isTRUE('rJava' %in% rundeps)){
    cat('::set-output name=NEED_RJAVA::true\n')
    file.create('/NEED_RJAVA')
  }
  if(isTRUE(any(c('rjags', 'runjags') %in% rundeps))){
    if(!require('rjags'))
      install.packages('rjags')
    cat('::set-output name=NEED_JAGS::true\n')
    file.create('/NEED_JAGS')
  }
}

recurse_deps <- function(pkgs){
  if(!length(pkgs))
    return(character())
  all <- sort(unique(c(pkgs, unlist(unname(tools::package_dependencies(pkgs, recursive = TRUE))))))
  setdiff(all, basepkgs)
}

#' @rdname buildtools
#' @param field which field from the description to show
#' @export
read_description_field <- function(field, path = '.'){
  desc <- tools:::.read_description(file.path(path, 'DESCRIPTION'))
  extra <- tools:::.expand_package_description_db_R_fields(desc)
  as.list(gsub("'", "", trimws(c(desc, extra)), fixed = TRUE))[[field]]
}


#' @rdname buildtools
#' @param field which field from the description to show
#' @export
get_ostype <- function(path = '.'){
  read_description_field('OS_type', path = path)
}

get_schema_keywords <- function(path = '.'){
  keywords <- read_description_field('X-schema.org-keywords', path)
  if(length(keywords)){
    tolower(trimws(strsplit(keywords, ',', fixed = TRUE)[[1]]))
  }
}

#' @rdname buildtools
#' @export
get_maintainer_info <- function(path = '.'){
  # see also tools:::.expand_package_description_db_R_fields
  x <- tools:::.read_description(file.path(path, 'DESCRIPTION'))
  extra <- tools:::.expand_package_description_db_R_fields(x)
  maintainerline <- as.list(gsub("'", "", trimws(c(x, extra)), fixed = TRUE))$Maintainer
  info <- list(
    name = trimws(sub("<(.*)>", '', maintainerline)),
    email =  tolower(trimws(sub("^.*<(.*)>.*$", '\\1', maintainerline)))
  )
  login <- Sys.getenv('MAINTAINERLOGIN', "")
  if(nchar(login))
    info$login <- tolower(login)
  aar <- x["Authors@R"]
  if(is.na(aar)) return(info)
  authors <- utils:::.read_authors_at_R_field(aar)
  maintainer <- Filter(function(x){"cre" %in% x$role}, authors)
  if(!length(maintainer)) return(info)
  orcid <- as.list(maintainer[[1]]$comment)$ORCID
  pattern <- '0000-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]'
  m <- regexpr(pattern, orcid)
  result <- regmatches(orcid, m)
  if(length(result)){
    info$orcid <- result
  }
  return(info)
}

filter_topics <- function(x){
  setdiff(x, c("r", "rstats", "cran", "r-package", "rpackage", "package", "r-stats", "rstats-package"))
}

#' @rdname buildtools
#' @export
get_gitstats <- function(repo, pkgdir, url){
  out <- list(
    updates = weekly_commits(repo = repo),
    tags = latest_tags(repo = repo)
  )
  keywords <- filter_topics(get_schema_keywords(pkgdir))
  if(length(keywords)){
    out$topics <- unique(keywords)
  }
  if(!grepl('^https?://github.com', url)){
    return(out)
  }
  repo <- sub("^https?://github.com/", "", url)
  repo <- sub("/$", "", repo)
  endpoint <- sprintf('/repos/%s/contributors', repo)
  contributors <- gh::gh(endpoint, .limit = 500, .progress = FALSE)
  logins <- vapply(contributors, function(x){x$login}, character(1))
  counts <- vapply(contributors, function(x){x$contributions}, integer(1))
  out$contributions = structure(as.list(counts), names = tolower(logins))
  ghinfo <- gh::gh(sprintf('/repos/%s', repo))
  ghtopics <- filter_topics(unlist(ghinfo$topics))
  if(length(ghtopics))
    out$topics <- unique(c(out$topics, ghtopics))
  if(tolower(ghinfo$owner$login) == tolower(dirname(repo))){
    out$organization <- identical(tolower(ghinfo$owner$type), 'organization')
  } else {
    message(sprintf('% seems transferred to %s!', repo, ghinfo$owner$login))
  }
  if(length(ghinfo$stargazers_count))
    out$stars <- ghinfo$stargazers_count
  return(out)
}

#' @export
#' @rdname buildtools
get_gitstats_base64 <- function(repo, pkgdir, url){
  gitstats <- tryCatch(get_gitstats(repo = repo, pkgdir = pkgdir, url = url), error = function(e){
    message('Failed to get gitstats: ', e$message)
    NULL
  })
  if(length(gitstats)){
    if(length(gitstats$topics))
      gitstats$topics <- I(gitstats$topics)
    json <- jsonlite::toJSON(gitstats, auto_unbox = TRUE)
    base64_gzip(json)
  }
}

#' @export
#' @rdname buildtools
find_readme_url <- function(url, subdir = NULL){
  # Same rules as pkgdown
  candidates <- c("README.md", 'readme.md', 'index.md')
  rawurls <- sprintf("%s/raw/HEAD/%s", url, candidates)
  if(length(subdir) && nchar(subdir)){
    rawurls <- c(sprintf("%s/raw/HEAD/%s", url, file.path(subdir, candidates)), rawurls)
  }
  for(x in rawurls){
    if(url_exists(x)){
      return(x)
    }
  }
}

#' @export
#' @rdname buildtools
render_readme <- function(url, outdir = '.'){
  base <- dirname(url)
  md <- readLines(url)
  html <- commonmark::markdown_html(md, extensions = TRUE)
  doc <- xml2::read_html(html)
  badges_extract(doc)
  for(img in xml2::xml_find_all(doc, '//img')){
    ref <- xml2::xml_attr(img, 'src')
    if(!grepl("https?://", ref)){
      xml2::xml_attr(img, 'src') <- paste0(base, '/', ref)
    }
  }
  for(a in xml2::xml_find_all(doc, '//a')){
    ref <- xml2::xml_attr(a, 'href')
    if(!grepl("https?://", ref) && !grepl('^#', ref)){
      xml2::xml_attr(a, 'href') <- paste0(base, '/', ref)
    }
  }
  body <- xml2::xml_child(doc)
  xml2::xml_name(body) <- 'div'
  xml2::xml_attr(body, 'class') <- 'commonmark-readme-html'
  writeLines(as.character(body), file.path(outdir, 'readme.html'))
  writeLines(md, file.path(outdir, 'readme.md'))
}

#' @export
#' @rdname buildtools
generate_cff <- function(path, outdir){
  outfile <- file.path(normalizePath(outdir, mustWork = TRUE), 'citation.cff')
  setwd(path)
  cffr::cff_write(outfile = outfile, dependencies = FALSE, gh_keywords = FALSE)
}

#' @export
#' @rdname buildtools
maintainer_info_base64 <- function(path = '.'){
  info <- get_maintainer_info(path = path)
  json <- jsonlite::toJSON(info, auto_unbox = TRUE)
  base64_gzip(json)
}

#' @export
#' @rdname buildtools
list_assets <- function(path){
  json <- jsonlite::toJSON(list.files(path))
  cat(sprintf('::set-output name=ASSETS::%s\n', base64_gzip(json)))
}

precache_rspm <- function(){
  url <- getOption('repos')['CRAN']
  for(i in 1:3){
    unlink(list.files(tempdir(), pattern = 'packagemanager.rstudio.com', full.names = TRUE))
    pkgs <- available.packages(repos = url)
    message("Found ", nrow(pkgs), " packages on rspm")
    if(nrow(pkgs) > 18500){
      message("OK")
      break
    } else {
      if(i == 3) stop("Failed to access RSPM repository")
      message("Retrying...")
    }
  }
}
