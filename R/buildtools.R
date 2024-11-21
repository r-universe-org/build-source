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
find_logo <- function (path, git_url, branch, subdir = "") {
  # Match logic from pkgdown but return path relative packagfe root.
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
  file_to_url(logo_path, git_url, branch, subdir)
}

file_to_url <- function(logo, git_url, branch, subdir){
  git_url <- sub("\\.git$", "", git_url)
  upstream <- paste0(git_url, '/raw/', branch)
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
  df[df$n > 0,]
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
  vignettes <- vignettes[nchar(vignettes$PDF) > 0,] #workaround Sweave bug
  if(nrow(vignettes) > 0){
    df <- vignettes[c('File', 'PDF', 'Title')]
    names(df) <- c("source", "filename", "title")
    inputs <- file.path('vignettes', df$source)
    if(length(subdir) && nchar(subdir)){
      inputs <- file.path(subdir, inputs)
    }
    stats <- gert::git_stat_files(inputs, repo = repo)
    srcfiles <- file.path(gert::git_info(repo)$path, inputs)
    rmddata <- vignettes_metadata(srcfiles)
    df$title <- vapply(seq_along(df$title), function(i){
      rmdtitle <- rmddata[[i]]$title
      if(length(rmdtitle)){
        if(is.list(rmdtitle) && length(rmdtitle$plain)){
          remove_markup(rmdtitle$plain) # see pkg 'forecast' vignettte
        } else {
          remove_markup(rmdtitle[[1]])
        }
      } else {
        remove_markup(df$title[i])
      }
    }, character(1))
    df$author = vapply(rmddata, function(x){remove_markup(normalize_author(x$author))}, character(1))
    df$engine = vignettes_engines(srcfiles)
    df$headings = vignettes_headings(srcfiles)
    df$created = stats$created
    df$modified = stats$modified
    df$commits = stats$commits
    return(df)
  }
}

## Packages with weird stuff in author:
# baRulho: literal html
# phonfieldwork: markdown
# MODIStsp: linebreak
# DataPackageR: email address (invalid html)
# deltareportr: unclosed html (non-xml)
remove_markup <- function(str){
  if(is.na(str))
    return(str)
  tryCatch({
    gsub('\\s+', ' ', xml2::xml_text(xml2::read_html(commonmark::markdown_html(str))))
  }, error = function(e){
    str
  })
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

vignettes_metadata <- function(files){
  lapply(files, function(x){
    tryCatch({
      if(grepl("\\.r?md$", x, ignore.case = TRUE)){
        rmarkdown::yaml_front_matter(x)
      }
    }, error = function(e){
      NULL
    })
  })
}

vignettes_engines <- function(files){
  vapply(files, function(x){
    tryCatch({
      tools:::getVignetteEngine(x)
    }, error = function(e){
      NA_character_
    })
  }, character(1), USE.NAMES = FALSE)
}

vignettes_headings <- function(files){
  lapply(files, function(x){
    if(grepl("\\.r?md$", x, ignore.case = TRUE)){
      tryCatch(markdown_headings(x), error = function(e){
        message(e)
        character()
      })
    } else {
      character()
    }
  })
}

markdown_headings <- function(file){
  body <- rmarkdown:::partition_yaml_front_matter(rmarkdown:::read_utf8(file))$body
  if(length(body)){
    xml <- commonmark::markdown_xml(body)
    doc <- xml2::xml_ns_strip(xml2::read_xml(xml))
    headings <- unique(xml2::xml_text(xml2::xml_find_all(doc, xpath = '//heading')))
    # Also remove pandoc-style class/id annotation
    trimws(sub("\\{.*\\}$", "", headings))
  }
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

# Account for some random GHA network failures
url_exists <- function(url){
  for(i in 1:3){
    try({
      req <- curl::curl_fetch_memory(url)
      return(req$status < 400)
    })
    Sys.sleep(3)
  }
  stop("Failed to connect to: ", url)
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
sysdeps_info <- function(pkg){
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
    return(df)
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

  # Temp workaround for pak hanging in resolver
  # options(repos = c(universe = Sys.getenv("MY_UNIVERSE"), CRAN = 'https://cloud.r-project.org'))

  # Try to install missing sysdeps.
  tryCatch({
    skiplist <- '(libcurl|pandoc|cargo|rustc)'
    sysreqs <- pak::pkg_sysreqs('.', upgrade = FALSE)$packages$system_packages
    syspkgs <- grep(skiplist, unlist(sysreqs), value = TRUE, invert = TRUE)
    if(length(syspkgs)){
      syspkgs <- paste(syspkgs, collapse = ' ')
      message("Installing sysreqs: ", syspkgs)
      system("apt-get update -y")
      system(paste("apt-get install -y", syspkgs))
      con <- file("DESCRIPTION", open = 'a')
      writeLines(paste("Config/pak/sysreqs:", syspkgs), con)
      close(con)
    } else {
      message("No sysreqs needed")
    }
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

  # Sanity check for P3m
  precache_pppm()

  desc <- as.data.frame(read.dcf('DESCRIPTION'))
  deps <- remotes::local_package_deps(dependencies=TRUE)
  vignette_builders <- split_by_comma(desc$VignetteBuilder)

  # Workaround for https://bugs.r-project.org/show_bug.cgi?id=18191
  deps <- as.character(c(deps, vignette_builders))

  # Add Additional_repositories
  if(length(desc$Additional_repositories)){
    addrepos <- trimws(strsplit(desc$Additional_repositories, ",", fixed=TRUE)[[1]])
    addrepos <- grep('^https?://\\S+$', addrepos, value = TRUE)
    addrepos <- grep(Sys.getenv("MY_UNIVERSE"), fixed = TRUE, addrepos, value = TRUE, invert = TRUE)
    message("Using additional_repositories: ", paste(addrepos, collapse = ', '))
    options(repos = c(getOption('repos'), addrepos))
  }

  # Try to download and cache *all* dependencies (also those preinstalled on this image)
  # Note that if deps is NULL, tools::package_dependencies() returns all packages!
  alldeps <- if(length(deps)){
    setdiff(unique(c(deps,unlist(unname(tools::package_dependencies(deps, recursive = TRUE))))), basepkgs)
  }

  # Some sanity check
  if(length(alldeps) > 1000){
    stop("This package has ", length(alldeps), " dependencies. This does not seem right.")
  }

  # Actually install
  message("Running: utils::install.packages(alldeps)")
  utils::install.packages(alldeps)

  # The following should not be needed if the remote is part of the universe
  # However we install it anyway to avoid race conditions if the remote was just added
  remotes <- desc$Remotes
  if(length(remotes)){
    try({
      remotes_repos <- split_by_comma(remotes)
      lapply(remotes_repos, function(x){remotes::install_github(x, upgrade = FALSE)})
    })
  }

  # Store recursive runtime and checktime dependencies
  harddeps <- remotes::local_package_deps()
  rundeps <- as.character(recurse_deps(harddeps))
  saveRDS(rundeps, '/tmp/rundeps.rds')
  #cat(sprintf('::set-output name=RUNDEPS::%s\n', base64_gzip(jsonlite::toJSON(rundeps))))

  # Not used right now: mostly shows all the testthat/rmarkdown stack stuff
  #checkdeps <- setdiff(recurse_deps(setdiff(deps, rundeps)), rundeps)
  #cat(sprintf('::set-output name=CHECKDEPS::%s\n', base64_gzip(jsonlite::toJSON(as.character(checkdeps)))))

  # Check if Java/JAGS are required
  if(isTRUE('rJava' %in% rundeps)){
    file.create('/NEED_RJAVA')
  }
  if(isTRUE(any(c('rjags', 'runjags') %in% c(rundeps, desc$Package)))){
    if(!require('rjags'))
      utils::install.packages('rjags')
    file.create('/NEED_JAGS')
  }
  if(isTRUE('cmdstanr' %in% c(deps, desc$Package))){
    file.create('/NEED_CMDSTAN')
    install_cmdstan_quick()
  }
}

# From: https://mc-stan.org/users/documentation/case-studies/jupyter_colab_notebooks_2020.html#challenge-fast-spin-up
# The typical cmdstanr::install_cmdstan() takes like 10 minutes, but it is too large to preinstall on the images.
install_cmdstan_quick <- function(){
  standir <- normalizePath('~/.cmdstan', mustWork = FALSE)
  dir.create(standir)
  on.exit(unlink('cmdstan.tgz'))
  utils::download.file('https://github.com/stan-dev/cmdstan/releases/download/v2.33.1/collab-cmdstan-2.33.1.tgz', 'cmdstan.tgz')
  system(sprintf("tar zxf cmdstan.tgz -C %s", standir), intern = TRUE)
  message("Done installing cmdstan!")
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
read_description_field <- function(fields, path = '.'){
  desc <- tools:::.read_description(file.path(path, 'DESCRIPTION'))
  extra <- tools:::.expand_package_description_db_R_fields(desc)
  unlist(lapply(fields, function(x){
    as.list(gsub("'", "", trimws(c(desc, extra)), fixed = TRUE))[[x]]
  }))
}


#' @rdname buildtools
#' @param field which field from the description to show
#' @export
get_ostype <- function(path = '.'){
  read_description_field('OS_type', path = path)
}

get_schema_keywords <- function(path = '.'){
  keywords <- read_description_field(c('X-schema.org-keywords', 'biocViews'), path)
  if(length(keywords)){
    trimws(strsplit(keywords, ',', fixed = TRUE)[[1]])
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
  if(nchar(login)){
    info$login <- tolower(login)
    info$mastodon <- scrape_github_mastodon(login)
    info$bluesky <- scrape_github_bluesky(login)
  }
  uuid <- Sys.getenv('MAINTAINERUUID', "")
  if(nchar(uuid)){
    info$uuid <- as.numeric(uuid)
  }
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

scrape_github_mastodon <- function(login){
  tryCatch({
    doc <- xml2::read_html(paste0("http://github.com/", login))
    link <- xml2::xml_find_all(doc, '//li[svg/title = "Mastodon"]/a')
    if(length(link)){
      xml2::xml_attr(link, 'href')
    }
  }, error = message)
}

scrape_github_bluesky <- function(login){
  tryCatch({
    doc <- xml2::read_html(paste0("http://github.com/", login))
    link <- xml2::xml_find_all(doc, '//li[svg/title = "Bluesky"]/a')
    if(length(link)){
      xml2::xml_attr(link, 'href')
    }
  }, error = message)
}

normalize_tags <- function(x){
  x <- unique(tolower(x))
  setdiff(x, c("r", "rstats", "cran", "r-cran", "cran-r", "r-package", "r-packages", "rpackage", "package", "r-stats", "rstats-package"))
}

get_home_url <- function(pkg){
  df <- utils::read.csv('https://r-universe-org.github.io/cran-to-git/crantogit.csv')
  url <- df[df$package == pkg, 'url']
  if(length(url))
    return(url)
}

get_real_owner <- function(pkg){
  df <- utils::read.csv('https://r-universe-org.github.io/cran-to-git/universes.csv')
  owner <- df[df$package == pkg, 'owner']
  if(length(owner))
    return(owner)
}

#' @rdname buildtools
#' @export
get_gitstats <- function(repo, pkgdir, url){
  out <- list(
    updates = weekly_commits(repo = repo),
    tags = latest_tags(repo = repo)
  )
  pkgname <- read_description_field('Package', pkgdir)
  keywords <- normalize_tags(get_schema_keywords(pkgdir))
  biocinfo <- tryCatch(bioc_releases(pkgname), error = message)
  if(length(biocinfo)){
    out$bioc <- biocinfo
  }
  if(length(keywords)){
    out$topics <- keywords
  }
  if(!length(url) || !grepl('^https?://github.com', url)){
    return(out)
  }
  repo <- sub("^https?://github.com/", "", url)
  repo <- sub("/$", "", repo)
  ghinfo <- gh::gh(sprintf('/repos/%s', repo))
  ghtopics <- normalize_tags(unlist(ghinfo$topics))
  if(length(ghtopics))
    out$topics <- unique(c(out$topics, ghtopics))
  if(tolower(ghinfo$owner$login) != tolower(dirname(repo))){
    message(sprintf('% seems transferred to %s!', repo, ghinfo$owner$login))
  }
  if(length(ghinfo$stargazers_count))
    out$stars <- jsonlite::unbox(ghinfo$stargazers_count)
  out$contributions = tryCatch(list_contributions(repo), error = function(e){
    message(e)
    NULL
  })
  return(out)
}

universe_name_fallback <- function(){
  sub("https://(.+)\\.r-universe\\.dev", "\\1", Sys.getenv('MY_UNIVERSE'))
}

universe_info <- function(){
  name <- Sys.getenv('UNIVERSE_NAME', universe_name_fallback())
  universe <- switch(name,
    'bioc' = 'bioconductor',
    'r-multiverse-staging' = 'r-multiverse',
    'ropensci-champions' = 'ropensci',
    name
  )
  gh::gh(sprintf('/users/%s', universe))
}

current_info <- function(package){
  tryCatch({
    url <- paste0(Sys.getenv('MY_UNIVERSE', 'https://cran.r-universe.dev'), '/', package, '/json')
    message("Looking up current package info: ", url)
    jsonlite::fromJSON(url)
  }, error = message)
}

list_contributions <- function(repo){
  endpoint <- sprintf('/repos/%s/contributors', repo)
  contributors <- gh::gh(endpoint, .limit = 100, .progress = FALSE)
  logins <- tolower(vapply(contributors, function(x){x$login}, character(1)))
  counts <- vapply(contributors, function(x){x$contributions}, integer(1))

  # Filter bots and duplicate users (github bug)
  skip <- duplicated(logins) | grepl('[bot]', logins, fixed = TRUE)
  counts <- counts[!skip]
  logins <- logins[!skip]
  structure(as.list(counts), names = logins)
}

#' @export
#' @rdname buildtools
get_gitstats_base64 <- function(repo, pkgdir, url){
  gitstats <- get_gitstats(repo = repo, pkgdir = pkgdir, url = url)
  if(length(gitstats)){
    if(length(gitstats$topics))
      gitstats$topics <- I(gitstats$topics)
    json <- jsonlite::toJSON(gitstats, auto_unbox = TRUE)
    base64_gzip(json)
  }
}

#' @export
#' @rdname buildtools
find_readme_url <- function(url, branch = 'HEAD', subdir = NULL){
  # Same rules as pkgdown
  candidates <- c("README.md", 'readme.md', 'index.md', '.github/README.md', 'docs/README.md')
  rawurls <- sprintf("%s/raw/%s/%s", url, branch, candidates)
  if(length(subdir) && nchar(subdir)){
    rawurls <- c(sprintf("%s/raw/%s/%s", url, branch, file.path(subdir, candidates)), rawurls)
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
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  writeLines(as.character(body), file.path(outdir, 'readme.html'))
  writeLines(md, file.path(outdir, 'readme.md'))
}


#' @export
#' @rdname buildtools
render_news_files <- function(package, outdir = '.', url = NULL){
  extra_dir <- file.path(normalizePath(outdir, mustWork = TRUE), 'extra')
  dir.create(extra_dir, showWarnings = FALSE)
  news <- tools:::.build_news_db(package)
  if(length(news)){
    tryCatch({
      # Try to add CRAN release dates (similar to pkgdown)
      # NB: dates in HTML are only shown starting R-4.3 (pr@82796)
      dateurl <- paste0("https://crandb.r-pkg.org/", package, "/all")
      pkgdata <- jsonlite::fromJSON(dateurl)
      stopifnot(identical(package, pkgdata$name))
      timeline <- pkgdata$timeline
      news$Date <- vapply(news$Version, function(ver){
        x <- timeline[[ver]]
        ifelse(length(x), as.character(as.Date(x)), NA_character_)
      }, character(1))
    }, error = function(e){
      message("Did not find CRAN timeline data for package: ", package)
    })
    txt <- paste(unlist(format(news)), collapse = "\n\n")
    html <- tools::toHTML(news, title = 'NEWS', logo = FALSE, up = NULL, top = NULL,
                          css = 'https://r-universe.dev/static/news-styles.css')
    html <- gsub('<h2>Changes in version', paste0('<h2>', package), html, fixed = TRUE)
    html <- gsub("(\\([-0-9]+\\))</h2>", '<span class="crandate">\\1</span></h2>', html)
    if(length(url)){
      html <- repo_auto_link(html, url)
    }
    writeLines(txt, file.path(extra_dir, 'NEWS.txt'))
    writeLines(html, file.path(extra_dir, 'NEWS.html'))
    cat(list.files(extra_dir, full.names = TRUE), sep = '\n')
  } else {
    message("No NEWS found")
  }
}

#' @export
#' @rdname buildtools
generate_citation_files <- function(path, outdir, git_url){
  extra_dir <- file.path(normalizePath(outdir, mustWork = TRUE), 'extra')
  dir.create(extra_dir, showWarnings = FALSE)
  citation_cff <- file.path(extra_dir, 'citation.cff')
  citation_json <- file.path(extra_dir, 'citation.json')
  citation_txt <- file.path(extra_dir, 'citation.txt')
  citation_html <- file.path(extra_dir, 'citation.html')
  setwd(path)
  cffr::cff_write(outfile = citation_cff, dependencies = FALSE, gh_keywords = FALSE)
  ct <- utils::citation(basename(outdir))
  if(!file.exists('inst/CITATION')){
    ct <- fixup_citation_url(ct, git_url)
  }
  jsonlite::write_json(ct, citation_json, force=TRUE, auto_unbox = TRUE, pretty = TRUE)
  writeLines(utils::capture.output(print(ct, bibtex = TRUE)), citation_txt)
  writeLines(tools::toHTML(ct), citation_html)
}

fixup_citation_url <- function(ct, git_url){
  if(grepl("https://github.com/bioc/", git_url)){
    pkghome <- sub("https://github.com/bioc/", "https://bioconductor.org/packages/", git_url, fixed = TRUE)
  } else if(grepl("https://github.com/cran/", git_url)){
    pkghome <- sub("https://github.com/cran/", "https://CRAN.R-project.org/package=", git_url, fixed = TRUE)
  } else if(grepl("https://github.com/r-forge/", git_url)){
    pkghome <- sub("https://github.com/r-forge/", "https://r-forge.r-project.org/projects/", git_url, fixed = TRUE)
  } else {
    pkg <- attr(ct, 'package')
    pkghome <- guess_development_url(pkg, git_url)
    if(!length(pkghome)){
      pkghome <- git_url
    }
  }
  out <- unclass(ct)
  out[[1]]$note <- sub("http.*", "", out[[1]]$note)
  out[[1]]$url <- pkghome
  structure(out, class = class(ct))
}

#' @export
#' @rdname buildtools
get_help_metadata <- function(package){
  path <- system.file(package = package)
  out <- readRDS(file.path(path, "help", "aliases.rds"))
  aliases <- split(names(out), unname(out))
  db <- load_rd_data(package)
  titles <- vapply(db, tools:::.Rd_get_title, character(1), USE.NAMES = FALSE)
  df <- data.frame(page = names(db), title = titles)
  df$topics <- lapply(unname(aliases[names(db)]), as.character)
  df
}

load_rd_data <- function(package){
  db <- tools::Rd_db(package)
  names(db) <- tools::file_path_sans_ext(names(db))
  mannames <- vapply(db, tools:::.Rd_get_name, character(1), USE.NAMES = FALSE)
  sortnames <- sub("^(.*-package)$", '___\\1', mannames)
  Filter(function(x){
    is.na(match("internal", get_rd_keywords(x)))
  }, db[order(sortnames)])
}

get_rd_keywords <- function(rd){
  # Mimic: tools:::.Rd_get_metadata
  keywords <- Filter(function(x){identical("\\keyword", attr(x, 'Rd_tag'))}, rd)
  unique(trimws(vapply(keywords, paste, "", collapse = "\n")))
}

#' @export
#' @rdname buildtools
get_package_datasets <- function(package){
  datafiles <- list.files(system.file('data', package = package))
  datasets <- as.data.frame(utils::data(package=package)$results[,c("Item", "Title"), drop = FALSE])
  if(nrow(datasets) > 0){
    names(datasets) <- c('name', 'title')
    datasets$object <- sub("^.* \\((.+)\\)$", "\\1", datasets$name) #see e.g. pkg 'hardhat'
    datasets$name <- sub("\\s.*$", "", datasets$name)
    datalist <- lapply(seq_len(nrow(datasets)), function(i){
      tryCatch({
        dataset <- datasets$object[i]
        dataname <- datasets$name[i]
        env <- new.env()
        utils::data(list = dataset, package = package, envir = env)
        return(env[[dataname]])
      }, error = function(err){
        message(sprintf('Failure loading dataset "%s" (%s)', dataset, err))
      })
    })
    datasets$file <- vapply(datasets$object, function(nm){
      filename <- grep(sprintf('^%s\\.', nm), datafiles, value = TRUE)
      ifelse(length(filename), filename[1], NA_character_)
    }, character(1))
    datasets$class <- lapply(datalist, function(x){if(!is.null(x)) class(x)})
    datasets$fields <- lapply(datalist, function(x){if(is.data.frame(x) || is.matrix(x)) colnames(x) else list()})
    datasets$rows <- vapply(datalist, function(x){ifelse(is.data.frame(x) || is.matrix(x), nrow(x), NA_integer_)}, integer(1))
    datasets$table <- vapply(datalist, function(x){tryCatch({data.table::fwrite(x, tempfile()); TRUE}, error = function(e){FALSE})}, logical(1))
    datasets$tojson <- vapply(datalist, function(x){tryCatch({jsonlite::toJSON(x); TRUE}, error = function(e){FALSE})}, logical(1))
    return(datasets)
  }
}

cranlogs_monthly_downloads <- function(pkg){
  cranlogs <- sprintf('https://cranlogs.r-pkg.org/downloads/total/last-month/%s', pkg)
  tryCatch(list(
    count = jsonlite::fromJSON(cranlogs)$downloads,
    source = cranlogs
  ), error = message)
}

bioc_monthly_downloads <- function(pkg){
  url <- sprintf('https://www.bioconductor.org/packages/stats/bioc/%s/%s_stats.tab', pkg, pkg)
  tryCatch({
    df <- read.table(url, header = TRUE)
    df <- df[df$Month != 'all' & df$Nb_of_downloads > 0,]
    list(
      count = round(median(head(df$Nb_of_downloads, 12))),
      source = dirname(url)
    )
  }, error = message)
}

cran_mentions_count <- function(pkg, project = 'cran'){
  tryCatch({
    req <- curl::curl_fetch_memory(sprintf('https://papers.ecosyste.ms/api/v1/projects/%s/%s', project, pkg))
    if(req$status == 200){
      jsonlite::fromJSON(rawToChar(req$content))$mentions_count
    }
  }, error = message)
}

#' @export
#' @rdname buildtools
generate_metadata_files <- function(package, repo, subdir, outdir, pkgdir, git_url, branch){
  extra_dir <- file.path(normalizePath(outdir, mustWork = TRUE), 'extra')
  dir.create(extra_dir, showWarnings = FALSE)
  exports <- sort(grep('^\\.__', getNamespaceExports(package), invert = TRUE, value = TRUE))
  datasets <- get_package_datasets(package = package)
  vignettes <- vignettes_info(repo = repo, pkg = package, subdir = subdir)
  sysdeps <- sysdeps_info(pkg = package)
  assets <- sort(c('extra/contents.json', list.files(outdir, recursive = TRUE, all.files = TRUE)))
  rundeps <- readRDS('/tmp/rundeps.rds') # never NULL
  readme_url <- Sys.getenv('README_URL')
  readme <- if(nchar(readme_url) > 0) readme_url
  logo <- find_logo(path = pkgdir, git_url = git_url, branch = branch, subdir = subdir)
  homeurl <- get_home_url(package)
  realowner <- get_real_owner(package)
  cranurl <- identical(tolower(git_url), homeurl)
  releases <- get_cran_releases(package)
  helppages <- get_help_metadata(package)
  pkgdown_url <- find_pkgdown_url(package)
  if(grepl("github.com/bioc/", tolower(git_url), fixed = TRUE)){
    downloads <- bioc_monthly_downloads(package)
    mentions <- cran_mentions_count(package, 'bioconductor')
    dev_url <- guess_development_url(package, tolower(git_url), validate = TRUE)
    contents <- get_gitstats(repo, pkgdir, dev_url)
  } else {
    downloads <- cranlogs_monthly_downloads(package)
    mentions <- cran_mentions_count(package, 'cran')
    dev_url <- guess_development_url(package, tolower(git_url))
    contents <- get_gitstats(repo, pkgdir, git_url)
  }
  current <- current_info(package)
  searchresults <- get_blackbird_count(package)
  if(!length(searchresults)){
    searchresults <- current[['_searchresults']]
  }
  userinfo <- universe_info()
  userbio <- list(
    uuid = userinfo$id,
    type = tolower(userinfo$type),
    name = ifelse(length(userinfo$name), userinfo$name, userinfo$login)
  )
  if(length(userinfo$bio)){
    userbio$description <- userinfo$bio
  }
  contents$userbio <- lapply(userbio, jsonlite::unbox)
  if(length(downloads))
    contents$downloads <- lapply(downloads, jsonlite::unbox)
  if(length(mentions))
    contents$mentions <- jsonlite::unbox(mentions)
  if(length(dev_url))
    contents$devurl <- jsonlite::unbox(dev_url)
  if(length(pkgdown_url))
    contents$pkgdown <- jsonlite::unbox(pkgdown_url)
  if(length(searchresults))
    contents$searchresults <- jsonlite::unbox(searchresults)

  # Generate contents.json
  if(length(contents$contributions)){
    contents$contributions <- lapply(contents$contributions, jsonlite::unbox)
  }
  if(file.exists('/NEED_FORTRAN')){
    contents$fortran <- jsonlite::unbox(TRUE)
  }
  if(file.exists('/NEED_CARGO')){
    contents$cargo <- jsonlite::unbox(TRUE)
  }
  if(file.exists('/NEED_GOLANG')){
    contents$golang <- jsonlite::unbox(TRUE)
  }
  contents$assets <- assets
  contents$homeurl <- jsonlite::unbox(homeurl)
  contents$realowner <- jsonlite::unbox(realowner)
  contents$cranurl <- jsonlite::unbox(cranurl)
  contents$releases <- releases
  contents$exports <- exports
  contents$datasets <- datasets
  contents$help <- helppages
  contents$pkglogo <- jsonlite::unbox(logo)
  contents$readme <- jsonlite::unbox(readme)
  contents$rundeps <- rundeps
  contents$sysdeps <- sysdeps
  contents$vignettes <- vignettes
  jsonlite::write_json(Filter(function(x){!is.null(x)}, contents), path = file.path(extra_dir, 'contents.json'))
}

#' @export
#' @rdname buildtools
needs_compilation <- function(package){
  desc <- as.data.frame(read.dcf(system.file('DESCRIPTION', package = package)))
  desc$NeedsCompilation
}

#' @export
#' @rdname buildtools
maintainer_info_base64 <- function(path = '.'){
  info <- get_maintainer_info(path = path)
  json <- jsonlite::toJSON(info, auto_unbox = TRUE)
  base64_gzip(json)
}

# This is a hack, it relies on the 'manual-page' and <a><h2></a> structure that we generate earlier
get_help_titles_from_manual <- function(path){
  if(!file.exists(path))
    stop("Failed to find HTML manual needed to extract titles")
  doc <- xml2::read_html(path)
  containers <- xml2::xml_find_all(doc, "//div[@class='container manual-page']")
  pages <- xml2::xml_attr(containers, 'id')
  titles <- xml2::xml_text(xml2::xml_find_first(containers, 'a/h2'))
  structure(as.list(titles), names = pages)
}

precache_pppm <- function(){
  url <- getOption('repos')['CRAN']
  for(i in 1:3){
    unlink(list.files(tempdir(), pattern = 'p3m.dev', full.names = TRUE))
    pkgs <- utils::available.packages(repos = url)
    message("Found ", nrow(pkgs), " packages on p3m.dev")
    if(nrow(pkgs) > 20000){
      message("OK")
      break
    } else {
      if(i == 3) stop("Failed to access p3m. Maybe see: https://status.posit.co/#past-incidents")
      message("Retrying...")
    }
  }
}

bioc_releases <- function(package){
  yml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  bioc_devel <- jsonlite::read_json(sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', yml$devel_version))
  pkg_devel <- bioc_devel[[package]]
  if(length(pkg_devel)){
    bioc_branch <- 'devel'
    bioc_version <- yml$devel_version
    bioc_pkgver <- pkg_devel$Version
    bioc_release <- jsonlite::read_json(sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', yml$release_version))
    pkg_release <- bioc_release[[package]]
    if(length(pkg_release)){
      bioc_branch <- c(bioc_branch, 'release')
      bioc_version <- c(bioc_version, yml$release_version)
      bioc_pkgver <- c(bioc_pkgver,  pkg_release$Version)
    }
    data.frame(branch = bioc_branch, version = bioc_pkgver, bioc = bioc_version)
  }
}

get_cran_releases <- function(package){
  tryCatch(get_cran_releases_fast(package), error = function(e){
    message(e)
    get_cran_releases_slow(package)
  })
}

get_cran_releases_fast <- function(package){
  req <- curl::curl_fetch_memory(paste0("https://crandb.r-pkg.org/", package, "/all"))
  if(req$status_code == 404){
    return(NULL) # probably not a cran package
  } else if(req$status_code != 200){
    stop("Error reaching crandb")
  }
  pkgdata <- jsonlite::fromJSON(rawToChar(req$content))
  stopifnot(identical(package, pkgdata$name))
  versions <- names(pkgdata$versions)
  dates <- as.POSIXct(vapply(pkgdata$versions, function(x){x$crandb_file_date}, character(1), USE.NAMES = FALSE))
  data.frame(
    version = versions,
    date = as.Date(dates)
  )
}

get_cran_releases_slow <- function(package){
  current <- tools:::CRAN_current_db()
  matches <- which(grepl(sprintf('^%s_.*\\.tar\\.gz', package), row.names(current)))
  filenames <- row.names(current)[matches]
  mtimes <- current[matches, 'mtime']
  archive <- tools:::CRAN_archive_db()
  oldies <- archive[[package]]
  if(length(oldies)){
    filenames <- c(row.names(oldies), filenames)
    mtimes <- c(oldies$mtime, mtimes)
  }
  data.frame(
    version = sub("^.*_", "", sub('.tar.gz', '', filenames, fixed = TRUE)),
    date = as.Date(mtimes)
  )
}

split_by_comma <- function(x){
  if(!length(x) || !is.character(x))
    return(x)
  trimws(strsplit(x, ',')[[1]])
}

# Keep whitespace within R blocks to account for comment lines
# For example: https://github.com/cran/blogdown/blob/1.18/DESCRIPTION
normalize_description <- function(path){
  x <-read.dcf(path, keep.white='Authors@R')
  write.dcf(x, path, keep.white='Authors@R')
}

find_pkgdown_url <- function(package){
  tryCatch(dirname(as.character(downlit:::remote_metadata(package)$urls$reference)), error = function(...){})
}
