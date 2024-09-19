guess_development_url <- function(package, git_url, validate = FALSE){
  desc <- as.data.frame(read.dcf(system.file('DESCRIPTION', package = package)))
  input <- paste(desc$BugReports, desc$DevelopmentURL, desc$URL, git_url)
  input <- normalize_github_urls(input)
  input <- paste(input, replace_rforge_urls(input)) #Prefer GitHub URL over r-forge guess
  input <- gsub('https://github.com/(cran|bioc)/', '', input) # No mirror URLS here
  pattern <- 'https?://(github.com|gitlab.com|bitbucket.org)/[A-Za-z0-9_-]+/[A-Za-z0-9_.-]+'
  m <- regexpr(pattern, input, ignore.case = TRUE)
  urls <- regmatches(input, m)
  dev_url <- sub("\\.git$", "", sub("^http://", "https://", tolower(urls)))
  if(length(dev_url) && isTRUE(validate) && curl::curl_fetch_memory(dev_url)$status == 404){
    warning("URL does not exist: ", dev_url)
    return(character())
  }
  return(dev_url)
}

replace_rforge_urls <- function(input){
  input <- gsub("www.rforge.net", "github.com/s-u", input, fixed = TRUE)
  input <- gsub('r-forge\\.r-project\\.org/projects/', 'github.com/r-forge/', input, ignore.case = TRUE)
  input <- gsub('r-forge\\.r-project\\.org/scm/[^ ]*\\?root=(\\S*)', 'github.com/r-forge/\\1', input, ignore.case = TRUE)
  input <- gsub("https?://lists\\.r-forge\\.r-project\\.org", "", input, ignore.case = TRUE) # Remove URLS to mailing list
  gsub("https?://([A-Za-z0-9_.-]+)\\.r-forge\\.r-project\\.org", 'https://github.com/r-forge/\\1', input, ignore.case = TRUE)
}

normalize_github_urls <- function(input){
  input <- gsub('//www.github.com', '//github.com', input, fixed = TRUE)
  input <- gsub("https?://([A-Za-z0-9-]+)\\.github\\.io/([A-Za-z0-9_.-]+)", 'https://github.com/\\1/\\2', input)
  sub("https://([A-Za-z0-9-]+)\\.r-universe.dev/([A-Za-z0-9_.-]+)", 'https://github.com/\\1/\\2', input)
}
