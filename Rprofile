# Use RSPM for CRAN
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
options(repos = c(CRAN = "https://packagemanager.rstudio.com/all/__linux__/focal/latest"))

# Enable BioConductor repo
utils::setRepositories(ind = 1:2)

# Enable dev repo
options(repos = c(rOpenSci = "https://dev.ropensci.org", getOption("repos")))

# Other
options(crayon.enabled = TRUE)
