.onLoad <- function(libname, pkgname) {
  # Workaround for https://github.com/globalgov/messydates/issues/92
  options(gh_cache = FALSE)
}
