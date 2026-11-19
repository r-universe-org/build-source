local_description <- function(lines){
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("Package: testpkg", "Version: 1.0.0", lines), file.path(dir, "DESCRIPTION"))
  dir
}
