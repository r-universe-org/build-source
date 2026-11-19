local_description <- function(lines){
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("Package: testpkg", "Version: 1.0.0", lines), file.path(dir, "DESCRIPTION"))
  dir
}

with_no_maintainer_env <- function(code){
  old <- Sys.getenv(c("MAINTAINERLOGIN", "MAINTAINERUUID"), unset = NA)
  Sys.unsetenv(c("MAINTAINERLOGIN", "MAINTAINERUUID"))
  on.exit({
    for(nm in names(old)) if(!is.na(old[[nm]])) do.call(Sys.setenv, as.list(old[nm]))
  })
  force(code)
}

authors_r_lines <- c(
  "Authors@R: c(",
  "    person(\"Jeroen\", \"Ooms\", , \"JeroenOoms@gmail.com\", role = c(\"aut\", \"cre\", \"cph\"),",
  "      comment = c(ORCID = \"0000-0002-4035-0289\")),",
  "    person(\"Duncan\", \"Temple Lang\", role = c(\"aut\", \"cph\"),",
  "      comment = c(\"Original author of RJSONIO\")),",
  "    person(\"rOpenSci\", role = \"fnd\")",
  "  )"
)
