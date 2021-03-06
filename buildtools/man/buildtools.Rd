% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildtools.R
\name{find_logo}
\alias{find_logo}
\alias{package_sysdeps_string}
\alias{vignettes_base64}
\alias{install_dependencies}
\title{Build tools}
\usage{
find_logo(path, git_url, subdir = "")

vignettes_base64(repo, pkg = basename(repo), subdir = "")

install_dependencies(path = ".")
}
\arguments{
\item{path}{root directory of package}

\item{git_url}{of the git repository}

\item{subdir}{path within the git repo where the pkg is}

\item{repo}{path to the git repository}

\item{pkg}{name of the installed package}
}
\description{
Get some extra info about packages.
}
