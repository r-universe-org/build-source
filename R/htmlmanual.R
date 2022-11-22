#' Generate HTML reference manual
#'
#' Renders HTML version of the full package manual
#'
#' @rdname html_manual
#' @param package name of the package
#' @param outdir where to put the html file
#' @export
render_html_manual <- function(package, outdir = '.'){
  message("Rendering HTML reference manual for package: ", package)
  postdoc::render_package_manual(package = package, outdir = outdir)
}
