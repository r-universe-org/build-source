#' Render rmarkdown article
#'
#' @export
#' @param input path to Rmd file
render_article <- function(input){
  rmarkdown::render(input, rmarkdown::html_document(
    toc = TRUE,
    toc_depth = 2,
    theme = NULL,
    mathjax = NULL,
    highlight = 'pygments',
    template = template_file('template.html'),
    includes = rmarkdown::includes(
      in_header = template_file('header.html'),
      after_body = template_file('footer.html')
    )
  ))
}

template_file <- function(path){
  normalizePath(system.file(paste0('rmd-template/', path), package = 'buildtools'), mustWork = TRUE)
}
