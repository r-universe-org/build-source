#' Render rmarkdown article
#'
#' @export
#' @rdname articles
#' @param input path to Rmd file
#' @param ... passed to render
render_article <- function(input, ...){
  rmarkdown::render(input, output_format = r_universe_format(), ...)
}

r_universe_format <- function(){
  template_file <- function(path){
    normalizePath(system.file(paste0('rmd-template/', path),
                              package = 'buildtools'), mustWork = TRUE)
  }
  rmarkdown::html_document(
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
  )
}

#  Hack to replace knitr::rmarkdown engine
#' @export
#' @rdname articles
replace_rmarkdown_engine <- function(){
  message("Replacing rmarkdown engine...")
  default_engine <- tools::vignetteEngine('rmarkdown', package = 'knitr')
  tools::vignetteEngine('rmarkdown', package = 'knitr', tangle = default_engine$tangle,
    pattern = default_engine$pattern, weave = function(..., output_format = NULL){
      default_engine$weave(..., output_format = r_universe_format())
    })
}
