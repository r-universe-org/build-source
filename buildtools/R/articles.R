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
  message("Replacing default rmarkdown theme...")
  rmd_engine <- tools::vignetteEngine('rmarkdown', package = 'knitr')
  tools::vignetteEngine('rmarkdown', package = 'knitr', tangle = rmd_engine$tangle,
    pattern = rmd_engine$pattern, weave = function(..., output_format = NULL){
      rmd_engine$weave(..., output_format = r_universe_format())
    })

  # For backward compatibility with old vignettes that use legacy knitr::knitr engine
  old_engine <- tools::vignetteEngine('knitr', package = 'knitr')
  tools::vignetteEngine('knitr', package = 'knitr', tangle = old_engine$tangle,
    pattern = old_engine$pattern, weave = function(file, ..., output_format = NULL){
      if (grepl("\\.[Rr]md$", file)){
        rmd_engine$weave(file, ..., output_format = r_universe_format())
      } else {
        old_engine$weave(file, ...)
      }
    })

  # For Hendrik's rsp vignettes
  setHook(packageEvent("R.rsp", "onLoad"), function(...) {
    message("Found R.rsp! Replacing R.rsp markdown theme...")
    rsp_engine <- tools::vignetteEngine('rsp', package='R.rsp')
    tools::vignetteEngine('rsp', package = 'R.rsp', weave = function(file, ..., postprocess = TRUE){
      if(isTRUE(grepl("\\.md\\.rsp$", file))){
        mdfile <- rsp_engine$weave(file, ..., postprocess = FALSE)
        render_article(mdfile)
      } else {
        rsp_engine$weave(file, ..., postprocess = postprocess)
      }
    }, tangle = rsp_engine$tangle, pattern = rsp_engine$pattern)
  })

  # Mainly Dirk's vignettes
  setHook(packageEvent("simplermarkdown", "onLoad"), function(...) {
    message("Found simplermarkdown! Replacing mdweave_to_html theme...")
    old_engine <- tools::vignetteEngine('mdweave_to_html', package='simplermarkdown')
    tools::vignetteEngine('mdweave_to_html', package = 'simplermarkdown', weave = function(file, ...){
      mdfile <- file.path(tempdir(), paste0(tools::file_path_sans_ext(file), '.md'))
      simplermarkdown:::mdweave(file, mdfile, ...)
      htmlfile <- render_article(mdfile)
      file.copy(htmlfile, '.')
    }, tangle = old_engine$tangle, pattern = old_engine$pattern)
  })
}
